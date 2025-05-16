use std::{cell::RefCell, collections::HashSet, rc::Rc};

use super::{
    CompiledFunction, Scope, Value, builtins,
    context::{AllItems, CompilerContext},
    mangle_type,
    mangler::MangledIdentifier,
    rc::{self, RcValue},
};
use crate::{identifier::Identifier, types};
use inkwell::{
    AddressSpace,
    module::{Linkage, Module},
    values::FunctionValue,
};

impl types::functions::Function {
    const fn linkage(&self) -> Linkage {
        match self.body {
            types::functions::FunctionBody::Statements(_) => match self.visibility {
                types::Visibility::Export => Linkage::External,
                types::Visibility::Internal => Linkage::Internal,
            },
            types::functions::FunctionBody::Extern(_) => Linkage::External,
        }
    }
}

pub struct CompiledModule<'ctx> {
    pub(super) llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
    // TODO get rid of the refcell
    functions: RefCell<HashSet<types::store::TypeId>>,
}

impl std::fmt::Debug for CompiledModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f, 0)
    }
}

impl<'ctx> CompiledModule<'ctx> {
    pub fn new(scope: Rc<Scope<'ctx>>, llvm_module: Module<'ctx>) -> Self {
        Self {
            llvm_module,
            scope,
            functions: RefCell::new(HashSet::new()),
        }
    }

    pub fn to_ir(&self) -> String {
        self.llvm_module
            .print_to_string()
            .to_string()
            .replace("\\n", "\n")
    }

    fn declare_function_inner(
        &self,
        name: &MangledIdentifier,
        arguments: &[types::store::TypeId],
        return_type: &types::Type,
        linkage: Linkage,
        context: &CompilerContext<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> FunctionValue<'ctx> {
        let function_type = context.make_function_type(arguments, return_type, types);

        self.llvm_module
            .add_function(name.as_str(), function_type, Some(linkage))
    }

    pub(crate) fn declare_function(
        &self,
        linkage: Linkage,
        name: &MangledIdentifier,
        arguments: &[types::store::TypeId],
        return_type: &types::Type,
        context: &CompilerContext<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(name, arguments, return_type, linkage, context, types)
    }

    pub(crate) fn get_variable(&self, name: Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        function: &types::functions::Function,
        context: &CompilerContext<'ctx>,
        types: &mut dyn types::store::TypeStore,
        items: &mut AllItems<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        self.functions.borrow_mut().insert(function.type_id);

        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = self.get_or_create_function(function, context, types);

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        context.builder.position_at_end(entry_block);

        for (argument, argument_value) in function.arguments.iter().zip(llvm_function.get_params())
        {
            // TODO get rid of the clone here!
            let value = match types.get(argument.type_id).kind().clone() {
                types::TypeKind::Unit => todo!(),
                types::TypeKind::Object(instantiated_struct_id) => {
                    let struct_instance_type = items
                        .get_or_instantiate_struct(&instantiated_struct_id.clone(), types)
                        .unwrap()
                        .definition
                        .instance_type();

                    let rc = RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        struct_instance_type,
                    );
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                types::TypeKind::Array(element_type_id) => {
                    Value::Reference(builtins::array::ArrayValue::build_instance(
                        element_type_id,
                        context,
                        items,
                        types,
                        // TODO we need the actual vtable here!
                        context
                            .llvm_context
                            .ptr_type(AddressSpace::default())
                            .const_null(),
                    ))
                }
                types::TypeKind::Callable { .. } => todo!(),
                types::TypeKind::U64 => Value::Primitive(
                    types::structs::InstantiatedStructId::new(
                        CompilerContext::get_std_type("u64"),
                        types::generics::TypeArguments::new_empty(),
                    ),
                    argument_value,
                ),
                types::TypeKind::Pointer(_) => todo!(),
                types::TypeKind::U8 => todo!(),
                types::TypeKind::Struct(_) => todo!(),
                types::TypeKind::IndirectCallable(_, _) => todo!(),
                types::TypeKind::InterfaceObject { .. } => todo!(),
                types::TypeKind::Generic(_) => todo!(),
                types::TypeKind::Interface(_) => todo!(),
            };

            scope.set_value(argument.name, value);
        }

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc::build_prologue(&rcs, context, types);

        CompiledFunction {
            llvm_function,
            scope,
            entry: entry_block,
            end: end_block,
            rcs,
            return_value: None,
        }
    }

    pub(crate) fn finalize(self) -> Module<'ctx> {
        self.llvm_module.verify().unwrap();

        self.llvm_module
    }

    pub(crate) fn has_function(&self, type_id: types::store::TypeId) -> bool {
        self.functions.borrow().contains(&type_id)
    }

    pub(crate) fn get_or_create_function(
        &self,
        handle: &types::functions::Function,
        context: &CompilerContext<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> FunctionValue<'ctx> {
        let mangled_name = match &handle.body {
            types::functions::FunctionBody::Extern(identifier) => {
                MangledIdentifier::new_raw(*identifier)
            }
            types::functions::FunctionBody::Statements(_) => mangle_type(types.get(handle.type_id)),
        };

        self.llvm_module
            .get_function(mangled_name.as_str())
            .unwrap_or_else(|| {
                self.declare_function(
                    handle.linkage(),
                    &mangled_name,
                    &handle
                        .arguments
                        .iter()
                        .map(|x| x.type_id)
                        .collect::<Vec<_>>(),
                    types.get(handle.return_type),
                    context,
                    types,
                )
            })
    }

    pub(crate) fn debug_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        nesting_level: usize,
    ) -> std::fmt::Result {
        self.scope.debug_print(f, nesting_level)
    }
}
