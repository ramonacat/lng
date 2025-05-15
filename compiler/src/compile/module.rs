use std::{collections::HashSet, rc::Rc};

use super::{
    CompiledFunction, Scope, Value, builtins,
    context::{AllItems, CompilerContext},
    mangle_type,
    mangler::MangledIdentifier,
    rc::{self, RcValue},
};
use crate::{identifier::Identifier, types};
use inkwell::{
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
    llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
    functions: HashSet<types::InstantiatedType>,
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
            functions: HashSet::new(),
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
        arguments: &[types::functions::Argument],
        return_type: &types::InstantiatedType,
        linkage: Linkage,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let function_type = context.make_function_type(arguments, return_type);

        self.llvm_module
            .add_function(name.as_str(), function_type, Some(linkage))
    }

    pub(crate) fn declare_function(
        &self,
        linkage: Linkage,
        name: &MangledIdentifier,
        arguments: &[types::functions::Argument],
        return_type: &types::InstantiatedType,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(name, arguments, return_type, linkage, context)
    }

    pub(crate) fn get_variable(&self, name: Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &mut self,
        function: &types::functions::Function,
        context: &CompilerContext<'ctx>,
        structs: &mut AllItems<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        self.functions.insert(function.type_.clone());

        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = self.get_or_create_function(function, context);

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        context.builder.position_at_end(entry_block);

        for (argument, argument_value) in function.arguments.iter().zip(llvm_function.get_params())
        {
            let value = match argument.type_.kind() {
                types::InstantiatedTypeKind::Unit => todo!(),
                types::InstantiatedTypeKind::Object(instantiated_struct_id) => {
                    let rc = RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        structs
                            .get_or_instantiate_struct(instantiated_struct_id)
                            .unwrap()
                            .definition
                            .instance_type(),
                    );
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                types::InstantiatedTypeKind::Array { element_type: a } => Value::Reference(
                    builtins::array::ArrayValue::build_instance(a.as_ref(), context, structs),
                ),
                types::InstantiatedTypeKind::Callable { .. } => todo!(),
                types::InstantiatedTypeKind::U64 => Value::Primitive(
                    types::structs::InstantiatedStructId::new(
                        CompilerContext::get_std_type("u64"),
                        types::generics::TypeArgumentValues::new_empty(),
                    ),
                    argument_value,
                ),
                types::InstantiatedTypeKind::Pointer(_) => todo!(),
                types::InstantiatedTypeKind::U8 => todo!(),
                types::InstantiatedTypeKind::Struct(_) => todo!(),
                types::InstantiatedTypeKind::Function(_) => todo!(),
                types::InstantiatedTypeKind::IndirectCallable(_, _) => todo!(),
                types::InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
                types::InstantiatedTypeKind::Generic(_) => todo!(),
                types::InstantiatedTypeKind::Interface(_) => todo!(),
            };

            scope.set_value(argument.name, value);
        }

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc::build_prologue(&rcs, context);

        CompiledFunction {
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

    pub(crate) fn has_function(&self, type_: &types::InstantiatedType) -> bool {
        self.functions.contains(type_)
    }

    pub(crate) fn get_or_create_function(
        &self,
        handle: &types::functions::Function,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let mangled_name = match &handle.body {
            types::functions::FunctionBody::Extern(identifier) => {
                MangledIdentifier::new_raw(*identifier)
            }
            types::functions::FunctionBody::Statements(_) => mangle_type(&handle.type_),
        };

        self.llvm_module
            .get_function(mangled_name.as_str())
            .unwrap_or_else(|| {
                self.declare_function(
                    handle.linkage(),
                    &mangled_name,
                    &handle.arguments,
                    &handle.return_type,
                    context,
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
