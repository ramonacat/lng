use std::rc::Rc;

use super::{
    CompiledFunction, Scope, Value, builtins,
    context::CompilerContext,
    rc::{self, RcValue},
};
use crate::{
    identifier::Identifier,
    name_mangler::{MangledIdentifier, nomangle_identifier},
    types::{self, InstantiatedType},
};
use inkwell::{
    module::{Linkage, Module},
    values::FunctionValue,
};

impl types::functions::Function<InstantiatedType> {
    const fn linkage(&self) -> Linkage {
        match self.body {
            types::functions::FunctionBody::Statements(_) => match self.visibility {
                types::Visibility::Export => Linkage::External,
                types::Visibility::Internal => Linkage::Internal,
            },
            types::functions::FunctionBody::Extern(_) => Linkage::External,
        }
    }

    pub(super) fn mangled_id(&self) -> MangledIdentifier {
        match self.body {
            types::functions::FunctionBody::Extern(identifier) => nomangle_identifier(identifier),
            types::functions::FunctionBody::Statements(_) => self.id.into_mangled(),
        }
    }
}

pub struct CompiledModule<'ctx> {
    llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
}

impl std::fmt::Debug for CompiledModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f, 0)
    }
}

impl<'ctx> CompiledModule<'ctx> {
    pub const fn new(scope: Rc<Scope<'ctx>>, llvm_module: Module<'ctx>) -> Self {
        Self { llvm_module, scope }
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
        arguments: &[types::functions::Argument<InstantiatedType>],
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
        arguments: &[types::functions::Argument<InstantiatedType>],
        return_type: &types::InstantiatedType,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(name, arguments, return_type, linkage, context)
    }

    pub(crate) fn set_variable(&self, name: Identifier, value: Value<'ctx>) {
        self.scope.set_value(name, value);
    }

    pub(crate) fn get_variable(&self, name: Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        handle: &types::functions::Function<InstantiatedType>,
        context: &CompilerContext<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = self.get_or_create_function(handle, context);

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        context.builder.position_at_end(entry_block);

        for (argument, argument_value) in handle.arguments.iter().zip(llvm_function.get_params()) {
            let value = match &argument.type_.kind() {
                types::InstantiatedTypeKind::Unit => todo!(),
                types::InstantiatedTypeKind::Object {
                    type_name: id,
                    type_argument_values,
                } => {
                    let rc = RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        context
                            .global_scope
                            .structs
                            .inspect_instantiated(&(*id, type_argument_values.clone()), |x| {
                                x.unwrap().definition.type_.clone()
                            }),
                    );
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                types::InstantiatedTypeKind::Array { element_type: a } => Value::Reference(
                    builtins::array::ArrayValue::build_instance(a.as_ref(), context),
                ),
                types::InstantiatedTypeKind::Callable { .. } => todo!(),
                types::InstantiatedTypeKind::U64 => Value::Primitive(
                    CompilerContext::get_std_type("u64"),
                    types::TypeArgumentValues::new_empty(),
                    argument_value,
                ),
                types::InstantiatedTypeKind::Pointer(_) => todo!(),
                types::InstantiatedTypeKind::U8 => todo!(),
                types::InstantiatedTypeKind::Struct(_) => todo!(),
                types::InstantiatedTypeKind::Function(_) => todo!(),
            };

            scope.set_value(argument.name, value);
        }

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc::build_prologue(&rcs, context);

        CompiledFunction {
            handle: handle.clone(),
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

    pub(crate) fn has_function(&self, name: &MangledIdentifier) -> bool {
        self.llvm_module
            .get_function(name.as_str())
            .is_some_and(|x| x.count_basic_blocks() > 0)
    }

    pub(crate) fn get_or_create_function(
        &self,
        handle: &types::functions::Function<InstantiatedType>,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.llvm_module
            .get_function(handle.mangled_id().as_str())
            .unwrap_or_else(|| {
                self.declare_function(
                    handle.linkage(),
                    &handle.mangled_id(),
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
