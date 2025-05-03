use std::rc::Rc;

use super::{
    CompiledFunction, FunctionHandle, Scope, Value, builtins,
    context::CompilerContext,
    rc::{self, RcValue},
    value::InstantiatedStructHandle,
};
use crate::{name_mangler::MangledIdentifier, types};
use inkwell::{
    module::{Linkage, Module},
    values::FunctionValue,
};

pub struct CompiledModule<'ctx> {
    path: types::FQName,
    llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
}

impl std::fmt::Debug for CompiledModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f, 0)
    }
}

impl<'ctx> CompiledModule<'ctx> {
    pub const fn new(
        path: types::FQName,
        scope: Rc<Scope<'ctx>>,
        llvm_module: Module<'ctx>,
    ) -> Self {
        Self {
            path,
            llvm_module,
            scope,
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
        arguments: &[types::Argument],
        return_type: &types::Type,
        linkage: Linkage,
        type_argument_values: &types::TypeArgumentValues,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let function_type =
            context.make_function_type(arguments, return_type, type_argument_values);

        self.llvm_module
            .add_function(name.as_str(), function_type, Some(linkage))
    }

    pub fn declare_function(
        &self,
        linkage: Linkage,
        name: &MangledIdentifier,
        arguments: &[types::Argument],
        return_type: &types::Type,
        type_argument_values: &types::TypeArgumentValues,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(
            name,
            arguments,
            return_type,
            linkage,
            type_argument_values,
            context,
        )
    }

    pub fn import_function(
        &self,
        function: &FunctionHandle,
        type_argument_values: &types::TypeArgumentValues,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(
            &function.name,
            &function.arguments,
            &function.return_type,
            Linkage::External,
            type_argument_values,
            context,
        )
    }

    pub fn set_variable(&self, name: types::Identifier, value: Value<'ctx>) {
        self.scope.set_value(name, value);
    }

    pub fn get_variable(&self, name: types::Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        handle: FunctionHandle,
        function: &types::FunctionDefinition,
        type_argument_values: &types::TypeArgumentValues,
        context: &CompilerContext<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = self.get_or_create_function(&handle, type_argument_values, context);

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        context.builder.position_at_end(entry_block);

        for (argument, argument_value) in function.arguments.iter().zip(llvm_function.get_params())
        {
            let value = match &argument.type_ {
                types::Type::Unit => todo!(),
                types::Type::Object {
                    type_name: identifier,
                    type_argument_values: object_tav,
                } => {
                    let value_type = context
                        .global_scope
                        .get_value(*identifier)
                        .unwrap()
                        .as_struct()
                        .unwrap();
                    let value_type = InstantiatedStructHandle::new(value_type, object_tav.clone());
                    let rc = RcValue::from_pointer(argument_value.into_pointer_value(), value_type);
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                types::Type::Array { element_type: a } => Value::Reference(
                    builtins::array::ArrayValue::build_instance(a.as_ref(), context),
                ),
                types::Type::StructDescriptor(_) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => {
                    Value::Primitive(context.get_std_type("u64").unwrap(), argument_value)
                }
                types::Type::Pointer(_) => todo!(),
                types::Type::U8 => todo!(),
                types::Type::Generic(_) => todo!(),
            };

            scope.set_value(argument.name, value);
        }

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc::build_prologue(&rcs, context);

        CompiledFunction {
            handle,
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

    pub(crate) const fn path(&self) -> types::FQName {
        self.path
    }

    pub(crate) fn get_or_create_function(
        &self,
        handle: &FunctionHandle,
        type_argument_values: &types::TypeArgumentValues,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.llvm_module
            .get_function(handle.name.as_str())
            .unwrap_or_else(|| {
                self.declare_function(
                    handle.linkage,
                    &handle.name,
                    &handle.arguments,
                    &handle.return_type,
                    type_argument_values,
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
