use std::{collections::HashMap, rc::Rc};

use super::{
    CompiledFunction, FunctionHandle, Scope, Value, builtins,
    context::CompilerContext,
    rc::{self, RcValue},
};
use crate::{identifier::Identifier, name_mangler::MangledIdentifier, types};
use inkwell::{
    module::{Linkage, Module},
    values::FunctionValue,
};

pub struct CompiledModule<'ctx> {
    llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
    imported_functions: HashMap<types::functions::FunctionId, FunctionHandle>,
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
            imported_functions: HashMap::new(),
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
        return_type: &types::Type,
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
        return_type: &types::Type,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(name, arguments, return_type, linkage, context)
    }

    pub(crate) fn import_function(&mut self, function: FunctionHandle) {
        self.imported_functions.insert(function.id, function);
    }

    pub(crate) fn set_variable(&self, name: Identifier, value: Value<'ctx>) {
        self.scope.set_value(name, value);
    }

    pub(crate) fn get_variable(&self, name: Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        handle: FunctionHandle,
        context: &CompilerContext<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = self.get_or_create_function(&handle, context);

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        context.builder.position_at_end(entry_block);

        for (argument, argument_value) in handle.arguments.iter().zip(llvm_function.get_params()) {
            let value = match &argument.type_.kind() {
                types::TypeKind::Unit => todo!(),
                types::TypeKind::Object { type_name: id } => {
                    let rc = RcValue::from_pointer(argument_value.into_pointer_value(), id.clone());
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                types::TypeKind::Array { element_type: a } => Value::Reference(
                    builtins::array::ArrayValue::build_instance(a.as_ref(), context),
                ),
                types::TypeKind::Callable { .. } => todo!(),
                types::TypeKind::U64 => {
                    Value::Primitive(CompilerContext::get_std_type("u64"), argument_value)
                }
                types::TypeKind::Pointer(_) => todo!(),
                types::TypeKind::U8 => todo!(),
                types::TypeKind::Generic(_) => todo!(),
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

    pub(crate) fn has_function(&self, name: &MangledIdentifier) -> bool {
        self.llvm_module.get_function(name.as_str()).is_some()
    }

    pub(crate) fn get_or_create_function(
        &self,
        handle: &FunctionHandle,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.llvm_module
            .get_function(handle.id.into_mangled().as_str())
            .unwrap_or_else(|| {
                self.declare_function(
                    handle.linkage,
                    &handle.id.into_mangled(),
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
