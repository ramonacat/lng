use std::{collections::HashMap, rc::Rc};

use super::{
    context::CompilerContext,
    rc_builder::{self, RcValue},
    CompileError, CompileErrorDescription, CompiledFunction, FunctionHandle, Scope, ValueType,
};
use crate::{
    ast::SourceRange,
    types::{self, Identifier, ModulePath},
};
use inkwell::module::Module;

pub struct CompiledModule<'ctx> {
    pub path: types::ModulePath,
    pub llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
}

impl<'ctx> CompiledModule<'ctx> {
    pub fn resolve_function(
        &self,
        name: &Identifier,
        location: SourceRange,
    ) -> Result<FunctionHandle<'ctx>, CompileError> {
        self.scope.get_function(name, self.path.clone(), location)
    }

    pub(crate) fn set_variable(&self, name: Identifier, value: ValueType<'ctx>) {
        self.scope.register(name, value);
    }

    pub(crate) fn begin_compile_function(
        &self,
        function: &types::Function,
        context: &CompilerContext<'ctx>,
    ) -> Result<super::CompiledFunction<'ctx>, CompileError> {
        let mut rcs = vec![];
        let handle = self.resolve_function(&function.name, function.location)?;
        let scope = self.scope.child();

        for (argument, argument_value) in function
            .arguments
            .iter()
            .zip(handle.llvm_function.get_params())
        {
            let value = if argument.type_.is_primitive() {
                ValueType::Value(argument_value.into())
            } else {
                let rc = RcValue::from_pointer(argument_value.into_pointer_value(), context);
                rcs.push(rc);

                ValueType::Reference(rc)
            };

            scope.register(argument.name.clone(), value);
        }

        let entry_block = context
            .llvm_context
            .append_basic_block(handle.llvm_function, "entry");

        let end_block = context
            .llvm_context
            .append_basic_block(handle.llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc_builder::build_prologue(&rcs, context);

        Ok(CompiledFunction {
            handle,
            scope,
            entry: entry_block,
            end: end_block,
            rcs,
        })
    }
}

pub struct GlobalScope<'ctx> {
    modules: HashMap<ModulePath, CompiledModule<'ctx>>,
    scope: Rc<Scope<'ctx>>,
}

impl<'ctx> GlobalScope<'ctx> {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            scope: Scope::root(),
        }
    }

    pub fn create_module(
        &mut self,
        path: types::ModulePath,
        llvm_module: Module<'ctx>,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .entry(path.clone())
            .or_insert_with(|| CompiledModule {
                path,
                llvm_module,
                scope: self.scope.child(),
            })
    }

    pub fn get(&self, path: &types::ModulePath) -> Option<&CompiledModule<'ctx>> {
        self.modules.get(path)
    }

    pub fn resolve_function(
        &self,
        module: &types::ModulePath,
        name: &types::Identifier,
        location: SourceRange,
    ) -> Result<FunctionHandle<'ctx>, CompileError> {
        self.modules
            .get(module)
            .ok_or_else(|| {
                CompileErrorDescription::ModuleNotFound(module.clone()).at(module.clone(), location)
            })?
            .resolve_function(name, location)
    }

    pub fn into_modules(self) -> impl Iterator<Item = CompiledModule<'ctx>> {
        self.modules.into_values()
    }
}
