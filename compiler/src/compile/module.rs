use std::{collections::HashMap, rc::Rc};

use super::{
    CompileError, CompiledFunction, FunctionHandle, Scope, Value,
    context::CompilerContext,
    rc_builder::{self, RcValue},
};
use crate::{
    ast::SourceRange,
    name_mangler::MangledIdentifier,
    types::{self, Identifier, ModulePath},
};
use inkwell::{
    module::{Linkage, Module},
    values::FunctionValue,
};

pub struct CompiledModule<'ctx> {
    pub path: types::ModulePath,
    pub llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
}

impl<'ctx> CompiledModule<'ctx> {
    // TODO implement name mangling to avoid collisions between functions from different modules!!!
    fn declare_function_inner(
        &self,
        name: MangledIdentifier,
        arguments: &[types::Argument],
        return_type: &types::Type,
        linkage: Linkage,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let arguments = arguments
            .iter()
            .map(|arg| context.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        let function_type = context
            .type_to_llvm(return_type)
            .fn_type(&arguments[..], false);

        self.llvm_module
            .add_function(name.as_str(), function_type, Some(linkage))
    }

    pub fn declare_function(
        &self,
        export: bool,
        name: MangledIdentifier,
        arguments: &[types::Argument],
        return_type: &types::Type,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let linkage = if export {
            Linkage::External
        } else {
            Linkage::Internal
        };

        self.declare_function_inner(name, arguments, return_type, linkage, context)
    }

    pub fn import_function(
        &self,
        function: &FunctionHandle,
        name: MangledIdentifier,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(
            name,
            &function.arguments,
            &function.return_type,
            Linkage::External,
            context,
        )
    }
    pub fn resolve_function(
        &self,
        name: &Identifier,
        location: SourceRange,
        struct_: Option<Identifier>,
    ) -> Result<FunctionHandle, CompileError> {
        if let Some(struct_) = struct_ {
            let struct_handle = self
                .scope
                .get_struct(&struct_, self.path.clone(), location)
                .unwrap();
            let function_value = struct_handle.static_fields.get(name).unwrap();

            return match function_value {
                Value::Function(function_handle) => Ok(function_handle.clone()),
                _ => todo!(),
            };
        }
        self.scope.get_function(name, self.path.clone(), location)
    }

    pub fn set_variable(&self, name: Identifier, value: Value<'ctx>) {
        self.scope.register(name, value);
    }

    pub fn get_variable(&self, name: &Identifier) -> Option<Value<'ctx>> {
        self.scope.get_variable(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        function: &types::Function,
        context: &CompilerContext<'ctx>,
        struct_: Option<Identifier>,
    ) -> Result<super::CompiledFunction<'ctx>, CompileError> {
        let mut rcs = vec![];
        let handle = self.resolve_function(&function.name, function.location, struct_)?;
        let scope = self.scope.child();

        let llvm_function = handle.get_or_create_in_module(self, context);
        for (argument, argument_value) in function.arguments.iter().zip(llvm_function.get_params())
        {
            let value = match &argument.type_ {
                types::Type::Void => todo!(),
                types::Type::Object(identifier) => {
                    let rc = RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        scope.get_struct(identifier, self.path.clone(), argument.position)?,
                        context,
                    );
                    rcs.push(rc.clone());

                    Value::Reference(rc)
                }
                // TODO there should be a built-in object for arrays that has bounds, etc. and
                // this object should be passed to the function, instead to a reference
                types::Type::Array(a) => match &**a {
                    types::Type::Void => todo!(),
                    types::Type::Object(identifier) => Value::Reference(RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        scope.get_struct(identifier, self.path.clone(), argument.position)?,
                        context,
                    )),
                    types::Type::Array(_) => todo!(),
                    types::Type::StructDescriptor(_, _) => todo!(),
                    types::Type::Callable { .. } => todo!(),
                    types::Type::U64 => todo!(),
                },
                types::Type::StructDescriptor(_, _) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => Value::Primitive(
                    self.scope
                        .get_struct(
                            &Identifier::parse("u64"),
                            self.path.clone(),
                            argument.position,
                        )
                        .unwrap(),
                    argument_value,
                ),
            };

            scope.register(argument.name.clone(), value);
        }

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc_builder::build_prologue(&rcs, context);

        Ok(CompiledFunction {
            handle,
            scope,
            entry: entry_block,
            end: end_block,
            rcs,
            return_value: None,
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

    // TODO rename to get_module
    pub fn get(&self, path: &types::ModulePath) -> Option<&CompiledModule<'ctx>> {
        self.modules.get(path)
    }

    pub fn get_value(
        &self,
        module: &types::ModulePath,
        name: &types::Identifier,
    ) -> Option<Value<'ctx>> {
        self.modules.get(module).and_then(|x| x.get_variable(name))
    }

    pub fn into_modules(self) -> impl Iterator<Item = CompiledModule<'ctx>> {
        self.modules.into_values()
    }

    pub(crate) fn register(&self, id: Identifier, value: Value<'ctx>) {
        self.scope.register(id, value);
    }
}
