use std::rc::Rc;

use super::{
    CompiledFunction, FunctionHandle, Scope, Value,
    context::CompilerContext,
    rc_builder::{self, RcValue},
};
use crate::{
    name_mangler::{MangledIdentifier, mangle_module},
    types,
};
use inkwell::{
    module::{Linkage, Module},
    values::FunctionValue,
};

pub struct CompiledModule<'ctx> {
    path: types::ModulePath,
    llvm_module: Module<'ctx>,
    scope: Rc<Scope<'ctx>>,
}

impl std::fmt::Debug for CompiledModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f, 0)
    }
}

impl<'ctx> CompiledModule<'ctx> {
    pub fn new(
        path: types::ModulePath,
        scope: Rc<Scope<'ctx>>,
        context: &CompilerContext<'ctx>,
    ) -> Self {
        let llvm_module = context
            .llvm_context
            .create_module(mangle_module(&path).as_str());
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
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        let function_type = context.make_function_type(arguments, return_type);

        self.llvm_module
            .add_function(name.as_str(), function_type, Some(linkage))
    }

    pub fn declare_function(
        &self,
        export: bool,
        name: &MangledIdentifier,
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
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(
            &function.name,
            &function.arguments,
            &function.return_type,
            Linkage::External,
            context,
        )
    }

    pub fn set_variable(&self, name: types::Identifier, value: Value<'ctx>) {
        self.scope.set_value(name, value);
    }

    pub fn get_variable(&self, name: &types::Identifier) -> Option<Value<'ctx>> {
        self.scope.get_value(name)
    }

    pub(crate) fn begin_compile_function(
        &self,
        handle: FunctionHandle,
        function: &types::FunctionDefinition,
        context: &CompilerContext<'ctx>,
    ) -> super::CompiledFunction<'ctx> {
        let mut rcs = vec![];
        let scope = self.scope.child();

        let llvm_function = handle.get_or_create_in_module(self, context);
        for (argument, argument_value) in function.arguments.iter().zip(llvm_function.get_params())
        {
            let value = match &argument.type_ {
                types::Type::Void => todo!(),
                types::Type::Object(identifier) => {
                    let rc = RcValue::from_pointer(
                        argument_value.into_pointer_value(),
                        // TODO don't ignore identifier's module!
                        scope
                            .get_value(&identifier.item)
                            .unwrap()
                            .as_struct()
                            .unwrap(),
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
                        // TODO don't ignore identifier's module!
                        scope
                            .get_value(&identifier.item)
                            .unwrap()
                            .as_struct()
                            .unwrap(),
                    )),
                    types::Type::Array(_) => todo!(),
                    types::Type::StructDescriptor(_, _) => todo!(),
                    types::Type::Callable { .. } => todo!(),
                    types::Type::U64 => todo!(),
                    types::Type::Pointer(_) => todo!(),
                    types::Type::U8 => todo!(),
                },
                types::Type::StructDescriptor(_, _) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => Value::Primitive(
                    self.scope
                        .get_value(&types::Identifier::parse("u64"))
                        .unwrap()
                        .as_struct()
                        .unwrap(),
                    argument_value,
                ),
                types::Type::Pointer(_) => todo!(),
                types::Type::U8 => todo!(),
            };

            scope.set_value(argument.name.clone(), value);
        }

        let entry_block = context
            .llvm_context
            .append_basic_block(llvm_function, "entry");

        let end_block = context
            .llvm_context
            .append_basic_block(llvm_function, "end");

        context.builder.position_at_end(entry_block);

        rc_builder::build_prologue(&rcs, context);

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

    pub(crate) fn path(&self) -> types::ModulePath {
        self.path.clone()
    }

    // TODO this is a kinda weird API, FunctionHandle should manage the FunctionValue by itself
    // probably?
    pub(crate) fn get_llvm_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.llvm_module.get_function(name)
    }

    pub(crate) fn debug_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        nesting_level: usize,
    ) -> std::fmt::Result {
        self.scope.debug_print(f, nesting_level)
    }
}
