use std::{collections::HashMap, rc::Rc, sync::RwLock};

use crate::{
    ast::SourceRange,
    types::{self, ModulePath},
};

use super::{CompileError, CompileErrorDescription, FunctionHandle, StructHandle, Value};

pub struct Scope<'ctx> {
    locals: RwLock<HashMap<types::Identifier, Value<'ctx>>>,
    parent: Option<Rc<Scope<'ctx>>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn root() -> Rc<Self> {
        Rc::new(Self {
            locals: RwLock::new(HashMap::new()),
            parent: None,
        })
    }

    pub fn child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            locals: RwLock::new(HashMap::new()),
            parent: Some(self.clone()),
        })
    }

    pub fn register(&self, name: types::Identifier, value: Value<'ctx>) {
        self.locals.write().unwrap().insert(name, value);
    }

    pub fn get_variable(&self, name: &types::Identifier) -> Option<Value<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(name) {
            return Some(variable.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_variable(name);
        }

        None
    }

    // TODO make (ModulePath, SourceLocation) an instance of ErrorLocation
    // TODO make this get_callable, and handle other types of callables when they exist
    // TODO instead of the get_* methods, there should be into_ on Value
    pub fn get_function(
        &self,
        name: &types::Identifier,
        module_path: ModulePath,
        location: SourceRange,
    ) -> Result<FunctionHandle, CompileError> {
        let called_item = self.get_variable(name).ok_or_else(|| {
            CompileErrorDescription::FunctionNotFound {
                module_name: module_path.clone(),
                function_name: name.clone(),
            }
            .at(module_path.clone(), location)
        })?;

        let Value::Function(function) = called_item else {
            todo!();
        };

        Ok(function.clone())
    }

    pub fn get_struct(
        &self,
        name: &types::Identifier,
        module_path: ModulePath,
        location: SourceRange,
    ) -> Result<StructHandle<'ctx>, CompileError> {
        let called_item = self.get_variable(name).ok_or_else(|| {
            CompileErrorDescription::StructNotFound {
                module_name: module_path.clone(),
                struct_name: name.clone(),
            }
            .at(module_path.clone(), location)
        })?;

        let Value::Struct(struct_) = called_item else {
            todo!();
        };

        Ok(struct_.clone())
    }
}
