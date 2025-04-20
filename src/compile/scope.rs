use std::{collections::HashMap, rc::Rc, sync::RwLock};

use crate::{
    ast::SourceRange,
    types::{self, ModulePath},
};

use super::{CompileError, CompileErrorDescription, FunctionHandle, ValueType};

pub struct Scope<'ctx> {
    locals: RwLock<HashMap<types::Identifier, ValueType<'ctx>>>,
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

    pub fn register(&self, name: types::Identifier, value: ValueType<'ctx>) {
        self.locals.write().unwrap().insert(name, value);
    }

    pub fn get_variable(&self, name: &types::Identifier) -> Option<ValueType<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(name) {
            return Some(variable.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_variable(name);
        }

        None
    }

    // TODO make (ModulePath, SourceLocation) an instance of ErrorLocation
    pub fn get_function(
        &self,
        name: &types::Identifier,
        module_path: ModulePath,
        location: SourceRange,
    ) -> Result<FunctionHandle<'ctx>, CompileError> {
        let called_item = self.get_variable(name).ok_or_else(|| {
            CompileErrorDescription::FunctionNotFound {
                module_name: module_path.clone(),
                function_name: name.clone(),
            }
            .at(module_path.clone(), location)
        })?;

        let ValueType::Function(function) = called_item else {
            todo!();
        };

        Ok(function.clone())
    }
}
