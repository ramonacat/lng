use std::{collections::HashMap, rc::Rc, sync::RwLock};

use crate::types;

use super::{Value, context::CompilerContext, module::CompiledModule};

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

    // TODO check for overwrite!
    pub fn set_value(&self, name: types::Identifier, value: Value<'ctx>) {
        self.locals.write().unwrap().insert(name, value);
    }

    pub fn get_value(&self, name: &types::Identifier) -> Option<Value<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(name) {
            return Some(variable.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_value(name);
        }

        None
    }
}

pub struct GlobalScope<'ctx> {
    modules: HashMap<types::ModulePath, CompiledModule<'ctx>>,
    scope: Rc<Scope<'ctx>>,
}

impl Default for GlobalScope<'_> {
    fn default() -> Self {
        Self::new()
    }
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
        context: &CompilerContext<'ctx>,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .entry(path.clone())
            .or_insert_with(|| CompiledModule::new(path, self.scope.child(), context))
    }

    pub fn get_module(&self, path: &types::ModulePath) -> Option<&CompiledModule<'ctx>> {
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

    pub(crate) fn register(&self, id: types::Identifier, value: Value<'ctx>) {
        self.scope.set_value(id, value);
    }
}
