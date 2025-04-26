use std::{collections::HashMap, rc::Rc, sync::RwLock};

use inkwell::module::Module;

use crate::types;

use super::{Value, module::CompiledModule};

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

    // TODO rename to set_value, check for overwrite!
    pub fn register(&self, name: types::Identifier, value: Value<'ctx>) {
        self.locals.write().unwrap().insert(name, value);
    }

    // TODO rename to get_value
    pub fn get_variable(&self, name: &types::Identifier) -> Option<Value<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(name) {
            return Some(variable.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_variable(name);
        }

        None
    }
}

pub struct GlobalScope<'ctx> {
    modules: HashMap<types::ModulePath, CompiledModule<'ctx>>,
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
            // TODO the llvm_module should be created by CompiledModule itself?
            .or_insert_with(|| CompiledModule::new(path, llvm_module, self.scope.child()))
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
        self.scope.register(id, value);
    }
}
