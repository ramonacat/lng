use std::{collections::HashMap, fmt::Debug, rc::Rc, sync::RwLock};

use crate::types::{self, ItemPath};

use super::{Value, context::CompilerContext, module::CompiledModule};

pub struct Scope<'ctx> {
    locals: RwLock<HashMap<types::Identifier, Value<'ctx>>>,
    parent: Option<Rc<Scope<'ctx>>>,
}

impl Debug for Scope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f, 0)
    }
}

impl<'ctx> Scope<'ctx> {
    pub(crate) fn debug_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        nesting_level: usize,
    ) -> std::fmt::Result {
        let indentation = "  ".repeat(nesting_level);
        write!(f, "{indentation}")?;
        writeln!(f, "{{")?;

        for local in self.locals.read().unwrap().iter() {
            write!(f, "{indentation}{indentation}")?;
            writeln!(f, "{}: {:?}", local.0, local.1)?;
        }

        if let Some(parent) = &self.parent {
            write!(f, "{indentation}{indentation}")?;
            writeln!(f, "=== PARENT ===")?;
            parent.debug_print(f, nesting_level + 1)?;
        }

        write!(f, "{indentation}")?;
        writeln!(f, "}}")?;

        Ok(())
    }
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

impl Debug for GlobalScope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;

        for module in &self.modules {
            writeln!(f, "  {}: ", module.0)?;
            module.1.debug_print(f, 1)?;
        }

        writeln!(f, "===SCOPE===")?;

        self.scope.debug_print(f, 1)?;

        writeln!(f, "}}")?;

        Ok(())
    }
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

    pub fn get_value(&self, item_path: &ItemPath) -> Option<Value<'ctx>> {
        self.modules
            .get(&item_path.module)
            .and_then(|x| x.get_variable(&item_path.item))
    }

    pub fn into_modules(self) -> impl Iterator<Item = CompiledModule<'ctx>> {
        self.modules.into_values()
    }

    pub(crate) fn register(&self, id: types::Identifier, value: Value<'ctx>) {
        self.scope.set_value(id, value);
    }
}
