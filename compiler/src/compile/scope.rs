use std::{collections::HashMap, fmt::Debug, rc::Rc, sync::RwLock};

use inkwell::module::Module;

use crate::types::{self, FQName};

use super::{Value, module::CompiledModule};

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

    pub fn set_value(&self, name: types::Identifier, value: Value<'ctx>) {
        let old = self.locals.write().unwrap().insert(name, value);
        assert!(old.is_none());
    }

    pub fn get_value(&self, name: types::Identifier) -> Option<Value<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(&name) {
            return Some(variable.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get_value(name);
        }

        None
    }
}

pub struct GlobalScope<'ctx> {
    modules: HashMap<types::FQName, CompiledModule<'ctx>>,
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
        Self {
            modules: HashMap::new(),
            scope: Scope::root(),
        }
    }
}

impl<'ctx> GlobalScope<'ctx> {
    pub(super) fn get_or_create_module(
        &mut self,
        path: types::FQName,
        create_llvm_module: impl FnOnce() -> Module<'ctx>,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .entry(path)
            .or_insert_with(|| CompiledModule::new(path, self.scope.child(), create_llvm_module()))
    }

    pub fn get_module(&self, path: types::FQName) -> Option<&CompiledModule<'ctx>> {
        self.modules.get(&path)
    }

    pub(crate) fn get_module_mut(
        &mut self,
        root_path: FQName,
    ) -> Option<&mut CompiledModule<'ctx>> {
        self.modules.get_mut(&root_path)
    }

    pub fn get_value(&self, item_path: FQName) -> Option<Value<'ctx>> {
        self.modules
            .get(&item_path.without_last())
            .and_then(|x| x.get_variable(item_path.last()))
    }

    pub fn into_modules(self) -> impl Iterator<Item = CompiledModule<'ctx>> {
        self.modules.into_values()
    }
}
