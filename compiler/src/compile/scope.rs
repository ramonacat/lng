use std::{collections::HashMap, fmt::Debug, rc::Rc, sync::RwLock};

use crate::{identifier::Identifier, types};

use super::{Value, context::AllItems};

pub struct Scope<'ctx> {
    locals: RwLock<HashMap<Identifier, Value<'ctx>>>,
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

    pub(crate) fn set_value(&self, name: Identifier, value: Value<'ctx>) {
        let old = self.locals.write().unwrap().insert(name, value);
        assert!(old.is_none(), "{name}: {old:?}");
    }

    pub(crate) fn get_value(&self, name: Identifier) -> Option<Value<'ctx>> {
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
    scope: Rc<Scope<'ctx>>,
    // TODO should it be made private?
    pub structs: AllItems<'ctx>,
}

impl Debug for GlobalScope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;

        writeln!(f, "---STRUCTS---")?;

        writeln!(f, "{:?}", &self.structs)?;

        writeln!(f, "===SCOPE===")?;

        self.scope.debug_print(f, 1)?;

        writeln!(f, "}}")?;

        Ok(())
    }
}

impl<'ctx> GlobalScope<'ctx> {
    pub(crate) fn new(
        structs: HashMap<types::structs::StructId, types::structs::Struct<types::GenericType>>,
        functions: HashMap<
            types::functions::FunctionId,
            types::functions::Function<types::GenericType>,
        >,
    ) -> Self {
        Self {
            scope: Scope::root(),
            structs: AllItems::new(structs, functions),
        }
    }

    // TODO should we take care of creating child scopes here, instead of exposing scope()?
    pub(crate) const fn scope(&self) -> &Rc<Scope<'ctx>> {
        &self.scope
    }
}
