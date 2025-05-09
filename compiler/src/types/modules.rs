use crate::types::FunctionId;
use crate::{
    identifier::{FQName, Identifier},
    name_mangler::MangledIdentifier,
    types::Struct,
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use super::{Item, ItemKind, functions::Function, structs::StructId};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    FQName(FQName),
}

impl Display for ModuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FQName(fqname) => write!(f, "{fqname}"),
        }
    }
}

impl ModuleId {
    pub(crate) fn root() -> Self {
        Self::FQName(FQName::parse(""))
    }

    pub(crate) fn child(self, path: Identifier) -> Self {
        match self {
            Self::FQName(fqname) => Self::FQName(fqname.with_part(path)),
        }
    }

    pub(crate) fn parse(arg: &str) -> Self {
        Self::FQName(FQName::parse(arg))
    }

    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        match self {
            Self::FQName(fqname) => fqname.into_mangled(),
        }
    }
}

pub struct AppModule {
    main: FunctionId,
    module: Module,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
}
pub struct LibraryModule {
    module: Module,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
}

pub enum RootModule {
    App(AppModule),
    Library(LibraryModule),
}

impl RootModule {
    pub(crate) const fn main(&self) -> Option<FunctionId> {
        match self {
            Self::App(app_module) => Some(app_module.main),
            Self::Library(_) => None,
        }
    }

    pub(crate) const fn root_module(&self) -> &Module {
        match self {
            Self::App(app_module) => &app_module.module,
            Self::Library(library_module) => &library_module.module,
        }
    }

    pub(crate) const fn structs(&self) -> &HashMap<StructId, Struct> {
        match self {
            Self::App(app_module) => &app_module.structs,
            Self::Library(library_module) => &library_module.structs,
        }
    }

    pub(crate) const fn functions(&self) -> &HashMap<FunctionId, Function> {
        match self {
            Self::App(app_module) => &app_module.functions,
            Self::Library(library_module) => &library_module.functions,
        }
    }

    pub(crate) const fn new_app(
        main: FunctionId,
        root_module: Module,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
    ) -> Self {
        Self::App(AppModule {
            main,
            module: root_module,
            structs,
            functions,
        })
    }

    pub(crate) const fn new_library(
        root_module: Module,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
    ) -> Self {
        Self::Library(LibraryModule {
            module: root_module,
            structs,
            functions,
        })
    }
}

#[derive(Clone)]
pub struct Module {
    items: HashMap<Identifier, Item>,
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut struct_ = f.debug_struct("module");
        for item in &self.items {
            struct_.field(&item.0.raw(), item.1);
        }
        struct_.finish()
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub(crate) fn declare_item(&mut self, name: Identifier, item: Item) {
        self.items.insert(name, item);
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (Identifier, &Item)> {
        self.items.iter().map(|(k, v)| (*k, v))
    }

    pub(crate) fn get_item(&self, item_name: FQName) -> Option<&Item> {
        if item_name.len() == 1 {
            return self.items.get(&item_name.last());
        }

        let (first, rest) = item_name.split_first();

        // TODO check visibility (but for that we need an argument to tell us whether we should,
        // and what the accessing module is)
        let Item {
            kind: ItemKind::Module(module),
            visibility: _,
            position: _,
        } = self.items.get(&first).unwrap()
        else {
            todo!();
        };

        module.get_item(rest)
    }
}
