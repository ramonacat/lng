use crate::types::FunctionId;
use crate::{identifier::FQName, types::Struct};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use super::store::MultiStore;
use super::{functions::Function, structs::StructId};

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
    pub(crate) fn parse(arg: &str) -> Self {
        Self::FQName(FQName::parse(arg))
    }

    // TODO this is problematic, we'll need
    // a proper toposort to be able to get rid of it
    pub(crate) fn len(self) -> usize {
        match self {
            Self::FQName(fqname) => fqname.len(),
        }
    }
}

pub struct AppModule {
    main: FunctionId,
    modules: HashMap<ModuleId, Module>,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
    types: MultiStore,
}

pub struct LibraryModule {
    modules: HashMap<ModuleId, Module>,
    structs: HashMap<StructId, Struct>,
    functions: HashMap<FunctionId, Function>,
    types: MultiStore,
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

    pub(crate) const fn modules(&self) -> &HashMap<ModuleId, Module> {
        match self {
            Self::App(app_module) => &app_module.modules,
            Self::Library(library_module) => &library_module.modules,
        }
    }

    pub(crate) const fn types(&self) -> &MultiStore {
        match self {
            Self::App(app_module) => &app_module.types,
            Self::Library(library_module) => &library_module.types,
        }
    }

    pub(crate) const fn new_app(
        main: FunctionId,
        modules: HashMap<ModuleId, Module>,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
        types: MultiStore,
    ) -> Self {
        Self::App(AppModule {
            main,
            modules,
            structs,
            functions,
            types,
        })
    }

    pub(crate) const fn new_library(
        modules: HashMap<ModuleId, Module>,
        structs: HashMap<StructId, Struct>,
        functions: HashMap<FunctionId, Function>,
        types: MultiStore,
    ) -> Self {
        Self::Library(LibraryModule {
            modules,
            structs,
            functions,
            types,
        })
    }
}

#[derive(Clone)]
pub struct Module {
    #[allow(unused)]
    id: ModuleId,
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "module")
    }
}

impl Module {
    pub const fn new(id: ModuleId) -> Self {
        Self { id }
    }
}
