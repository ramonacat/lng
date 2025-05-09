use itertools::Itertools;

use crate::{
    identifier::{FQName, Identifier},
    types::{self, modules::ModuleId, structs::StructId},
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    FQName(FQName),
    Module(ModuleId, Identifier),
    Identifier(Identifier),
    Struct(StructId, Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MangledIdentifier {
    mangled: String,
    source: IdentifierKind,
}

impl MangledIdentifier {
    pub(crate) fn as_str(&self) -> &str {
        self.mangled.as_str()
    }

    pub(crate) fn with_types(self, arg: &types::TypeArgumentValues) -> Self {
        Self {
            mangled: self.mangled
                + "$$$"
                + &(arg
                    .0
                    .iter()
                    .map(|(name, value)| name.to_string() + "$" + &value.to_string())
                    .join("$$")),
            source: self.source,
        }
    }
}

pub fn mangle_item_name(module: ModuleId, item: Identifier) -> MangledIdentifier {
    MangledIdentifier {
        mangled: module.into_mangled().mangled + "$" + &item.raw(),
        source: IdentifierKind::Module(module, item),
    }
}

pub fn mangle_struct_item_name(struct_id: StructId, item: Identifier) -> MangledIdentifier {
    MangledIdentifier {
        mangled: struct_id.into_mangled().mangled + "$" + &item.raw(),
        source: IdentifierKind::Struct(struct_id, item),
    }
}

pub fn mangle_fq_name(fq_name: FQName) -> MangledIdentifier {
    let mangled_module_path = fq_name
        .parts()
        .iter()
        .copied()
        .map(Identifier::raw)
        .join("$");

    MangledIdentifier {
        mangled: mangled_module_path,
        source: IdentifierKind::FQName(fq_name),
    }
}

pub fn nomangle_identifier(identifier: Identifier) -> MangledIdentifier {
    MangledIdentifier {
        mangled: identifier.raw(),
        source: IdentifierKind::Identifier(identifier),
    }
}
