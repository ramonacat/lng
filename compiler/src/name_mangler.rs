use std::fmt::Write;

use crate::{
    identifier::{FQName, Identifier},
    types::{InstantiatedType, TypeArgumentValues, modules::ModuleId, structs::StructId},
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
}

pub fn mangle_type_argument_values(tav: &TypeArgumentValues<InstantiatedType>) -> String {
    let mut result = String::new();

    for (id, value) in &tav.0 {
        write!(result, "{id}$_${value}").unwrap();
    }

    result
}

pub fn mangle_item_name(
    module: ModuleId,
    item: Identifier,
    tav: &TypeArgumentValues<InstantiatedType>,
) -> MangledIdentifier {
    MangledIdentifier {
        mangled: module.into_mangled().mangled
            + "$$"
            + &item.raw()
            + "$$$"
            + &mangle_type_argument_values(tav),
        source: IdentifierKind::Module(module, item),
    }
}

pub fn mangle_struct_item_name(
    struct_id: StructId,
    item: Identifier,
    tav: &TypeArgumentValues<InstantiatedType>,
) -> MangledIdentifier {
    MangledIdentifier {
        mangled: struct_id.into_mangled(tav).mangled + "$$" + &item.raw() + "$$$",
        source: IdentifierKind::Struct(struct_id, item),
    }
}

pub fn mangle_fq_name(fq_name: FQName) -> MangledIdentifier {
    let mangled_module_path = fq_name.to_string().replace('.', "$");

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
