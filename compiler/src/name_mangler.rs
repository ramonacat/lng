use itertools::Itertools;

use crate::{
    identifier::Identifier,
    types::{self, TypeArgumentValues},
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    FQName(types::FQName),
    Identifier(Identifier),
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

    pub(crate) fn with_types(self, arg: &TypeArgumentValues) -> Self {
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

pub fn mangle_fq_name(fq_name: types::FQName) -> MangledIdentifier {
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
