use itertools::Itertools;

use crate::types::{FQName, Identifier};

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    FQName(FQName),
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
