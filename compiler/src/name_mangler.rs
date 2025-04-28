use itertools::Itertools;

use crate::types::{FieldPath, Identifier, ItemPath, ModulePath};

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    FieldPath(FieldPath),
    ItemPath(ItemPath),
    Module(ModulePath),
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

pub fn mangle_module(module: ModulePath) -> MangledIdentifier {
    let mangled_module_path = module.parts().iter().map(Identifier::raw).join("$");

    MangledIdentifier {
        mangled: mangled_module_path,
        source: IdentifierKind::Module(module),
    }
}

pub fn nomangle_item(identifier: Identifier) -> MangledIdentifier {
    MangledIdentifier {
        mangled: identifier.raw().to_string(),
        source: IdentifierKind::Identifier(identifier),
    }
}

pub fn mangle_item(path: ItemPath) -> MangledIdentifier {
    let mangled_module_path = &mangle_module(path.module.clone()).mangled;
    let mangled_identifier = path.item.raw();

    let mangled = format!("{mangled_module_path}__$__{mangled_identifier}");

    MangledIdentifier {
        mangled,
        source: IdentifierKind::ItemPath(path),
    }
}

pub fn mangle_field(field: FieldPath) -> MangledIdentifier {
    let mangled_struct_ = mangle_item(field.struct_.clone());
    let mangled_item = field.field.raw();

    let mangled = format!("{}__$__{}", mangled_struct_.as_str(), mangled_item);

    MangledIdentifier {
        mangled,
        source: IdentifierKind::FieldPath(field),
    }
}
