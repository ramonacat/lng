use itertools::Itertools;

use crate::types::{Identifier, ModulePath};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MangledIdentifier(String);
impl MangledIdentifier {
    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MangledModulePath(String);
impl MangledModulePath {
    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

pub fn mangle_module(module: &ModulePath) -> MangledModulePath {
    let mangled_module_path = module.parts().iter().map(Identifier::raw).join("$");

    MangledModulePath(mangled_module_path)
}

pub fn nomangle_item(identifier: &Identifier) -> MangledIdentifier {
    MangledIdentifier(identifier.raw().to_string())
}

pub fn mangle_item(module: &ModulePath, identifier: &Identifier) -> MangledIdentifier {
    let mangled_module_path = mangle_module(module);
    let mangled_identifier = identifier.raw();

    MangledIdentifier(format!(
        "{}__$__{}",
        mangled_module_path.0, mangled_identifier
    ))
}

pub fn mangle_field(
    module: &ModulePath,
    struct_name: &Identifier,
    item: &Identifier,
) -> MangledIdentifier {
    let mangled_module_path = mangle_module(module);
    let mangled_struct_name = struct_name.raw();
    let mangled_item = item.raw();

    MangledIdentifier(format!(
        "{}__$__{}__$__{}",
        mangled_module_path.0, mangled_struct_name, mangled_item
    ))
}
