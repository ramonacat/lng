use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast::{self, SourceSpan},
    identifier::{FQName, Identifier},
    types,
};

use super::errors::TypeCheckError;

#[derive(Debug, Clone)]
pub(super) struct DeclaredFunction {
    pub(super) id: types::functions::FunctionId,
    pub(super) module_name: types::modules::ModuleId,
    pub(super) arguments: Vec<types::functions::Argument>,
    pub(super) return_type: types::Type,
    pub(super) ast: ast::Function,
    pub(super) position: ast::SourceSpan,
    pub(super) visibility: types::Visibility,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredStructField {
    pub(super) type_: types::Type,
    pub(super) static_: bool,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredImport {
    pub(super) imported_item: FQName,
}

// TODO can we kill the RefCells and let the borrowck do the borrowck?
pub(super) struct DeclaredRootModule<'pre> {
    pub(super) structs: RefCell<HashMap<types::structs::StructId, types::structs::Struct>>,
    pub(super) functions: RefCell<HashMap<types::functions::FunctionId, DeclaredFunction>>,
    pub(super) module: DeclaredModule<'pre>,
    pub(super) predeclared_functions:
        RefCell<HashMap<types::functions::FunctionId, types::functions::Function>>,
}
impl<'pre> DeclaredRootModule<'pre> {
    pub(crate) fn new() -> Self {
        Self {
            structs: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
            module: DeclaredModule::new(),
            predeclared_functions: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn from_predeclared(
        original_module: &'pre types::modules::Module,
        structs: HashMap<types::structs::StructId, types::structs::Struct>,
        functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    ) -> Self {
        let mut module = DeclaredModule::new();
        module.import_predeclared(original_module);

        Self {
            structs: RefCell::new(structs),
            functions: RefCell::new(HashMap::new()),
            predeclared_functions: RefCell::new(functions),
            module,
        }
    }
}

#[derive(Clone)]
pub(super) struct DeclaredModule<'pre> {
    pub(super) items: HashMap<Identifier, DeclaredItem<'pre>>,
}

impl std::fmt::Debug for DeclaredModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut struct_ = f.debug_struct("DeclaredModule");

        for (item_name, item) in &self.items {
            struct_.field(&item_name.raw(), item);
        }

        struct_.finish()
    }
}

impl<'pre> DeclaredModule<'pre> {
    pub(super) fn import_predeclared(&mut self, module: &'pre types::modules::Module) {
        let root_name = FQName::parse("");

        for (item_name, item) in module.items() {
            let visibility = item.visibility;

            self.declare(
                root_name.with_part(item_name),
                DeclaredItem {
                    kind: DeclaredItemKind::Predeclared(item),
                    visibility,
                    position: item.position,
                },
            );
        }
    }

    pub(super) fn declare(&mut self, name: FQName, item: DeclaredItem<'pre>) {
        if name.len() == 1 {
            let old = self.items.insert(name.last(), item);
            assert!(old.is_none());
        } else {
            let (first, rest) = name.split_first();

            let found_module = self.items.get_mut(&first).unwrap();
            match &mut found_module.kind {
                DeclaredItemKind::Module(m) => {
                    m.declare(rest, item);
                }
                DeclaredItemKind::Predeclared(types::Item {
                    kind: types::ItemKind::Module(_),
                    visibility: _,
                    position: ast::SourceSpan::Internal,
                }) => {
                    // do nothing, the structure must've been already setup
                }
                _ => todo!(),
            }
        }
    }

    pub(super) fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    // TODO can we get rid of the clones here?
    pub(super) fn get_item(&self, name: FQName) -> Option<DeclaredItem> {
        if name.len() == 1 {
            return self.items.get(&name.last()).cloned();
        }

        let (first, rest) = name.split_first();

        match &self.items.get(&first).unwrap().kind {
            DeclaredItemKind::Module(m) => m.get_item(rest),
            DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Module(m),
                visibility,
                position: ast::SourceSpan::Internal,
            }) => Some(DeclaredItem {
                kind: DeclaredItemKind::Predeclared(m.get_item(rest).unwrap()),
                visibility: *visibility,
                position: ast::SourceSpan::Internal,
            }),
            _ => todo!(),
        }
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (&Identifier, &DeclaredItem)> {
        self.items.iter()
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeclaredItemKind<'pre> {
    Function(types::functions::FunctionId),
    Struct(types::structs::StructId),
    Import(DeclaredImport),
    Predeclared(&'pre types::Item),
    Module(DeclaredModule<'pre>),
}

#[derive(Clone)]
pub(super) struct DeclaredItem<'pre> {
    pub(super) kind: DeclaredItemKind<'pre>,
    pub(super) visibility: types::Visibility,
    pub(super) position: SourceSpan,
}

impl std::fmt::Debug for DeclaredItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DeclaredItemKind::Function(function_id) => {
                write!(f, "Function({function_id})")
            }
            DeclaredItemKind::Struct(struct_id) => {
                write!(f, "Struct({struct_id})")
            }
            DeclaredItemKind::Import(declared_import) => {
                write!(f, "Import({})", declared_import.imported_item)
            }
            DeclaredItemKind::Predeclared(item) => write!(f, "Checked({item:?})"),
            DeclaredItemKind::Module(declared_module) => write!(f, "Module({declared_module:?})"),
        }
    }
}

impl DeclaredItem<'_> {
    pub(super) fn type_(
        &self,
        root_module: &DeclaredModule,
        current_module: types::modules::ModuleId,
        error_location: ast::SourceSpan,
    ) -> Result<types::Type, TypeCheckError> {
        // TODO also handle the case of this item being generic
        match &self.kind {
            DeclaredItemKind::Function(function_id) => Ok(types::Type::new_not_generic(
                types::TypeKind::Callable(*function_id),
            )),
            DeclaredItemKind::Struct(struct_id) => {
                Ok(types::Type::new_not_generic(types::TypeKind::Object {
                    type_name: types::structs::InstantiatedStructId(
                        *struct_id,
                        types::TypeArgumentValues::new_empty(),
                    ),
                }))
            }
            DeclaredItemKind::Import(DeclaredImport { imported_item, .. }) => root_module
                .get_item(*imported_item)
                .unwrap()
                .type_(root_module, current_module, error_location),
            DeclaredItemKind::Predeclared(item) => {
                item.type_(root_module, current_module, error_location)
            }
            DeclaredItemKind::Module(_) => todo!(),
        }
    }
}

pub(super) fn resolve_type(
    root_module: &DeclaredModule,
    current_module: types::modules::ModuleId,
    r#type: &ast::TypeDescription,
    error_location: ast::SourceSpan,
) -> Result<types::Type, TypeCheckError> {
    // TODO handle generics here
    match r#type {
        ast::TypeDescription::Array(type_description) => {
            Ok(types::Type::new_not_generic(types::TypeKind::Array {
                element_type: Box::new(resolve_type(
                    root_module,
                    current_module,
                    type_description,
                    error_location,
                )?),
            }))
        }
        ast::TypeDescription::Named(name) if name == "()" => Ok(types::Type::unit()),
        ast::TypeDescription::Named(name) if name == "u64" => Ok(types::Type::u64()),
        ast::TypeDescription::Named(name) => {
            let item = root_module
                // TODO get_item should take a pair of (ModuleId, Identifier)
                .get_item(FQName::parse(&current_module.child(*name).to_string()))
                // TODO should there be a keyword for global scope access instead of this or_else?
                .or_else(|| root_module.get_item(FQName::from_identifier(*name)))
                .unwrap();

            match item.kind {
                DeclaredItemKind::Function(function_id) => Ok(types::Type::new_not_generic(
                    types::TypeKind::Callable(function_id),
                )),
                DeclaredItemKind::Struct(declared_struct) => {
                    Ok(types::Type::new_not_generic(types::TypeKind::Object {
                        type_name: types::structs::InstantiatedStructId(
                            declared_struct,
                            types::TypeArgumentValues::new_empty(),
                        ),
                    }))
                }
                DeclaredItemKind::Predeclared(item) => {
                    item.type_(root_module, current_module, error_location)
                }
                DeclaredItemKind::Import(declared_import) => {
                    let imported_item = root_module.get_item(declared_import.imported_item);

                    imported_item
                        .unwrap()
                        .type_(root_module, current_module, error_location)
                }
                DeclaredItemKind::Module(_) => todo!(),
            }
        }
    }
}
