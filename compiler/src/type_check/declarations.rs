use std::collections::HashMap;

use crate::{ast, std::TYPE_NAME_U64, types};

#[derive(Debug, Clone)]
pub(super) struct DeclaredArgument {
    pub(super) name: types::Identifier,
    pub(super) type_: ast::TypeDescription,
    pub(super) position: ast::SourceRange,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredFunctionDefinition {
    pub(super) arguments: Vec<DeclaredArgument>,
    pub(super) return_type: types::Type,
    pub(super) ast: ast::Function,
    pub(super) position: ast::SourceRange,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredFunction {
    pub(super) name: types::FQName,
    pub(super) definition: DeclaredFunctionDefinition,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredAssociatedFunction {
    pub(super) struct_: types::FQName,
    pub(super) name: types::Identifier,
    pub(super) definition: DeclaredFunctionDefinition,
    pub(super) visibility: types::Visibility,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredStructField {
    pub(super) type_: types::Type,
    pub(super) static_: bool,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredImport {
    pub(super) imported_item: types::FQName,
    pub(super) position: ast::SourceRange,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredStruct {
    pub(super) name: types::FQName,
    pub(super) fields: HashMap<types::Identifier, DeclaredStructField>,
}

#[derive(Clone)]
pub(super) struct DeclaredModule {
    pub(super) items: HashMap<types::Identifier, DeclaredItem>,
}

impl std::fmt::Debug for DeclaredModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut struct_ = f.debug_struct("DeclaredModule");

        for (item_name, item) in &self.items {
            struct_.field(&item_name.raw(), item);
        }

        struct_.finish()
    }
}

impl DeclaredModule {
    pub(super) fn import_predeclared(&mut self, module: &types::Module) {
        let root_name = types::FQName::parse("");

        for (item_name, item) in module.items() {
            let visibility = item.visibility;

            self.declare(
                root_name.with_part(item_name),
                DeclaredItem {
                    kind: DeclaredItemKind::Predeclared(item.clone()),
                    visibility,
                },
            );
        }
    }

    pub(super) fn declare(&mut self, name: types::FQName, item: DeclaredItem) {
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

    pub(super) fn get_item_mut(&mut self, name: types::FQName) -> Option<&mut DeclaredItem> {
        if name.len() == 1 {
            return self.items.get_mut(&name.last());
        }

        let (first, rest) = name.split_first();

        let DeclaredItem {
            kind: DeclaredItemKind::Module(m),
            visibility: _,
        } = self.items.get_mut(&first).unwrap()
        else {
            todo!();
        };

        m.get_item_mut(rest)
    }

    // TODO can we get rid of the clones here?
    pub(super) fn get_item(&self, name: types::FQName) -> Option<DeclaredItem> {
        if name.len() == 1 {
            return self.items.get(&name.last()).cloned();
        }

        let (first, rest) = name.split_first();

        match &self.items.get(&first).unwrap().kind {
            DeclaredItemKind::Module(m) => m.get_item(rest),
            DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Module(m),
                visibility,
            }) => Some(DeclaredItem {
                kind: DeclaredItemKind::Predeclared(m.get_item(rest).unwrap().clone()),
                visibility: *visibility,
            }),
            _ => todo!(),
        }
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (&types::Identifier, &DeclaredItem)> {
        self.items.iter()
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeclaredItemKind {
    Function(DeclaredFunction),
    Struct(DeclaredStruct),
    Import(DeclaredImport),
    // TODO make this a reference, so we don't have to clone?
    Predeclared(types::Item),
    Module(DeclaredModule),
}

#[derive(Clone)]
pub(super) struct DeclaredItem {
    pub(super) kind: DeclaredItemKind,
    pub(super) visibility: types::Visibility,
}

impl std::fmt::Debug for DeclaredItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DeclaredItemKind::Function(declared_function) => {
                write!(f, "Function({})", declared_function.name)
            }
            DeclaredItemKind::Struct(declared_struct) => {
                write!(f, "Struct({})", declared_struct.name)
            }
            DeclaredItemKind::Import(declared_import) => {
                write!(f, "Import({})", declared_import.imported_item)
            }
            DeclaredItemKind::Predeclared(item) => write!(f, "Checked({item:?})"),
            DeclaredItemKind::Module(declared_module) => write!(f, "Module({declared_module:?})"),
        }
    }
}

impl DeclaredItem {
    pub(super) fn type_(&self, root_module: &DeclaredModule) -> types::Type {
        match &self.kind {
            DeclaredItemKind::Function(declared_function) => types::Type::Callable {
                arguments: declared_function
                    .definition
                    .arguments
                    .iter()
                    .map(|declaration| types::Argument {
                        name: declaration.name,
                        type_: resolve_type(
                            root_module,
                            declared_function.name.without_last(),
                            &declaration.type_,
                        )
                        .instance_type(),
                        position: declaration.position,
                    })
                    .collect(),
                return_type: Box::new(declared_function.definition.return_type.clone()),
            },
            DeclaredItemKind::Struct(declared_struct) => {
                types::Type::StructDescriptor(types::StructDescriptorType {
                    name: declared_struct.name,
                    fields: declared_struct
                        .fields
                        .iter()
                        .map(|(field_name, declaration)| types::StructField {
                            struct_name: declared_struct.name,
                            name: *field_name,
                            type_: declaration.type_.clone(),
                            static_: declaration.static_,
                        })
                        .collect(),
                })
            }
            DeclaredItemKind::Import(DeclaredImport { imported_item, .. }) => root_module
                .get_item(*imported_item)
                .unwrap()
                .type_(root_module),
            DeclaredItemKind::Predeclared(item) => item.type_(root_module),
            DeclaredItemKind::Module(_) => todo!(),
        }
    }
}

pub(super) fn resolve_type(
    root_module: &DeclaredModule,
    current_module: types::FQName,
    r#type: &ast::TypeDescription,
) -> types::Type {
    match r#type {
        ast::TypeDescription::Array(type_description) => types::Type::Array(Box::new(
            resolve_type(root_module, current_module, type_description),
        )),
        ast::TypeDescription::Named(name) if name == "()" => todo!(),
        ast::TypeDescription::Named(name) if name == "u64" => root_module
            .get_item(*TYPE_NAME_U64)
            .unwrap()
            .type_(root_module),
        ast::TypeDescription::Named(name) => {
            let name = types::Identifier::parse(name);
            let item = root_module
                .get_item(current_module.with_part(name))
                // TODO should there be a keyword for global scope access instead of this or_else?
                .or_else(|| {
                    root_module.get_item(types::FQName::from_parts(std::iter::once(&name.raw())))
                })
                .unwrap();

            match item.kind {
                DeclaredItemKind::Function(declared_function) => types::Type::Callable {
                    arguments: declared_function
                        .definition
                        .arguments
                        .iter()
                        .map(|a| types::Argument {
                            name: a.name,
                            type_: resolve_type(
                                root_module,
                                declared_function.name.without_last(),
                                &a.type_,
                            )
                            .instance_type(),
                            position: a.position,
                        })
                        .collect(),
                    // TODO the type should probably also be resolve_type'd
                    return_type: Box::new(declared_function.definition.return_type),
                },
                DeclaredItemKind::Struct(declared_struct) => {
                    types::Type::StructDescriptor(types::StructDescriptorType {
                        name: declared_struct.name,
                        fields: declared_struct
                            .fields
                            .iter()
                            .map(|(name, field)| types::StructField {
                                struct_name: declared_struct.name,
                                name: *name,
                                // TODO field.type_ should also probably be resolve_type'd
                                type_: field.type_.clone(),
                                static_: field.static_,
                            })
                            .collect(),
                    })
                }
                DeclaredItemKind::Predeclared(item) => item.type_(root_module),
                DeclaredItemKind::Import(declared_import) => {
                    let imported_item = root_module.get_item(declared_import.imported_item);

                    imported_item.unwrap().type_(root_module)
                }
                DeclaredItemKind::Module(_) => todo!(),
            }
        }
    }
}
