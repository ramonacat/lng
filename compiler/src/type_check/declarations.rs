use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast,
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

// TODO can we kill the RefCells and let the borrowck do the borrowck?
pub(super) struct DeclaredRootModule {
    // TODO make the fields private
    pub(super) structs: RefCell<HashMap<types::structs::StructId, types::structs::Struct>>,
    pub(super) functions: RefCell<HashMap<types::functions::FunctionId, DeclaredFunction>>,
    pub(super) predeclared_functions:
        RefCell<HashMap<types::functions::FunctionId, types::functions::Function>>,
    pub(super) modules: HashMap<types::modules::ModuleId, types::modules::Module>,
    pub(super) imports:
        HashMap<(types::modules::ModuleId, Identifier), (types::modules::ModuleId, Identifier)>,
}

impl std::fmt::Debug for DeclaredRootModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "structs")?;

        for struct_ in self.structs.borrow().keys() {
            writeln!(f, "    {struct_}")?;
        }

        writeln!(f, "functions")?;

        for function in self.functions.borrow().keys() {
            writeln!(f, "    {function}")?;
        }

        writeln!(f, "predeclared_functions")?;

        for function in self.predeclared_functions.borrow().keys() {
            writeln!(f, "    {function}")?;
        }

        Ok(())
    }
}

// TODO this should take refs instead of clones, but we need to get rid of RefCells in
// DeclaredRootModule first
pub enum ItemKind {
    Struct(types::structs::Struct),
    Function(DeclaredFunction),
    PredeclaredFunction(types::functions::Function),
}
impl ItemKind {
    pub(crate) fn type_(&self) -> types::Type {
        match self {
            // TODO handle generics
            Self::Struct(struct_) => types::Type::new_not_generic(types::TypeKind::Object {
                type_name: types::structs::InstantiatedStructId(
                    struct_.id,
                    types::TypeArgumentValues::new_empty(),
                ),
            }),
            Self::Function(declared_function) => {
                types::Type::new_not_generic(types::TypeKind::Callable(declared_function.id))
            }
            Self::PredeclaredFunction(function) => {
                types::Type::new_not_generic(types::TypeKind::Callable(function.id))
            }
        }
    }
}

impl DeclaredRootModule {
    pub(crate) fn new() -> Self {
        Self {
            structs: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
            predeclared_functions: RefCell::new(HashMap::new()),
            modules: HashMap::new(),
            imports: HashMap::new(),
        }
    }

    pub(crate) fn from_predeclared(
        modules: &HashMap<types::modules::ModuleId, types::modules::Module>,
        structs: HashMap<types::structs::StructId, types::structs::Struct>,
        functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    ) -> Self {
        Self {
            structs: RefCell::new(structs),
            functions: RefCell::new(HashMap::new()),
            predeclared_functions: RefCell::new(functions),
            modules: modules.clone(),
            imports: HashMap::new(),
        }
    }

    pub(crate) fn declare_module(
        &mut self,
        module_path: types::modules::ModuleId,
        module: types::modules::Module,
    ) {
        let old = self.modules.insert(module_path, module);
        assert!(old.is_none());
    }

    pub(crate) fn import(
        &mut self,
        r#as: (types::modules::ModuleId, Identifier),
        item: (types::modules::ModuleId, Identifier),
    ) {
        let old = self.imports.insert(dbg!(r#as), dbg!(item));
        assert!(old.is_none());
    }

    pub fn resolve_import(
        &self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> (types::modules::ModuleId, Identifier) {
        self.imports
            .get(&(module_id, name))
            .copied()
            .unwrap_or((module_id, name))
    }

    pub fn get_item(
        &self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> Option<ItemKind> {
        self.functions
            .borrow()
            .get(&types::functions::FunctionId::InModule(module_id, name))
            .map(|function| ItemKind::Function(function.clone()))
            .or_else(|| {
                self.predeclared_functions
                    .borrow()
                    .get(&types::functions::FunctionId::InModule(module_id, name))
                    .map(|predeclared_function| {
                        ItemKind::PredeclaredFunction(predeclared_function.clone())
                    })
                    .or_else(|| {
                        self.structs
                            .borrow()
                            .get(&types::structs::StructId::InModule(module_id, name))
                            .map(|struct_| ItemKind::Struct(struct_.clone()))
                    })
            })
    }

    // TODO this should return a reference, once self.structs is not a RefCell
    pub(crate) fn get_struct(
        &self,
        struct_name: types::structs::StructId,
    ) -> Option<types::structs::Struct> {
        self.structs.borrow().get(&struct_name).cloned()
    }
}

pub(super) fn resolve_type(
    root_module: &DeclaredRootModule,
    current_module: types::modules::ModuleId,
    r#type: &ast::TypeDescription,
) -> Result<types::Type, TypeCheckError> {
    // TODO handle generics here
    match r#type {
        ast::TypeDescription::Array(type_description) => {
            Ok(types::Type::new_not_generic(types::TypeKind::Array {
                element_type: Box::new(resolve_type(
                    root_module,
                    current_module,
                    type_description,
                )?),
            }))
        }
        ast::TypeDescription::Named(name) if name == "()" => Ok(types::Type::unit()),
        ast::TypeDescription::Named(name) if name == "u64" => Ok(types::Type::u64()),
        ast::TypeDescription::Named(name) => {
            let (current_module, name) = root_module.resolve_import(current_module, *name);

            let item = root_module
                // TODO get_item should take a pair of (ModuleId, Identifier)
                .get_item(current_module, name)
                // TODO should there be a keyword for global scope access instead of this or_else?
                // TODO if this is a global, we should have the (ModuleId, Identifier) pair here!
                .or_else(|| {
                    let item_id = FQName::from_identifier(name);
                    root_module.get_item(
                        types::modules::ModuleId::FQName(item_id.without_last()),
                        item_id.last(),
                    )
                })
                .unwrap();

            Ok(item.type_())
        }
    }
}
