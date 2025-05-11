use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast,
    identifier::Identifier,
    types::{self, GenericType},
};

use super::errors::TypeCheckError;

#[derive(Debug, Clone)]
pub(super) struct DeclaredFunction {
    pub(super) id: types::functions::FunctionId,
    pub(super) module_name: types::modules::ModuleId,
    pub(super) arguments: Vec<types::functions::Argument<GenericType>>,
    pub(super) return_type: types::GenericType,
    pub(super) ast: ast::Function,
    pub(super) position: ast::SourceSpan,
    pub(super) visibility: types::Visibility,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredStructField {
    pub(super) type_: types::GenericType,
    pub(super) static_: bool,
}

// TODO can we kill the RefCells and let the borrowck do the borrowck?
pub(super) struct DeclaredRootModule {
    // TODO make the fields private
    pub(super) structs:
        RefCell<HashMap<types::structs::StructId, types::structs::Struct<types::GenericType>>>,
    pub(super) functions: RefCell<HashMap<types::functions::FunctionId, DeclaredFunction>>,
    pub(super) predeclared_functions: RefCell<
        HashMap<types::functions::FunctionId, types::functions::Function<types::GenericType>>,
    >,
    pub(super) modules: HashMap<types::modules::ModuleId, types::modules::Module>,
    pub(super) imports:
        HashMap<(types::modules::ModuleId, Identifier), (types::modules::ModuleId, Identifier)>,
    pub(super) interfaces: RefCell<
        HashMap<types::interfaces::InterfaceId, types::interfaces::Interface<types::GenericType>>,
    >,
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
    Struct(types::structs::Struct<GenericType>),
    Function(DeclaredFunction),
    PredeclaredFunction(types::functions::Function<GenericType>),
    Interface(types::interfaces::Interface<GenericType>),
}

impl ItemKind {
    pub(crate) fn type_(&self) -> types::GenericType {
        match self {
            // TODO handle generics
            Self::Struct(struct_) => types::GenericType::new(
                types::GenericTypeKind::StructObject {
                    type_name: struct_.id,
                },
                struct_.type_.arguments().clone(),
            ),
            Self::Function(declared_function) => types::GenericType::new(
                types::GenericTypeKind::Callable(declared_function.id),
                types::TypeArguments::new_empty(),
            ),
            Self::PredeclaredFunction(function) => types::GenericType::new(
                types::GenericTypeKind::Callable(function.id),
                function.type_.arguments().clone(),
            ),
            Self::Interface(interface) => types::GenericType::new(
                types::GenericTypeKind::InterfaceObject(interface.id),
                interface.type_.arguments().clone(),
            ),
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
            interfaces: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn from_predeclared(
        modules: &HashMap<types::modules::ModuleId, types::modules::Module>,
        structs: HashMap<types::structs::StructId, types::structs::Struct<GenericType>>,
        functions: HashMap<
            types::functions::FunctionId,
            types::functions::Function<types::GenericType>,
        >,
    ) -> Self {
        Self {
            structs: RefCell::new(structs),
            functions: RefCell::new(HashMap::new()),
            predeclared_functions: RefCell::new(functions),
            modules: modules.clone(),
            imports: HashMap::new(),
            interfaces: RefCell::new(HashMap::new()),
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
        let old = self.imports.insert(r#as, item);
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

    // TODO we should probably take the TypeArguments as an argument here or something?
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
            })
            .or_else(|| {
                self.structs
                    .borrow()
                    .get(&types::structs::StructId::InModule(module_id, name))
                    .map(|struct_| ItemKind::Struct(struct_.clone()))
            })
            .or_else(|| {
                self.interfaces
                    .borrow()
                    .get(&types::interfaces::InterfaceId::InModule(module_id, name))
                    .map(|interface| ItemKind::Interface(interface.clone()))
            })
    }
}

pub(super) fn resolve_type(
    root_module: &DeclaredRootModule,
    current_module: types::modules::ModuleId,
    r#type: &ast::TypeDescription,
) -> Result<types::GenericType, TypeCheckError> {
    // TODO handle generics here
    match r#type {
        ast::TypeDescription::Array(type_description) => Ok(types::GenericType::new(
            types::GenericTypeKind::Array {
                element_type: Box::new(resolve_type(
                    root_module,
                    current_module,
                    type_description,
                )?),
            },
            types::TypeArguments::new_empty(),
        )),
        ast::TypeDescription::Named(name) if name == "()" => Ok(types::GenericType::unit()),
        ast::TypeDescription::Named(name) if name == "u64" => Ok(types::GenericType::u64()),
        ast::TypeDescription::Named(name) => {
            let (current_module, name) = root_module.resolve_import(current_module, *name);

            let item = root_module.get_item(current_module, name).unwrap();

            Ok(item.type_())
        }
    }
}
