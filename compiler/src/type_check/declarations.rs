use std::collections::HashMap;

use crate::{
    ast,
    identifier::Identifier,
    types::{self, interfaces::InstantiatedInterfaceId, structs::InstantiatedStructId},
};

use super::errors::TypeCheckError;

#[derive(Debug, Clone)]
pub(super) struct DeclaredFunction {
    pub(super) id: types::functions::FunctionId,
    pub(super) type_id: types::store::TypeId,
    pub(super) module_name: types::modules::ModuleId,
    pub(super) arguments: Vec<types::functions::Argument>,
    pub(super) return_type: types::store::TypeId,
    pub(super) ast: ast::Function,
    pub(super) position: ast::SourceSpan,
    pub(super) visibility: types::Visibility,
}

#[derive(Debug, Clone)]
pub(super) struct DeclaredStructField {
    pub(super) type_: types::store::TypeId,
    pub(super) static_: bool,
}

pub(super) struct DeclaredRootModule {
    // TODO make the fields private
    pub(super) structs: HashMap<types::structs::StructId, types::structs::Struct>,
    pub(super) functions: HashMap<types::functions::FunctionId, DeclaredFunction>,
    pub(super) predeclared_functions:
        HashMap<types::functions::FunctionId, types::functions::Function>,
    pub(super) modules: HashMap<types::modules::ModuleId, types::modules::Module>,
    pub(super) imports:
        HashMap<(types::modules::ModuleId, Identifier), (types::modules::ModuleId, Identifier)>,
    pub(super) interfaces: HashMap<types::interfaces::InterfaceId, types::interfaces::Interface>,
}

impl std::fmt::Debug for DeclaredRootModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "structs")?;

        for struct_ in self.structs.keys() {
            writeln!(f, "    {struct_}")?;
        }

        writeln!(f, "functions")?;

        for function in self.functions.keys() {
            writeln!(f, "    {function}")?;
        }

        writeln!(f, "predeclared_functions")?;

        for function in self.predeclared_functions.keys() {
            writeln!(f, "    {function}")?;
        }

        Ok(())
    }
}

pub enum DeclaredItemKind<'item> {
    Struct(&'item types::structs::Struct),
    Function(&'item DeclaredFunction),
    PredeclaredFunction(&'item types::functions::Function),
    Interface(&'item types::interfaces::Interface),
}

impl DeclaredItemKind<'_> {
    // TODO can we get rid of this?
    pub(crate) fn type_(&self, types: &dyn types::store::TypeStore) -> types::Type {
        match self {
            // TODO handle generics
            Self::Struct(struct_) => types::Type::new_generic(
                types::TypeKind::Object(InstantiatedStructId::new(
                    struct_.id,
                    types::generics::TypeArguments::new_empty(),
                )),
                types.get(struct_.type_id).arguments().clone(),
            ),
            Self::Function(declared_function) => {
                types::Type::new(types::TypeKind::Callable(declared_function.id))
            }
            Self::PredeclaredFunction(function) => types::Type::new_generic(
                types::TypeKind::Callable(function.id),
                types.get(function.type_id).arguments().clone(),
            ),
            Self::Interface(interface) => types::Type::new_generic(
                // TODO handle generic interfaces here
                types::TypeKind::InterfaceObject(InstantiatedInterfaceId::new(
                    interface.id,
                    types::generics::TypeArguments::new_empty(),
                )),
                types.get(interface.type_id).arguments().clone(),
            ),
        }
    }
}

impl DeclaredRootModule {
    pub(crate) fn new() -> Self {
        Self {
            structs: HashMap::new(),
            functions: HashMap::new(),

            predeclared_functions: HashMap::new(),
            modules: HashMap::new(),
            imports: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }

    pub(crate) fn from_predeclared(
        modules: &HashMap<types::modules::ModuleId, types::modules::Module>,
        structs: HashMap<types::structs::StructId, types::structs::Struct>,
        functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    ) -> Self {
        Self {
            structs,
            functions: HashMap::new(),
            predeclared_functions: functions,
            modules: modules.clone(),
            imports: HashMap::new(),
            interfaces: HashMap::new(),
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

    pub fn get_item_id(
        &self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> Option<types::store::TypeId> {
        self.functions
            .get(&types::functions::FunctionId::InModule(module_id, name))
            .map(|x| x.type_id)
            .or_else(|| {
                self.predeclared_functions
                    .get(&types::functions::FunctionId::InModule(module_id, name))
                    .map(|x| x.type_id)
            })
            .or_else(|| {
                self.structs
                    .get(&types::structs::StructId::InModule(module_id, name))
                    .map(|x| x.type_id)
            })
            .or_else(|| {
                self.interfaces
                    .get(&types::interfaces::InterfaceId::InModule(module_id, name))
                    .map(|x| x.type_id)
            })
    }

    pub fn get_item(
        &self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> Option<DeclaredItemKind> {
        self.functions
            .get(&types::functions::FunctionId::InModule(module_id, name))
            .map(DeclaredItemKind::Function)
            .or_else(|| {
                self.predeclared_functions
                    .get(&types::functions::FunctionId::InModule(module_id, name))
                    .map(DeclaredItemKind::PredeclaredFunction)
            })
            .or_else(|| {
                self.structs
                    .get(&types::structs::StructId::InModule(module_id, name))
                    .map(DeclaredItemKind::Struct)
            })
            .or_else(|| {
                self.interfaces
                    .get(&types::interfaces::InterfaceId::InModule(module_id, name))
                    .map(DeclaredItemKind::Interface)
            })
    }
}

// TODO return TypeId instead of Type?
pub(super) fn resolve_type(
    root_module: &DeclaredRootModule,
    current_module: types::modules::ModuleId,
    r#type: &ast::TypeDescription,
    types: &dyn types::store::TypeStore,
) -> Result<types::Type, TypeCheckError> {
    // TODO handle generics here
    match r#type {
        ast::TypeDescription::Array(type_description) => {
            Ok(types::Type::new(types::TypeKind::Array(Box::new(
                resolve_type(root_module, current_module, type_description, types)?,
            ))))
        }
        ast::TypeDescription::Named(name) if name == "()" => Ok(types::Type::unit()),
        ast::TypeDescription::Named(name) if name == "u64" => Ok(types::Type::u64()),
        ast::TypeDescription::Named(name) => {
            let (current_module, name) = root_module.resolve_import(current_module, *name);

            let item = root_module.get_item(current_module, name).unwrap();

            Ok(item.type_(types))
        }
    }
}
