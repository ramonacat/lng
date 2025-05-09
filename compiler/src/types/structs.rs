use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::name_mangler::{MangledIdentifier, mangle_item_name};

use super::{
    Function, FunctionId, Identifier, Type, TypeArgumentValues, TypeArguments, modules::ModuleId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDescriptorType {
    pub id: StructId,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedStructId(pub StructId, pub TypeArgumentValues);

impl Display for InstantiatedStructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub struct_id: StructId,
    pub name: Identifier,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructId {
    InModule(ModuleId, Identifier),
    // TODO a dynamically generated one can also be used here
}
impl StructId {
    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        match self {
            Self::InModule(module_id, identifier) => mangle_item_name(module_id, identifier),
        }
    }
}

impl Display for StructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InModule(module_id, fqname) => write!(f, "struct({module_id}, {fqname})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: StructId,
    #[allow(unused)] // TODO this will be needed once we have a syntax for type instantiation
    pub type_arguments: TypeArguments,
    pub fields: Vec<StructField>,
    // TODO eventually change to just a vec of FunctionIds
    pub impls: HashMap<FunctionId, Function>,
}

impl Struct {
    pub(crate) fn instantiate(&self, _type_argument_values: &TypeArgumentValues) -> Self {
        // TODO support actually instantiating the impls and fields, but this can't be done yet, as the
        // self argument will lead to infinite recursion the way things are handled rn
        let fields = self.fields.clone();
        let impls = self.impls.clone();

        Self {
            id: self.id,
            type_arguments: TypeArguments::new_empty(),
            fields,
            impls,
        }
    }

    pub(crate) fn field_type(&self, field_name: Identifier) -> Type {
        self.fields
            .iter()
            .find(|x| x.name == field_name)
            .unwrap()
            .type_
            .clone()
    }
}
