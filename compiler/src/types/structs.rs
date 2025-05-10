use std::fmt::{Display, Formatter};

use crate::name_mangler::{MangledIdentifier, mangle_item_name};

use super::{
    AnyType, FunctionId, GenericType, Identifier, InstantiatedType, TypeArgumentValues,
    modules::ModuleId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDescriptorType<T: AnyType> {
    pub id: StructId,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField<T>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField<T: AnyType> {
    pub struct_id: StructId,
    pub name: Identifier,
    pub type_: T,
    pub static_: bool,
}

impl StructField<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
    ) -> StructField<InstantiatedType> {
        StructField {
            struct_id: self.struct_id,
            name: self.name,
            type_: self.type_.instantiate(type_argument_values),
            static_: self.static_,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructId {
    InModule(ModuleId, Identifier),
    // TODO a dynamically generated one can also be used here
}

impl StructId {
    pub(crate) fn into_mangled(self, tav: &TypeArgumentValues) -> MangledIdentifier {
        match self {
            Self::InModule(module_id, identifier) => mangle_item_name(module_id, identifier, tav),
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
pub struct Struct<T: AnyType> {
    pub id: StructId,
    pub fields: Vec<StructField<T>>,
    pub impls: Vec<FunctionId>,
    pub type_: T,
}

impl<T: AnyType> Struct<T> {
    pub(crate) fn field_type(&self, field_name: Identifier) -> T {
        self.fields
            .iter()
            .find(|x| x.name == field_name)
            .unwrap()
            .type_
            .clone()
    }
}

impl Struct<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
    ) -> Struct<InstantiatedType> {
        Struct {
            id: self.id,
            fields: self
                .fields
                .iter()
                .map(|x| x.instantiate(type_argument_values))
                .collect(),
            impls: self.impls.clone(),
            type_: self.type_.instantiate(type_argument_values),
        }
    }
}
