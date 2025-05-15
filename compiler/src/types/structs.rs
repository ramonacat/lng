use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use super::{
    Expression, FunctionId, Identifier, InstantiatedType, TypeArgumentValues,
    interfaces::InterfaceId, modules::ModuleId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDescriptorType {
    pub id: StructId,
    // the fields are a Vec<_>, so that the order is well defined
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub struct_id: StructId,
    pub name: Identifier,
    pub type_: InstantiatedType,
    pub static_: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructId {
    InModule(ModuleId, Identifier),
    // TODO a dynamically generated one can also be used here
}

impl Display for StructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InModule(module_id, fqname) => write!(f, "struct({module_id}, {fqname})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedStructId(StructId, TypeArgumentValues);

impl Display for InstantiatedStructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl InstantiatedStructId {
    pub(crate) const fn id(&self) -> StructId {
        self.0
    }

    pub(crate) const fn argument_values(&self) -> &TypeArgumentValues {
        &self.1
    }

    pub(crate) const fn new(id: StructId, tav: TypeArgumentValues) -> Self {
        Self(id, tav)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: StructId,
    pub fields: Vec<StructField>,
    pub impls: Vec<FunctionId>,
    pub type_: InstantiatedType,
    pub instance_type: InstantiatedType,
    pub implemented_interfaces: HashMap<InterfaceId, HashMap<Identifier, FunctionId>>,
}

impl Struct {
    pub(crate) fn field_type(&self, field_name: Identifier) -> InstantiatedType {
        self.fields
            .iter()
            .find(|x| x.name == field_name)
            .unwrap()
            .type_
            .clone()
    }

    pub(crate) fn implements(&self, interface_id: InterfaceId) -> bool {
        self.implemented_interfaces.contains_key(&interface_id)
    }

    pub(crate) fn instance_type(&self) -> InstantiatedType {
        self.instance_type.clone()
    }

    pub(crate) fn with_type_arguments(&self, argument_values: &TypeArgumentValues) -> Self {
        Self {
            id: self.id,
            fields: self.fields.clone(),
            impls: self.impls.clone(),
            type_: self.type_.with_type_arguments(argument_values),
            instance_type: self.instance_type.with_type_arguments(argument_values),
            implemented_interfaces: self.implemented_interfaces.clone(),
        }
    }
}
