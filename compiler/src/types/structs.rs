use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use super::{
    Expression, FunctionId, Identifier,
    generics::TypeArguments,
    interfaces::InterfaceId,
    modules::ModuleId,
    store::{TypeId, TypeStore},
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
    pub type_: TypeId,
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
pub struct InstantiatedStructId(StructId, TypeArguments);

impl Display for InstantiatedStructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl InstantiatedStructId {
    pub(crate) const fn id(&self) -> StructId {
        self.0
    }

    pub(crate) fn argument_values(&self) -> Vec<Option<TypeId>> {
        self.1.values()
    }

    pub(crate) const fn arguments(&self) -> &TypeArguments {
        &self.1
    }

    pub(crate) const fn new(id: StructId, tav: TypeArguments) -> Self {
        Self(id, tav)
    }

    pub(crate) const fn new_no_generics(struct_id: StructId) -> Self {
        Self(struct_id, TypeArguments::new_empty())
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: StructId,
    pub fields: Vec<StructField>,
    pub impls: Vec<FunctionId>,
    pub type_id: TypeId,
    pub instance_type: TypeId,
    pub implemented_interfaces: HashMap<InterfaceId, HashMap<Identifier, FunctionId>>,
}

impl Struct {
    pub(crate) fn field_type(&self, field_name: Identifier) -> TypeId {
        self.fields
            .iter()
            .find(|x| x.name == field_name)
            .unwrap()
            .type_
    }

    pub(crate) fn implements(&self, interface_id: InterfaceId) -> bool {
        self.implemented_interfaces.contains_key(&interface_id)
    }

    pub(crate) const fn instance_type(&self) -> TypeId {
        self.instance_type
    }

    pub(crate) fn with_type_arguments(
        &self,
        argument_values: Vec<TypeId>,
        types: &mut dyn TypeStore,
    ) -> Self {
        Self {
            id: self.id,
            fields: self.fields.clone(),
            impls: self.impls.clone(),
            type_id: types.add(
                types
                    .get(self.type_id)
                    .with_type_arguments(argument_values.clone()),
            ),
            instance_type: types.add(
                types
                    .get(self.instance_type)
                    .with_type_arguments(argument_values),
            ),
            implemented_interfaces: self.implemented_interfaces.clone(),
        }
    }
}
