use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use super::{FQName, Function, FunctionId, Identifier, Type, TypeArgumentValues, TypeArguments};

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
    FQName(FQName),
    // TODO a dynamically generated one can also be used here
}

impl Display for StructId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FQName(fqname) => write!(f, "struct({fqname})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: FQName,
    #[allow(unused)] // TODO this will be needed once we have a syntax for type instantiation
    pub type_arguments: TypeArguments,
    pub fields: Vec<StructField>,
    // TODO eventually change to just a vec of FunctionIds
    pub impls: HashMap<FunctionId, Function>,
}

// TODO the Hash, Eq, PartialEq implementations are questionable, get rid of them (but for that we
// need to have some globally identifiable id for structs (and FQName isn't it, because that won't
// handle runtime-defined ones))
impl std::hash::Hash for Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Eq for Struct {}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Struct {
    pub(crate) fn instantiate(&self, _type_argument_values: &TypeArgumentValues) -> Self {
        // TODO support actually instantiating the impls and fields, but this can't be done yet, as the
        // self argument will lead to infinite recursion the way things are handled rn
        let fields = self.fields.clone();
        // .map(|f| f.instantiate(type_argument_values, object_lookup))

        let impls = self.impls.clone();
        //.map(|(id, impl_)| (*id, impl_.instantiate(type_argument_values, object_lookup)))

        Self {
            name: self.name,
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
