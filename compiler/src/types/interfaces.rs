use std::collections::HashMap;

use crate::identifier::Identifier;

use super::{
    InstantiatedType, functions::Argument, generics::TypeArgumentValues, modules::ModuleId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InterfaceId {
    InModule(ModuleId, Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedInterfaceId(InterfaceId, TypeArgumentValues);
impl InstantiatedInterfaceId {
    pub(crate) const fn new(id: InterfaceId, tav: TypeArgumentValues) -> Self {
        Self(id, tav)
    }

    pub(crate) const fn id(&self) -> InterfaceId {
        self.0
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub return_type: InstantiatedType,
    pub arguments: Vec<Argument>,
}

#[derive(Clone)]
pub struct Interface {
    pub id: InterfaceId,
    pub type_: InstantiatedType,
    pub functions: HashMap<Identifier, FunctionDeclaration>,
}
