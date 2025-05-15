use std::collections::HashMap;

use crate::identifier::Identifier;

use super::{Type, functions::Argument, generics::TypeArguments, modules::ModuleId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InterfaceId {
    InModule(ModuleId, Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedInterfaceId(InterfaceId, TypeArguments);
impl InstantiatedInterfaceId {
    pub(crate) const fn new(id: InterfaceId, tav: TypeArguments) -> Self {
        Self(id, tav)
    }

    pub(crate) const fn id(&self) -> InterfaceId {
        self.0
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub arguments: Vec<Argument>,
}

#[derive(Clone)]
pub struct Interface {
    pub id: InterfaceId,
    pub type_: Type,
    pub functions: HashMap<Identifier, FunctionDeclaration>,
}
