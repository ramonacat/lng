use std::collections::HashMap;

use crate::identifier::Identifier;

use super::{InstantiatedType, functions::Argument, modules::ModuleId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InterfaceId {
    InModule(ModuleId, Identifier),
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
