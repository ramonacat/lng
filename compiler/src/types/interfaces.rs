use std::collections::HashMap;

use crate::identifier::Identifier;

use super::{AnyType, functions::Argument, modules::ModuleId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InterfaceId {
    InModule(ModuleId, Identifier),
}

#[derive(Clone)]
pub struct FunctionDeclaration<T: AnyType> {
    pub return_type: T,
    pub arguments: Vec<Argument<T>>,
}

#[derive(Clone)]
pub struct Interface<T: AnyType> {
    pub id: InterfaceId,
    pub type_: T,
    pub functions: HashMap<Identifier, FunctionDeclaration<T>>,
}
