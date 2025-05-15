use std::fmt::{Display, Formatter};

use crate::{ast, identifier::Identifier};

use super::{
    InstantiatedType, Statement, TypeArgumentValues, Visibility, modules::ModuleId,
    structs::StructId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedFunctionId(FunctionId, TypeArgumentValues);

impl Display for InstantiatedFunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl InstantiatedFunctionId {
    pub(crate) const fn id(&self) -> FunctionId {
        self.0
    }

    pub(crate) const fn argument_values(&self) -> &TypeArgumentValues {
        &self.1
    }

    pub(crate) const fn new(id: FunctionId, tav: TypeArgumentValues) -> Self {
        Self(id, tav)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionId {
    InModule(ModuleId, Identifier),
    InStruct(StructId, Identifier),
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InModule(module_id, identifier) => {
                write!(f, "InModule({module_id}, {identifier})")
            }
            Self::InStruct(struct_id, identifier) => {
                write!(f, "InStruct({struct_id}, {identifier})")
            }
        }
    }
}

impl FunctionId {
    pub(crate) const fn local(&self) -> Identifier {
        match self {
            Self::InModule(_, identifier) | Self::InStruct(_, identifier) => *identifier,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub module_name: ModuleId,
    pub arguments: Vec<Argument>,
    pub return_type: InstantiatedType,
    pub body: FunctionBody,
    pub position: ast::SourceSpan,
    pub visibility: Visibility,
    pub type_: InstantiatedType,
}

impl Function {
    pub(crate) fn type_(&self) -> InstantiatedType {
        self.type_.clone()
    }

    pub(crate) fn with_type_arguments(&self, argument_values: &TypeArgumentValues) -> Self {
        Self {
            id: self.id,
            module_name: self.module_name,
            arguments: self.arguments.clone(),
            return_type: self.return_type.clone(),
            body: self.body.clone(),
            position: self.position,
            visibility: self.visibility,
            type_: self.type_.with_type_arguments(argument_values),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Extern(Identifier),
    Statements(Vec<Statement>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Argument {
    pub name: Identifier,
    pub type_: InstantiatedType,
    pub position: ast::SourceSpan,
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}
