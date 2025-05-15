use std::fmt::{Display, Formatter};

use crate::{ast, identifier::Identifier};

use super::{
    Statement, Visibility,
    generics::TypeArguments,
    modules::ModuleId,
    store::{TypeId, TypeStore},
    structs::StructId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedFunctionId(FunctionId, TypeArguments);

impl Display for InstantiatedFunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl InstantiatedFunctionId {
    pub(crate) const fn id(&self) -> FunctionId {
        self.0
    }

    pub(crate) fn argument_values(&self) -> Vec<Option<TypeId>> {
        self.1.values()
    }

    pub(crate) const fn new(id: FunctionId, tav: TypeArguments) -> Self {
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
    pub return_type: TypeId,
    pub body: FunctionBody,
    pub position: ast::SourceSpan,
    pub visibility: Visibility,
    pub type_id: TypeId,
}

impl Function {
    pub(crate) fn with_type_arguments(
        &self,
        argument_values: Vec<TypeId>,
        types: &mut dyn TypeStore,
    ) -> Self {
        Self {
            id: self.id,
            module_name: self.module_name,
            arguments: self.arguments.clone(),
            return_type: self.return_type,
            body: self.body.clone(),
            position: self.position,
            visibility: self.visibility,
            type_id: types.add(types.get(self.type_id).with_type_arguments(argument_values)),
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
    pub type_id: TypeId,
    pub position: ast::SourceSpan,
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_id)
    }
}
