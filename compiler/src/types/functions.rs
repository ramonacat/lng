use std::fmt::{Display, Formatter};

use crate::{
    ast,
    identifier::Identifier,
    name_mangler::{MangledIdentifier, mangle_item_name, mangle_struct_item_name},
};

use super::{
    AnyType, GenericType, InstantiatedType, Statement, TypeArgumentValues, Visibility,
    modules::ModuleId, structs::StructId,
};

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
    pub(crate) fn into_mangled(self, tav: &TypeArgumentValues) -> MangledIdentifier {
        match self {
            Self::InModule(module_id, fqname) => mangle_item_name(module_id, fqname, tav),
            Self::InStruct(struct_id, identifier) => {
                mangle_struct_item_name(struct_id, identifier, tav)
            }
        }
    }

    pub(crate) const fn local(&self) -> Identifier {
        match self {
            Self::InModule(_, identifier) | Self::InStruct(_, identifier) => *identifier,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function<T: AnyType> {
    pub id: FunctionId,
    pub module_name: ModuleId,
    pub arguments: Vec<Argument<T>>,
    pub return_type: T,
    pub body: FunctionBody<T>,
    pub position: ast::SourceSpan,
    pub visibility: Visibility,
    pub type_: T,
}

impl<T: AnyType> Function<T> {
    pub(crate) fn type_(&self) -> T {
        self.type_.clone()
    }
}

impl Function<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
    ) -> Function<InstantiatedType> {
        Function {
            id: self.id,
            module_name: self.module_name,
            arguments: self
                .arguments
                .iter()
                .map(|x| x.instantiate(type_argument_values))
                .collect(),
            return_type: self.return_type.instantiate(type_argument_values),
            body: self.body.instantiate(type_argument_values),
            position: self.position,
            visibility: self.visibility,
            type_: self.type_.instantiate(type_argument_values),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody<T: AnyType> {
    Extern(Identifier),
    Statements(Vec<Statement<T>>),
}

impl FunctionBody<GenericType> {
    fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
    ) -> FunctionBody<InstantiatedType> {
        match self {
            Self::Extern(identifier) => FunctionBody::Extern(*identifier),
            Self::Statements(statements) => FunctionBody::Statements(
                statements
                    .iter()
                    .map(|x| x.instantiate(type_argument_values))
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Argument<T: AnyType> {
    pub name: Identifier,
    pub type_: T,
    pub position: ast::SourceSpan,
}

impl Argument<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues,
    ) -> Argument<InstantiatedType> {
        Argument {
            name: self.name,
            type_: self.type_.instantiate(type_argument_values),
            position: self.position,
        }
    }
}

impl<T: AnyType> Display for Argument<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}
