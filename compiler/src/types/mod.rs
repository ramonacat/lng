pub mod functions;
pub mod generics;
pub mod interfaces;
pub mod modules;
pub mod structs;

use crate::types::generics::TypeArgument;
use crate::types::generics::TypeArgumentValues;
use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
};

use functions::{FunctionId, InstantiatedFunctionId};
use generics::TypeArguments;
use interfaces::InstantiatedInterfaceId;
use interfaces::InterfaceId;
use modules::ModuleId;
use structs::{FieldValue, InstantiatedStructId, Struct, StructId};
use thiserror::Error;

use crate::{ast, identifier::Identifier};

#[derive(Debug, Error)]
pub enum TypeError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemId {
    Struct(StructId),
    Function(FunctionId),
    Module(ModuleId),
}

impl Display for ItemId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(struct_id) => write!(f, "{struct_id}"),
            Self::Function(function_id) => write!(f, "{function_id}"),
            Self::Module(module_id) => write!(f, "{module_id}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstantiatedTypeKind {
    Unit,
    Object(InstantiatedStructId),
    Array(Box<InstantiatedType>),
    // TODO this should be an object with special properties
    Callable(FunctionId),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<InstantiatedType>),
    Struct(InstantiatedStructId),
    Function(InstantiatedFunctionId),
    IndirectCallable(InstantiatedInterfaceId, Identifier),
    InterfaceObject(InstantiatedInterfaceId),
    // TODO make this TypeId
    Generic(TypeArgument),
    Interface(InterfaceId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedType {
    kind: InstantiatedTypeKind,
    // TODO type_arguments and type_argument_values should be a single collection!
    type_arguments: TypeArguments,
    type_argument_values: TypeArgumentValues,
}

impl InstantiatedType {
    pub(crate) fn new(kind: InstantiatedTypeKind) -> Self {
        Self {
            kind,
            type_arguments: TypeArguments::new_empty(),
            type_argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn unit() -> Self {
        Self::new(InstantiatedTypeKind::Unit)
    }

    pub(crate) fn u8() -> Self {
        Self::new(InstantiatedTypeKind::U8)
    }

    pub(crate) fn u64() -> Self {
        Self::new(InstantiatedTypeKind::U64)
    }

    pub(crate) const fn new_generic(
        kind: InstantiatedTypeKind,
        type_arguments: TypeArguments,
        type_argument_values: TypeArgumentValues,
    ) -> Self {
        Self {
            kind,
            type_arguments,
            type_argument_values,
        }
    }

    pub(crate) const fn kind(&self) -> &InstantiatedTypeKind {
        &self.kind
    }

    fn with_type_arguments(&self, argument_values: &TypeArgumentValues) -> Self {
        let type_argument_values = self.type_argument_values.clone();
        Self {
            kind: self.kind.clone(),
            type_arguments: self.type_arguments.clone(),
            type_argument_values: type_argument_values.merge(argument_values.clone()),
        }
    }

    pub(crate) const fn arguments(&self) -> &TypeArguments {
        &self.type_arguments
    }

    pub(crate) fn can_assign_to(
        &self,
        type_: &Self,
        lookup_struct: impl Fn(StructId) -> Option<Struct>,
    ) -> bool {
        match (&self.kind, &type_.kind) {
            (InstantiatedTypeKind::Object(l_name), InstantiatedTypeKind::Object(r_name)) => {
                l_name == r_name
            }
            (
                InstantiatedTypeKind::InterfaceObject(l_id),
                InstantiatedTypeKind::InterfaceObject(r_id),
            ) => l_id == r_id,
            (InstantiatedTypeKind::InterfaceObject(l_id), InstantiatedTypeKind::Object(r_name)) => {
                let struct_ = lookup_struct(r_name.id()).unwrap();

                // TODO check if it implements the interface with the right type args
                struct_.implements(l_id.id())
            }
            _ => todo!("\n{:?}\n{:?}", &self.kind, &type_.kind),
        }
    }
}

impl Display for InstantiatedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            InstantiatedTypeKind::Unit => write!(f, "()"),
            InstantiatedTypeKind::Object(instantiated_struct_id) => write!(
                f,
                "{}<{}>",
                instantiated_struct_id.id(),
                instantiated_struct_id.argument_values()
            ),
            InstantiatedTypeKind::Array(element_type) => write!(f, "{element_type}[]"),
            InstantiatedTypeKind::Callable(function_id) => write!(f, "callable<{function_id}>"),
            InstantiatedTypeKind::U64 => write!(f, "u64"),
            InstantiatedTypeKind::U8 => write!(f, "u8"),
            InstantiatedTypeKind::Pointer(instantiated_type) => write!(f, "{instantiated_type}*"),
            InstantiatedTypeKind::Struct(struct_id) => write!(f, "{struct_id}"),
            InstantiatedTypeKind::Function(function_id) => write!(f, "{function_id}"),
            InstantiatedTypeKind::IndirectCallable(_, _) => todo!(),
            InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
            InstantiatedTypeKind::Generic(_) => todo!(),
            InstantiatedTypeKind::Interface(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    UnsignedInteger(u64),
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Call {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
    LocalVariableAccess(Identifier),
    GlobalVariableAccess(ModuleId, Identifier),
    StructConstructor(Box<Expression>, Vec<FieldValue>),
    FieldAccess {
        target: Box<Expression>,
        field: Identifier,
    },
    SelfAccess,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: ast::SourceSpan,
    pub type_: InstantiatedType,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub binding: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}
