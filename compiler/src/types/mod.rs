pub mod functions;
pub mod generics;
pub mod interfaces;
pub mod modules;
pub mod store;
pub mod structs;

use crate::types::generics::TypeArgument;
use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
};

use functions::{FunctionId, InstantiatedFunctionId};
use generics::TypeArguments;
use interfaces::InstantiatedInterfaceId;
use interfaces::InterfaceId;
use modules::ModuleId;
use store::TypeId;
use structs::{FieldValue, InstantiatedStructId, Struct, StructId};
use thiserror::Error;

use crate::{ast, identifier::Identifier};

#[derive(Debug, Error)]
pub enum TypeError {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Unit,
    Object(InstantiatedStructId),
    Array(Box<Type>),
    // TODO this should be an object with special properties
    Callable(FunctionId),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<Type>),
    Struct(InstantiatedStructId),
    Function(InstantiatedFunctionId),
    IndirectCallable(InstantiatedInterfaceId, Identifier),
    // TODO this should be the same as object
    InterfaceObject(InstantiatedInterfaceId),
    // TODO make this TypeId
    Generic(TypeArgument),
    Interface(InterfaceId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
    arguments: TypeArguments,
}

impl Type {
    pub(crate) const fn new(kind: TypeKind) -> Self {
        Self {
            kind,
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) const fn new_generic(kind: TypeKind, type_arguments: TypeArguments) -> Self {
        Self {
            kind,
            arguments: type_arguments,
        }
    }

    pub(crate) const fn unit() -> Self {
        Self::new(TypeKind::Unit)
    }

    pub(crate) const fn u8() -> Self {
        Self::new(TypeKind::U8)
    }

    pub(crate) const fn u64() -> Self {
        Self::new(TypeKind::U64)
    }

    pub(crate) const fn kind(&self) -> &TypeKind {
        &self.kind
    }

    fn with_type_arguments(&self, argument_values: Vec<TypeId>) -> Self {
        Self {
            kind: self.kind.clone(),
            arguments: self.arguments.with_values(argument_values),
        }
    }

    pub(crate) const fn arguments(&self) -> &TypeArguments {
        &self.arguments
    }

    pub(crate) fn can_assign_to(
        &self,
        type_: &Self,
        lookup_struct: impl Fn(StructId) -> Option<Struct>,
    ) -> bool {
        match (&self.kind, &type_.kind) {
            (TypeKind::Object(l_name), TypeKind::Object(r_name)) => l_name == r_name,
            (TypeKind::InterfaceObject(l_id), TypeKind::InterfaceObject(r_id)) => l_id == r_id,
            (TypeKind::InterfaceObject(l_id), TypeKind::Object(r_name)) => {
                let struct_ = lookup_struct(r_name.id()).unwrap();

                // TODO check if it implements the interface with the right type args
                struct_.implements(l_id.id())
            }
            _ => todo!("\n{:?}\n{:?}", &self.kind, &type_.kind),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unit => write!(f, "()"),
            TypeKind::Object(instantiated_struct_id) => write!(
                f,
                "{}<{}>",
                instantiated_struct_id.id(),
                instantiated_struct_id.arguments()
            ),
            TypeKind::Array(element_type) => write!(f, "{element_type}[]"),
            TypeKind::Callable(function_id) => write!(f, "callable<{function_id}>"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::Pointer(instantiated_type) => write!(f, "{instantiated_type}*"),
            TypeKind::Struct(struct_id) => write!(f, "{struct_id}"),
            TypeKind::Function(function_id) => write!(f, "{function_id}"),
            TypeKind::IndirectCallable(_, _) => todo!(),
            TypeKind::InterfaceObject { .. } => todo!(),
            TypeKind::Generic(_) => todo!(),
            TypeKind::Interface(_) => todo!(),
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
    pub type_: Type,
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
