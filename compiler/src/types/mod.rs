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
use interfaces::InterfaceId;
use modules::ModuleId;
use structs::{FieldValue, InstantiatedStructId, Struct, StructId};
use thiserror::Error;

use crate::{ast, identifier::Identifier};

pub trait AnyType: Display + Clone + Debug + Hash + Eq + PartialEq {}
impl AnyType for InstantiatedType {}

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
    // TODO should be InstantiatedStructId
    Object {
        type_name: StructId,
        type_argument_values: TypeArgumentValues<InstantiatedType>,
    },
    Array {
        element_type: Box<InstantiatedType>,
    },
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
    IndirectCallable(InterfaceId, Identifier),
    InterfaceObject {
        // TODO should be InstantiatedInterfaceId
        interface_id: InterfaceId,
        type_argument_values: TypeArgumentValues<InstantiatedType>,
    },
    // TODO make this TypeId
    Generic(TypeArgument),
    Interface(InterfaceId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedType {
    kind: InstantiatedTypeKind,
    // TODO type_arguments and type_argument_values should be a single collection!
    type_arguments: TypeArguments,
    type_argument_values: TypeArgumentValues<InstantiatedType>,
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
        type_argument_values: TypeArgumentValues<Self>,
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

    fn with_type_arguments(&self, argument_values: &TypeArgumentValues<Self>) -> Self {
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
        lookup_struct: impl Fn(StructId) -> Option<Struct<Self>>,
    ) -> bool {
        match (&self.kind, &type_.kind) {
            (
                InstantiatedTypeKind::Object {
                    type_name: l_name,
                    type_argument_values: l_args,
                },
                InstantiatedTypeKind::Object {
                    type_name: r_name,
                    type_argument_values: r_args,
                },
            ) => l_name == r_name && l_args == r_args,
            (
                InstantiatedTypeKind::InterfaceObject {
                    interface_id: l_id,
                    type_argument_values: l_args,
                },
                InstantiatedTypeKind::InterfaceObject {
                    interface_id: r_id,
                    type_argument_values: r_args,
                },
            ) => l_id == r_id && l_args == r_args,
            (
                InstantiatedTypeKind::InterfaceObject {
                    interface_id: l_id,
                    type_argument_values: _,
                },
                InstantiatedTypeKind::Object {
                    type_name: r_name,
                    type_argument_values: _,
                },
            ) => {
                let struct_ = lookup_struct(*r_name).unwrap();

                // TODO check if it implements the interface with the right type args
                struct_.implements(*l_id)
            }
            _ => todo!("\n{:?}\n{:?}", &self.kind, &type_.kind),
        }
    }
}

impl Display for InstantiatedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            InstantiatedTypeKind::Unit => write!(f, "()"),
            InstantiatedTypeKind::Object {
                type_name,
                type_argument_values,
            } => write!(f, "{type_name}<{type_argument_values}>"),
            InstantiatedTypeKind::Array { element_type } => write!(f, "{element_type}[]"),
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
pub enum Statement<T: AnyType> {
    Expression(Expression<T>),
    Let(LetStatement<T>),
    Return(Expression<T>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    UnsignedInteger(u64),
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<T: AnyType> {
    Call {
        target: Box<Expression<T>>,
        arguments: Vec<Expression<T>>,
    },
    Literal(Literal),
    LocalVariableAccess(Identifier),
    GlobalVariableAccess(ModuleId, Identifier),
    StructConstructor(Box<Expression<T>>, Vec<FieldValue<T>>),
    FieldAccess {
        target: Box<Expression<T>>,
        field: Identifier,
    },
    SelfAccess,
}

#[derive(Debug, Clone)]
pub struct Expression<T: AnyType> {
    pub position: ast::SourceSpan,
    pub type_: T,
    pub kind: ExpressionKind<T>,
}

#[derive(Debug, Clone)]
pub struct LetStatement<T: AnyType> {
    pub binding: Identifier,
    pub value: Expression<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}
