pub mod functions;
pub mod generics;
pub mod interfaces;
pub mod modules;
pub mod structs;

use crate::types::generics::TypeArgument;
use crate::types::generics::TypeArgumentValues;
use crate::types::generics::TypeArguments;
use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
};

use functions::{FunctionId, InstantiatedFunctionId};
use interfaces::InterfaceId;
use modules::ModuleId;
use structs::{FieldValue, InstantiatedStructId, Struct, StructId};
use thiserror::Error;

use crate::{ast, identifier::Identifier};

pub trait AnyType: Display + Clone + Debug + Hash + Eq + PartialEq {}
impl AnyType for GenericType {}
impl AnyType for InstantiatedType {}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("missing type argument: {0}")]
    MissingTypeArgument(TypeArgument),
}

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
        interface_id: InterfaceId,
        type_argument_values: TypeArgumentValues<InstantiatedType>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedType {
    kind: InstantiatedTypeKind,
}

impl InstantiatedType {
    pub(crate) const fn new(kind: InstantiatedTypeKind) -> Self {
        Self { kind }
    }

    pub(crate) const fn kind(&self) -> &InstantiatedTypeKind {
        &self.kind
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericTypeKind {
    Generic(TypeArgument),
    Unit,
    StructObject { type_name: StructId },
    Array { element_type: Box<GenericType> },
    // TODO this should be an object with special properties
    Callable(FunctionId),
    IndirectCallable(InterfaceId, Identifier),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<GenericType>),
    Struct(StructId),
    Function(FunctionId),
    Interface(InterfaceId),
    InterfaceObject(InterfaceId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    kind: GenericTypeKind,
    type_arguments: TypeArguments,
    // Generic types can have values for type arguments, but are not required to have all of them
    // (or any at all), until they get to the final instantiation.
    type_argument_values: TypeArgumentValues<GenericType>,
}

impl Display for GenericType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let generic_type_kind = &self.kind;
        match generic_type_kind {
            GenericTypeKind::Generic(type_argument) => write!(f, "{type_argument}"),
            GenericTypeKind::Unit => write!(f, "()"),
            GenericTypeKind::StructObject { type_name } => write!(f, "{type_name}"),
            GenericTypeKind::Array { element_type } => write!(f, "{element_type}[]"),
            GenericTypeKind::Callable(function_id) => write!(f, "callable<{function_id}>"),
            GenericTypeKind::U64 => write!(f, "u64"),
            GenericTypeKind::U8 => write!(f, "u8"),
            GenericTypeKind::Pointer(generic_type) => write!(f, "{generic_type}*"),
            GenericTypeKind::Struct(struct_id) => write!(f, "{struct_id}"),
            GenericTypeKind::Function(function_id) => write!(f, "{function_id}"),
            GenericTypeKind::Interface(_) => todo!(),
            GenericTypeKind::InterfaceObject(_) => todo!(),
            GenericTypeKind::IndirectCallable(_, _) => todo!(),
        }
    }
}

impl GenericType {
    pub(crate) fn new(kind: GenericTypeKind, type_arguments: TypeArguments) -> Self {
        Self {
            kind,
            type_arguments,
            type_argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn u64() -> Self {
        Self::new(GenericTypeKind::U64, TypeArguments::new_empty())
    }

    pub(crate) fn u8() -> Self {
        Self::new(GenericTypeKind::U8, TypeArguments::new_empty())
    }

    pub(crate) fn unit() -> Self {
        Self::new(GenericTypeKind::Unit, TypeArguments::new_empty())
    }

    pub(crate) const fn kind(&self) -> &GenericTypeKind {
        &self.kind
    }

    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> Result<InstantiatedType, TypeError> {
        let type_argument_values = self
            .type_argument_values
            .instantiate(type_argument_values)?
            .merge(type_argument_values.clone());

        let kind: InstantiatedTypeKind = match &self.kind {
            GenericTypeKind::Generic(type_argument) => type_argument_values
                .get(*type_argument)
                .ok_or(TypeError::MissingTypeArgument(*type_argument))?
                .kind
                .clone(),
            GenericTypeKind::Unit => InstantiatedTypeKind::Unit,
            GenericTypeKind::StructObject { type_name } => InstantiatedTypeKind::Object {
                type_name: *type_name,
                type_argument_values,
            },
            GenericTypeKind::Array { element_type } => InstantiatedTypeKind::Array {
                element_type: Box::new(element_type.instantiate(&type_argument_values)?),
            },
            GenericTypeKind::Callable(function_id) => InstantiatedTypeKind::Callable(*function_id),
            GenericTypeKind::U64 => InstantiatedTypeKind::U64,
            GenericTypeKind::U8 => InstantiatedTypeKind::U8,
            GenericTypeKind::Pointer(pointee) => {
                InstantiatedTypeKind::Pointer(Box::new(pointee.instantiate(&type_argument_values)?))
            }
            GenericTypeKind::Struct(struct_id) => InstantiatedTypeKind::Struct(
                InstantiatedStructId::new(*struct_id, type_argument_values),
            ),
            GenericTypeKind::Function(function_id) => InstantiatedTypeKind::Function(
                InstantiatedFunctionId::new(*function_id, type_argument_values),
            ),
            GenericTypeKind::Interface(_) => todo!(),
            GenericTypeKind::InterfaceObject(interface_id) => {
                InstantiatedTypeKind::InterfaceObject {
                    interface_id: *interface_id,
                    type_argument_values,
                }
            }
            GenericTypeKind::IndirectCallable(interface_id, function_name) => {
                InstantiatedTypeKind::IndirectCallable(*interface_id, *function_name)
            }
        };

        Ok(InstantiatedType { kind })
    }

    pub(crate) const fn arguments(&self) -> &TypeArguments {
        &self.type_arguments
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn can_assign_to(
        &self,
        type_: &Self,
        lookup_struct: impl FnOnce(StructId) -> Option<Struct<Self>>,
    ) -> bool {
        match (self.kind(), type_.kind()) {
            (GenericTypeKind::Generic(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Unit) => todo!(),
            (
                GenericTypeKind::StructObject { type_name: left },
                GenericTypeKind::StructObject { type_name: right },
            ) => left == right,
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Array { .. }, GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Callable(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::U64, GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::U8, GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Pointer(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::StructObject { .. }) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Generic(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Unit) => todo!(),
            (
                GenericTypeKind::InterfaceObject(interface_id),
                GenericTypeKind::StructObject {
                    type_name: struct_id,
                },
            ) => {
                let struct_ = lookup_struct(*struct_id).unwrap();

                dbg!(&struct_);

                struct_.implements(*interface_id)
            }
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Array { .. }) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Callable(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Pointer(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Struct(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Function(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::InterfaceObject(_)) => todo!(),
            (GenericTypeKind::Generic(_), GenericTypeKind::IndirectCallable(_, _)) => todo!(),
            (GenericTypeKind::Unit, GenericTypeKind::IndirectCallable(_, _)) => todo!(),
            (GenericTypeKind::StructObject { .. }, GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::Array { .. }, GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::Callable(_), GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Generic(_)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Unit) => todo!(),
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::StructObject { .. }) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Array { .. }) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Callable(_)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::U64) => todo!(),
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::U8) => todo!(),
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Pointer(_)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Struct(_)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Function(_)) => {
                todo!()
            }
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::Interface(_)) => todo!(),
            (GenericTypeKind::IndirectCallable(_, _), GenericTypeKind::InterfaceObject(_)) => {
                todo!()
            }
            (GenericTypeKind::U64, GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::U8, GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::Pointer(_), GenericTypeKind::IndirectCallable(_, _)) => todo!(),
            (GenericTypeKind::Struct(_), GenericTypeKind::IndirectCallable(_, _)) => todo!(),
            (GenericTypeKind::Function(_), GenericTypeKind::IndirectCallable(_, _)) => todo!(),
            (GenericTypeKind::Interface(_), GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
            (GenericTypeKind::InterfaceObject(_), GenericTypeKind::IndirectCallable(_, _)) => {
                todo!()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement<T: AnyType> {
    Expression(Expression<T>),
    Let(LetStatement<T>),
    Return(Expression<T>),
}

impl Statement<GenericType> {
    fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> Result<Statement<InstantiatedType>, TypeError> {
        Ok(match self {
            Self::Expression(expression) => {
                Statement::Expression(expression.instantiate(type_argument_values)?)
            }
            Self::Let(let_statement) => {
                Statement::Let(let_statement.instantiate(type_argument_values)?)
            }
            Self::Return(expression) => {
                Statement::Return(expression.instantiate(type_argument_values)?)
            }
        })
    }
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
impl Expression<GenericType> {
    fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> Result<Expression<InstantiatedType>, TypeError> {
        Ok(Expression {
            position: self.position,
            type_: self.type_.instantiate(type_argument_values)?,
            kind: match &self.kind {
                ExpressionKind::Call { target, arguments } => ExpressionKind::Call {
                    target: Box::new(target.instantiate(type_argument_values)?),
                    arguments: arguments
                        .iter()
                        .map(|x| x.instantiate(type_argument_values))
                        .collect::<Result<Vec<_>, _>>()?,
                },
                ExpressionKind::Literal(literal) => ExpressionKind::Literal(literal.clone()),
                ExpressionKind::LocalVariableAccess(identifier) => {
                    ExpressionKind::LocalVariableAccess(*identifier)
                }
                ExpressionKind::GlobalVariableAccess(module_id, identifier) => {
                    ExpressionKind::GlobalVariableAccess(*module_id, *identifier)
                }
                ExpressionKind::StructConstructor(instantiated_struct_id, field_values) => {
                    ExpressionKind::StructConstructor(
                        Box::new(instantiated_struct_id.instantiate(type_argument_values)?),
                        field_values
                            .iter()
                            .map(|x| x.instantiate(type_argument_values))
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                }
                ExpressionKind::FieldAccess { target, field } => ExpressionKind::FieldAccess {
                    target: Box::new(target.instantiate(type_argument_values)?),
                    field: *field,
                },
                ExpressionKind::SelfAccess => ExpressionKind::SelfAccess,
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement<T: AnyType> {
    pub binding: Identifier,
    pub value: Expression<T>,
}
impl LetStatement<GenericType> {
    fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> Result<LetStatement<InstantiatedType>, TypeError> {
        Ok(LetStatement {
            binding: self.binding,
            value: self.value.instantiate(type_argument_values)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}
