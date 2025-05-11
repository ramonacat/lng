pub mod functions;
pub mod modules;
pub mod structs;

use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    hash::Hash,
};

use functions::FunctionId;
use itertools::Itertools;
use modules::ModuleId;
use structs::{Struct, StructId};

use crate::{ast, identifier::Identifier, std::TYPE_NAME_U64};

pub trait AnyType: Display + Clone + Debug + Hash + Eq + PartialEq {}
impl AnyType for GenericType {}
impl AnyType for InstantiatedType {}

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
pub struct TypeArguments(Vec<TypeArgument>);
impl TypeArguments {
    pub(crate) const fn new_empty() -> Self {
        Self(vec![])
    }

    pub(crate) const fn new(arguments: Vec<TypeArgument>) -> Self {
        Self(arguments)
    }
}

impl std::fmt::Display for TypeArguments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "{}", self.0.iter().map(ToString::to_string).join(","))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeArgument(Identifier);

impl TypeArgument {
    pub const fn new(name: Identifier) -> Self {
        Self(name)
    }
}

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgumentValues<TType: AnyType>(pub(crate) HashMap<TypeArgument, TType>);

impl<TType: AnyType> std::hash::Hash for TypeArgumentValues<TType> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0
            .iter()
            .sorted_by(|x, y| x.0.0.raw().cmp(&y.0.0.raw()))
            .collect_vec()
            .hash(state);
    }
}

impl<TType: AnyType> TypeArgumentValues<TType> {
    pub(crate) fn new_empty() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, type_argument: TypeArgument) -> Option<&TType> {
        self.0.get(&type_argument)
    }

    pub(crate) const fn new(tav: HashMap<TypeArgument, TType>) -> Self {
        Self(tav)
    }

    fn set(&mut self, argument: TypeArgument, value: TType) {
        let old = self.0.insert(argument, value);

        assert!(old.is_none());
    }

    fn merge(self, type_argument_values: Self) -> Self {
        let Self(mut values) = self;

        for (name, value) in type_argument_values.0 {
            values.insert(name, value);
        }

        Self(values)
    }
}

impl TypeArgumentValues<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> TypeArgumentValues<InstantiatedType> {
        let instantiated_arguments = self
            .0
            .iter()
            .map(|(arg, value)| (*arg, value.instantiate(type_argument_values)))
            .collect();

        TypeArgumentValues(instantiated_arguments)
    }
}

impl<TType: AnyType> std::fmt::Display for TypeArgumentValues<TType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(
            f,
            "<{}>",
            self.0
                .iter()
                .map(|(name, value)| format!("{name}={value}"))
                .join(", ")
        )
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
    Struct(StructId),
    Function(FunctionId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstantiatedType {
    kind: InstantiatedTypeKind,
    argument_values: TypeArgumentValues<InstantiatedType>,
}

impl InstantiatedType {
    pub(crate) const fn new(
        kind: InstantiatedTypeKind,
        argument_values: TypeArgumentValues<Self>,
    ) -> Self {
        Self {
            kind,
            argument_values,
        }
    }

    pub(crate) const fn kind(&self) -> &InstantiatedTypeKind {
        &self.kind
    }

    pub(crate) const fn argument_values(&self) -> &TypeArgumentValues<Self> {
        &self.argument_values
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericTypeKind {
    Generic(TypeArgument),
    Unit,
    Object { type_name: StructId },
    Array { element_type: Box<GenericType> },
    // TODO this should be an object with special properties
    Callable(FunctionId),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<GenericType>),
    Struct(StructId),
    Function(FunctionId),
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
            GenericTypeKind::Object { type_name } => write!(f, "{type_name}"),
            GenericTypeKind::Array { element_type } => write!(f, "{element_type}[]"),
            GenericTypeKind::Callable(function_id) => write!(f, "callable<{function_id}>"),
            GenericTypeKind::U64 => write!(f, "u64"),
            GenericTypeKind::U8 => write!(f, "u8"),
            GenericTypeKind::Pointer(generic_type) => write!(f, "{generic_type}*"),
            GenericTypeKind::Struct(struct_id) => write!(f, "{struct_id}"),
            GenericTypeKind::Function(function_id) => write!(f, "{function_id}"),
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

    #[allow(unused)] // TODO this will be used once we actually have a syntax for generics
    pub(crate) fn set_type_argument(&mut self, argument: TypeArgument, value: Self) {
        self.type_argument_values.set(argument, value);
    }

    // TODO this potentially can return an error (if arguments are missing)!
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> InstantiatedType {
        let type_argument_values = self
            .type_argument_values
            .instantiate(type_argument_values)
            .merge(type_argument_values.clone());

        let kind: InstantiatedTypeKind = match &self.kind {
            GenericTypeKind::Generic(type_argument) => {
                type_argument_values
                    .get(*type_argument)
                    .unwrap()
                    .clone()
                    .kind
            }
            GenericTypeKind::Unit => InstantiatedTypeKind::Unit,
            GenericTypeKind::Object { type_name } => InstantiatedTypeKind::Object {
                type_name: *type_name,
                type_argument_values: type_argument_values.clone(),
            },
            GenericTypeKind::Array { element_type } => InstantiatedTypeKind::Array {
                element_type: Box::new(element_type.instantiate(&type_argument_values)),
            },
            GenericTypeKind::Callable(function_id) => InstantiatedTypeKind::Callable(*function_id),
            GenericTypeKind::U64 => InstantiatedTypeKind::U64,
            GenericTypeKind::U8 => InstantiatedTypeKind::U8,
            GenericTypeKind::Pointer(pointee) => {
                InstantiatedTypeKind::Pointer(Box::new(pointee.instantiate(&type_argument_values)))
            }
            GenericTypeKind::Struct(struct_id) => InstantiatedTypeKind::Struct(*struct_id),
            GenericTypeKind::Function(function_id) => InstantiatedTypeKind::Function(*function_id),
        };

        InstantiatedType {
            kind,
            argument_values: type_argument_values,
        }
    }

    pub(crate) const fn arguments(&self) -> &TypeArguments {
        &self.type_arguments
    }

    pub(crate) fn struct_name(&self) -> StructId {
        match self.kind() {
            GenericTypeKind::Generic(_) => todo!(),
            GenericTypeKind::Unit => todo!(),
            GenericTypeKind::Object { type_name } => *type_name,
            GenericTypeKind::Array { .. } => todo!(),
            GenericTypeKind::Callable(_) => todo!(),
            GenericTypeKind::U64 => *TYPE_NAME_U64,
            GenericTypeKind::U8 => todo!(),
            GenericTypeKind::Pointer(_) => todo!(),
            GenericTypeKind::Struct(_) => todo!(),
            GenericTypeKind::Function(_) => todo!(),
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
    ) -> Statement<InstantiatedType> {
        match self {
            Self::Expression(expression) => {
                Statement::Expression(expression.instantiate(type_argument_values))
            }
            Self::Let(let_statement) => {
                Statement::Let(let_statement.instantiate(type_argument_values))
            }
            Self::Return(expression) => {
                Statement::Return(expression.instantiate(type_argument_values))
            }
        }
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
    StructConstructor(StructId),
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
    ) -> Expression<InstantiatedType> {
        Expression {
            position: self.position,
            type_: self.type_.instantiate(type_argument_values),
            kind: match &self.kind {
                ExpressionKind::Call { target, arguments } => ExpressionKind::Call {
                    target: Box::new(target.instantiate(type_argument_values)),
                    arguments: arguments
                        .iter()
                        .map(|x| x.instantiate(type_argument_values))
                        .collect(),
                },
                ExpressionKind::Literal(literal) => ExpressionKind::Literal(literal.clone()),
                ExpressionKind::LocalVariableAccess(identifier) => {
                    ExpressionKind::LocalVariableAccess(*identifier)
                }
                ExpressionKind::GlobalVariableAccess(module_id, identifier) => {
                    ExpressionKind::GlobalVariableAccess(*module_id, *identifier)
                }
                ExpressionKind::StructConstructor(instantiated_struct_id) => {
                    ExpressionKind::StructConstructor(*instantiated_struct_id)
                }
                ExpressionKind::FieldAccess { target, field } => ExpressionKind::FieldAccess {
                    target: Box::new(target.instantiate(type_argument_values)),
                    field: *field,
                },
                ExpressionKind::SelfAccess => ExpressionKind::SelfAccess,
            },
        }
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
    ) -> LetStatement<InstantiatedType> {
        LetStatement {
            binding: self.binding,
            value: self.value.instantiate(type_argument_values),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}
