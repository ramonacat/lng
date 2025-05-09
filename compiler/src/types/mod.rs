pub mod functions;
pub mod modules;
pub mod structs;

use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use functions::{Function, FunctionId};
use itertools::Itertools;
use modules::ModuleId;
use structs::{InstantiatedStructId, Struct, StructId};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    std::TYPE_NAME_U64,
};

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
pub struct TypeArgumentValues(pub(crate) HashMap<TypeArgument, Type>);

impl std::hash::Hash for TypeArgumentValues {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().collect_vec().hash(state);
    }
}

impl TypeArgumentValues {
    pub(crate) fn new_empty() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, type_argument: TypeArgument) -> Option<&Type> {
        self.0.get(&type_argument)
    }

    pub(crate) const fn new(tav: HashMap<TypeArgument, Type>) -> Self {
        Self(tav)
    }
}

impl std::fmt::Display for TypeArgumentValues {
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
pub enum TypeKind {
    Generic(TypeArgument),
    Unit,
    Object { type_name: InstantiatedStructId },
    Array { element_type: Box<Type> },
    // TODO this should be an object with special properties
    Callable(FunctionId),
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    Pointer(Box<Type>),
}

// TODO separate GenericType and InstantiatedType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
    arguments: TypeArguments,
    argument_values: TypeArgumentValues,
}

impl Type {
    pub(crate) fn debug_name(&self) -> String {
        let Self {
            kind,
            arguments: _,
            argument_values: type_argument_values,
        } = self;
        match kind {
            TypeKind::Unit => "void".to_string(),
            TypeKind::Object {
                type_name: item_path,
            } => format!("{item_path}{type_argument_values}"),
            TypeKind::Array {
                element_type: inner,
            } => format!("{}[]", inner.debug_name()),
            TypeKind::Callable(function_id) => format!("Callable({function_id})",),
            TypeKind::U64 => "u64".to_string(),
            TypeKind::U8 => "u8".to_string(),
            TypeKind::Pointer(inner) => format!("*{}", inner.debug_name()),
            TypeKind::Generic(type_argument) => format!("Generic({type_argument})"),
        }
    }

    pub(crate) fn new_not_generic(kind: TypeKind) -> Self {
        Self {
            kind,
            argument_values: TypeArgumentValues::new_empty(),
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) fn new_generic(kind: TypeKind, type_arguments: Vec<TypeArgument>) -> Self {
        Self {
            kind,
            arguments: TypeArguments::new(type_arguments),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn u64() -> Self {
        Self::new_not_generic(TypeKind::U64)
    }

    pub(crate) fn unit() -> Self {
        Self::new_not_generic(TypeKind::Unit)
    }

    pub(crate) const fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub(crate) fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        let kind = match &self.kind {
            // TODO assert that the type_argument is fully instantiated
            TypeKind::Generic(type_argument) => type_argument_values
                .get(*type_argument)
                .unwrap()
                .kind()
                .clone(),
            TypeKind::Unit => TypeKind::Unit,
            TypeKind::Object { type_name } => TypeKind::Object {
                type_name: type_name.clone(),
            },
            TypeKind::Array { element_type } => TypeKind::Array {
                element_type: Box::new(element_type.instantiate(type_argument_values)),
            },
            TypeKind::Callable(function_id) => {
                // TODO we should actually go to the function table and instantiate it there
                TypeKind::Callable(*function_id)
            }
            TypeKind::U64 => TypeKind::U64,
            TypeKind::U8 => TypeKind::U8,
            TypeKind::Pointer(target) => {
                TypeKind::Pointer(Box::new(target.instantiate(type_argument_values)))
            }
        };

        Self {
            kind,
            arguments: TypeArguments::new_empty(),
            argument_values: TypeArgumentValues::new_empty(),
        }
    }

    pub(crate) fn struct_name(&self) -> InstantiatedStructId {
        match &self.kind {
            TypeKind::Generic(_) => todo!(),
            TypeKind::Unit => todo!(),
            TypeKind::Object { type_name } => type_name.clone(),
            TypeKind::Array { .. } => todo!(),
            TypeKind::Callable { .. } => todo!(),
            TypeKind::U64 => InstantiatedStructId(*TYPE_NAME_U64, TypeArgumentValues::new_empty()),
            TypeKind::U8 => todo!(),
            TypeKind::Pointer(_) => todo!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unit => write!(f, "void"),
            TypeKind::Object {
                type_name: identifier,
            } => write!(f, "{identifier}{}", self.argument_values),
            TypeKind::Array {
                element_type: inner,
            } => write!(f, "{inner}[]"),
            TypeKind::Callable(function_id) => write!(f, "Callable({function_id})"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::Pointer(to) => write!(f, "*{to}"),
            TypeKind::Generic(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(Expression),
}

impl Statement {
    fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        match self {
            Self::Expression(expression) => {
                Self::Expression(expression.instantiate(type_argument_values))
            }
            Self::Let(let_statement) => Self::Let(let_statement.instantiate(type_argument_values)),
            Self::Return(expression) => Self::Return(expression.instantiate(type_argument_values)),
        }
    }
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
    GlobalVariableAccess(FQName),
    StructConstructor(InstantiatedStructId),
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
impl Expression {
    fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        Self {
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
                ExpressionKind::GlobalVariableAccess(fqname) => {
                    ExpressionKind::GlobalVariableAccess(*fqname)
                }
                ExpressionKind::StructConstructor(instantiated_struct_id) => {
                    ExpressionKind::StructConstructor(instantiated_struct_id.clone())
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
pub struct LetStatement {
    pub binding: Identifier,
    pub value: Expression,
}
impl LetStatement {
    fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        Self {
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
