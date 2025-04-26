use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    str,
};

use itertools::Itertools as _;

use crate::{
    ast::{self, SourceRange},
    name_mangler::MangledIdentifier,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier(String);

impl Identifier {
    pub fn parse(raw: &str) -> Self {
        Self(raw.to_string())
    }

    pub(crate) fn raw(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.0;

        write!(f, "{name}")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModulePath(Vec<Identifier>);

impl ModulePath {
    pub fn parse(raw: &str) -> Self {
        Self(raw.split('.').map(Identifier::parse).collect())
    }

    pub fn from_parts<'a>(path: impl Iterator<Item = &'a str>) -> Self {
        Self(path.map(Identifier::parse).collect())
    }

    pub fn parts(&self) -> &[Identifier] {
        self.0.as_slice()
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = self.0.iter().map(Identifier::raw).join(".");

        write!(f, "{name}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Object(Identifier),
    Array(Box<Type>),
    // TODO this should probably be simply an object, just of StructDescriptor<TargetStruct> type
    StructDescriptor(Identifier, Vec<StructField>),
    // TODO this should be an object with special properties
    Callable {
        self_type: Option<Identifier>,
        arguments: Vec<Argument>,
        return_type: Box<Type>,
    },
    // TODO add u128,u32,u16,u8 and signed counterparts
    // TODO add bool
    // TODO add float
    U64,
    U8,
    // TODO do we want this exposed to userland in an unsafe mode?
    // TODO this should have the target type as its value
    Pointer(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Object(identifier) => write!(f, "{identifier}"),
            Self::Array(inner) => write!(f, "{inner}[]"),
            Self::StructDescriptor(name, _) => write!(f, "StructDescriptor<{name}>"),
            Self::Callable {
                self_type,
                arguments,
                return_type,
            } => write!(
                f,
                "({}{}): {return_type}",
                self_type
                    .as_ref()
                    .map_or_else(String::new, |x| format!("self: {x}")),
                arguments
                    .iter()
                    .map(|a| format!("{a}"))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Self::U8 => write!(f, "u8"),
            Self::U64 => write!(f, "u64"),
            Self::Pointer(to) => write!(f, "*{to}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub type_: Type,
    pub position: SourceRange,
}

impl Eq for Argument {}
impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        (&self.name, &self.type_) == (&other.name, &other.type_)
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub mangled_name: MangledIdentifier,
    pub arguments: Vec<Argument>,
    pub self_type: Option<Identifier>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub export: bool,
    pub position: ast::SourceRange,
}
impl Function {
    pub(crate) const fn has_self(&self) -> bool {
        self.self_type.is_some()
    }

    pub(crate) fn is_exported(&self) -> bool {
        self.name.0 == "main" || matches!(self.body, FunctionBody::Extern) || self.export
    }

    pub(crate) fn type_(&self) -> Type {
        Type::Callable {
            self_type: self.self_type.clone(),
            arguments: self.arguments.clone(),
            return_type: Box::new(self.return_type.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Extern,
    Statements(Vec<Statement>),
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
    FunctionCall {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
    VariableAccess(Identifier),
    StructConstructor(Identifier),
    FieldAccess {
        target: Box<Expression>,
        field: Identifier,
    },
    SelfAccess,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: SourceRange,
    pub type_: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub binding: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: ModulePath,
    pub item: Identifier,
    pub position: ast::SourceRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: Identifier,
    pub mangled_name: MangledIdentifier,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    // TODO the exports should really be handled at module level
    pub export: bool,
    pub name: Identifier,
    #[allow(unused)]
    pub mangled_name: MangledIdentifier,
    pub fields: Vec<StructField>,
    pub impls: HashMap<Identifier, Function>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Import(Import),
}

#[derive(Debug)]
pub struct Module {
    pub items: HashMap<Identifier, Item>,
}

#[derive(Debug)]
// this should have a different name because this can be a program OR a library
pub struct Program {
    pub modules: HashMap<ModulePath, Module>,
}
