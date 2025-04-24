use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::ast::{self, SourceRange};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.0;

        write!(f, "{name}")
    }
}

// TODO support paths that are nested
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModulePath(pub Identifier);

impl ModulePath {
    pub(crate) fn as_str(&self) -> &str {
        &self.0 .0
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.0;

        write!(f, "{name}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    // TODO add integer and floating point types
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
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Object(identifier) => write!(f, "{}", identifier),
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::StructDescriptor(name, _) => write!(f, "StructDescriptor<{}>", name),
            Type::Callable {
                self_type,
                arguments,
                return_type,
            } => write!(
                f,
                "({}{}): {return_type}",
                self_type
                    .as_ref()
                    .map(|x| format!("self: {x}"))
                    .unwrap_or_else(String::new),
                arguments
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
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
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub export: bool,
    pub location: ast::SourceRange,
}
impl Function {
    pub(crate) fn has_self(&self) -> bool {
        self.arguments
            .first()
            .map(|a| a.name.0 == "self")
            .unwrap_or(false)
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
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
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

#[derive(Debug)]
pub struct ImportFunction {
    pub path: ModulePath,
    pub item: Identifier,
    pub location: ast::SourceRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    #[allow(unused)]
    pub name: Identifier,
    pub type_: Type,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<StructField>,
    pub impls: HashMap<Identifier, Function>,
}

#[derive(Debug)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    ImportFunction(ImportFunction),
}

#[derive(Debug)]
pub struct Module {
    pub items: HashMap<Identifier, Item>,
}

#[derive(Debug)]
pub struct Program {
    pub modules: HashMap<ModulePath, Module>,
}
