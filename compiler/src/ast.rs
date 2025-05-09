use std::{
    fmt::Display,
    sync::{LazyLock, RwLock},
};

use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

use crate::identifier::{FQName, Identifier};

static FILENAMES: LazyLock<RwLock<StringInterner<StringBackend>>> =
    LazyLock::new(|| RwLock::new(StringInterner::default()));

#[derive(Debug, Clone, Copy)]
pub enum SourceSpan {
    Visible(SourceFileName, usize, usize),
    Internal,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceFileName(SymbolU32);

impl SourceFileName {
    #[must_use]
    pub fn new(name: String) -> Self {
        let symbol = FILENAMES.write().unwrap().get_or_intern(name);
        Self(symbol)
    }
}

impl Display for SourceFileName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", FILENAMES.read().unwrap().resolve(self.0).unwrap())
    }
}

impl SourceSpan {
    #[must_use]
    pub const fn new(filename: SourceFileName, start: usize, end: usize) -> Self {
        Self::Visible(filename, start, end)
    }
}

impl Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Visible(filename, start, end) => {
                write!(f, "{filename}({start}, {end})")
            }
            Self::Internal => write!(f, "(internal)"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String, SourceSpan),
    UnsignedInteger(u64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(value, _) => write!(f, "\"{value}\""),
            Self::UnsignedInteger(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Call {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
    VariableReference(Identifier),
    StructConstructor(Identifier),
    FieldAccess {
        target: Box<Expression>,
        field_name: Identifier,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: SourceSpan,
    pub kind: ExpressionKind,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::Call { target, arguments } => write!(
                f,
                "{target}({})",
                arguments
                    .iter()
                    .map(|a| format!("{a}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExpressionKind::Literal(literal) => write!(f, "{literal}"),
            ExpressionKind::VariableReference(name) => write!(f, "{name}"),
            ExpressionKind::StructConstructor(name) => write!(f, "{name} {{}}"),
            ExpressionKind::FieldAccess { target, field_name } => {
                write!(f, "{target}.{field_name}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    #[allow(unused)]
    Expression(Expression, SourceSpan),
    Let(Identifier, TypeDescription, Expression),
    Return(Expression, SourceSpan),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDescription {
    Array(Box<TypeDescription>),
    Named(Identifier),
}

#[derive(Debug, Clone)]
pub struct Argument {
    #[allow(unused)] //FIXME actually use it
    pub name: Identifier,
    pub type_: TypeDescription,
    pub position: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Statements(Vec<Statement>, SourceSpan),
    Extern(Identifier, SourceSpan),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub return_type: TypeDescription,
    pub body: FunctionBody,
    pub visibility: Visibility,
    pub position: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub type_: TypeDescription,
    pub position: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<StructField>,
    pub visibility: Visibility,
    pub position: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub struct_name: Identifier,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Function(Function),
    Struct(Struct),
    Impl(Impl),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Export,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub position: SourceSpan,
}

#[derive(Debug)]
pub struct Import {
    pub path: FQName,
    pub alias: Option<Identifier>,
    pub position: SourceSpan,
}

#[derive(Debug)]
pub struct SourceFile {
    pub name: SourceFileName,
    pub declarations: Vec<Declaration>,
    pub imports: Vec<Import>,
}
