use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum SourceRange {
    Visible((usize, usize), (usize, usize)),
    Internal,
}

impl SourceRange {
    pub const fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Self::Visible(start, end)
    }

    pub fn as_id(&self) -> String {
        match self {
            Self::Visible(start, end) => {
                format!("{}_{}__{}_{}", start.0, start.1, end.0, end.1)
            }
            Self::Internal => "__internal__".to_string(),
        }
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Visible(start, end) => {
                write!(f, "({}, {}) to ({}, {})", start.0, start.1, end.0, end.1)
            }
            Self::Internal => write!(f, "(internal)"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String, SourceRange),
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
    FunctionCall {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
    VariableReference(String),
    StructConstructor(String),
    FieldAccess {
        target: Box<Expression>,
        field_name: String,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub position: SourceRange,
    pub kind: ExpressionKind,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::FunctionCall { target, arguments } => write!(
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
    Expression(Expression, SourceRange),
    Let(String, TypeDescription, Expression),
    Return(Expression, SourceRange),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDescription {
    Array(Box<TypeDescription>),
    Named(String),
}

#[derive(Debug, Clone)]
pub struct Argument {
    #[allow(unused)] //FIXME actually use it
    pub name: String,
    pub type_: TypeDescription,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Statements(Vec<Statement>, SourceRange),
    Extern(String, SourceRange),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub return_type: TypeDescription,
    pub body: FunctionBody,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_: TypeDescription,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub struct_name: String,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Function(Function),
    Struct(Struct),
    Impl(Impl),
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Export,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub visibility: Visibility,
    pub position: SourceRange,
}

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
    pub position: SourceRange,
}

#[derive(Debug)]
pub struct SourceFile {
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub imports: Vec<Import>,
}
