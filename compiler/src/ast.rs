use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum SourceRange {
    Visible((usize, usize), (usize, usize)),
    Internal,
}

impl SourceRange {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Self::Visible(start, end)
    }

    pub fn as_id(&self) -> String {
        match self {
            SourceRange::Visible(start, end) => {
                format!("{}_{}__{}_{}", start.0, start.1, end.0, end.1)
            }
            SourceRange::Internal => "__internal__".to_string(),
        }
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceRange::Visible(start, end) => {
                write!(f, "({}, {}) to ({}, {})", start.0, start.1, end.0, end.1)
            }
            SourceRange::Internal => write!(f, "(internal)"),
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
            Literal::String(value, _) => write!(f, "\"{value}\""),
            Literal::UnsignedInteger(value) => write!(f, "{value}"),
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
    // TODO unify position/location naming
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
                    .map(|a| format!("{}", a))
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
    #[allow(unused)]
    Statements(Vec<Statement>, SourceRange),
    #[allow(unused)]
    Extern(SourceRange),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    #[allow(unused)]
    pub return_type: TypeDescription,
    pub body: FunctionBody,
    pub export: bool,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_: TypeDescription,
    #[allow(unused)]
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub struct_name: String,
    pub functions: Vec<Function>,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(Function),
    Struct(Struct),
    Impl(Impl),
}

impl Declaration {
    pub fn position(&self) -> SourceRange {
        match self {
            Declaration::Function(function) => function.position,
            Declaration::Struct(struct_) => struct_.position,
            Declaration::Impl(impl_) => impl_.position,
        }
    }
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
