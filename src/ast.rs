use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct SourceRange(pub (usize, usize), pub (usize, usize));

impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}, {}) to ({}, {})",
            self.0 .0, self.0 .1, self.1 .0, self.1 .1
        )
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    #[allow(unused)]
    String(String, SourceRange),
}

#[derive(Debug, Clone)]
pub enum Expression {
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
        position: SourceRange,
    },
    Literal(Literal, SourceRange),
    VariableReference(String, SourceRange),
}

impl Expression {
    pub fn position(&self) -> SourceRange {
        match self {
            Expression::FunctionCall { position, .. } => *position,
            Expression::Literal(_, position) => *position,
            Expression::VariableReference(_, position) => *position,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    #[allow(unused)]
    Expression(Expression, SourceRange),
}

// TODO add SourcePosition here once the type_check internal type is decoupled from this
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
    pub return_type: TypeDescription,
    pub body: FunctionBody,
    pub export: bool,
    pub position: SourceRange,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_: TypeDescription,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(Function),
    Struct(Struct),
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
