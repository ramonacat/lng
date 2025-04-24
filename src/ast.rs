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
    String(String, SourceRange),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(value, _) => write!(f, "\"{value}\""),
        }
    }
}

#[derive(Debug, Clone)]
// TODO this should be wrapped in a struct, so position does not have to be repeated
pub enum Expression {
    FunctionCall {
        target: Box<Expression>,
        arguments: Vec<Expression>,
        position: SourceRange,
    },
    Literal(Literal, SourceRange),
    VariableReference(String, SourceRange),
    StructConstructor(String, SourceRange),
    FieldAccess {
        target: Box<Expression>,
        field_name: String,
        position: SourceRange,
    },
}

impl Expression {
    pub fn position(&self) -> SourceRange {
        match self {
            Expression::FunctionCall { position, .. } => *position,
            Expression::Literal(_, position) => *position,
            Expression::VariableReference(_, position) => *position,
            Expression::StructConstructor(_, position) => *position,
            Expression::FieldAccess { position, .. } => *position,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::FunctionCall {
                target,
                arguments,
                position: _,
            } => write!(
                f,
                "{target}({})",
                arguments
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::Literal(literal, _) => write!(f, "{literal}"),
            Expression::VariableReference(name, _) => write!(f, "{name}"),
            Expression::StructConstructor(name, _) => write!(f, "{name} {{}}"),
            Expression::FieldAccess {
                target,
                field_name,
                position: _,
            } => write!(f, "{target}.{field_name}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    #[allow(unused)]
    Expression(Expression, SourceRange),
    Let(String, TypeDescription, Expression),
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
