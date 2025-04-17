#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Array(Box<Type>),
    Named(String),
}

#[derive(Debug, Clone)]
pub struct Argument {
    #[allow(unused)] //FIXME actually use it
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Statements(Vec<Statement>),
    Intrinsic,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: FunctionBody,
}

#[derive(Debug)]
pub struct SourceFile {
    pub name: String,
    pub functions: Vec<Function>,
}
