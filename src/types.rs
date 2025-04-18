use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::ast;

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

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Object(String),
    #[allow(unused)]
    Array(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct Argument {
    #[allow(unused)]
    pub name: Identifier,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub body: ast::FunctionBody,
    pub export: bool,
    pub location: ast::SourceRange,
}

#[derive(Debug)]
pub struct Import {
    pub path: ModulePath,
    pub item: Identifier,
    pub location: ast::SourceRange,
}

#[derive(Debug)]
pub enum Item {
    Function(Function),
    Import(Import),
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Program {
    pub modules: HashMap<ModulePath, Module>,
}
