use std::{error::Error, fmt::Display};

use inkwell::builder::BuilderError;

use crate::ast;

#[derive(Debug)]
pub struct CompileError {
    description: CompileErrorDescription,
    position: ast::SourceSpan,
}

#[derive(Debug)]
pub enum CompileErrorDescription {
    InternalError(String),
}

impl Display for CompileErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InternalError(message) => {
                write!(f, "Internal error: {message}")
            }
        }
    }
}

impl Error for CompileError {}
impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "compile error: {} at {}",
            self.description, self.position
        )
    }
}

impl From<BuilderError> for CompileErrorDescription {
    fn from(value: BuilderError) -> Self {
        Self::InternalError(format!("{value}"))
    }
}

pub trait IntoCompileError {
    fn at(self, position: ast::SourceSpan) -> CompileError;
}

// TODO remove this, these errors should actually be panics
impl IntoCompileError for BuilderError {
    fn at(self, position: ast::SourceSpan) -> CompileError {
        CompileError {
            description: self.into(),
            position,
        }
    }
}
