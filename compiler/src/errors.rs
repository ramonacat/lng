use crate::{ast, types};

#[derive(Debug, Clone, Copy)]
pub enum ErrorLocation {
    Position(types::FQName, ast::SourceRange),
    Indeterminate,
    ItemOnly(types::FQName),
}

impl std::fmt::Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Position(fqname, source_range) => {
                write!(f, "{source_range} in module {fqname}")
            }
            Self::Indeterminate => write!(f, "indeterminate"),
            Self::ItemOnly(fqname) => write!(f, "{fqname}"),
        }
    }
}
