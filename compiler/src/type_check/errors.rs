use std::{error::Error, fmt::Display};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    types,
};

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    location: ast::SourceSpan,
}

impl Error for TypeCheckError {}
impl Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error {} at {}", self.description, self.location)
    }
}

#[derive(Debug)]
pub enum TypeCheckErrorDescription {
    UnexpectedArgumentTypeInCall {
        target: types::GenericType,
        argument_name: Identifier,
        expected_type: types::GenericType,
        actual_type: types::GenericType,
    },
    IncorrectNumberOfArgumentsPassed(types::GenericType),
    FunctionArgumentCannotBeVoid {
        argument_name: Identifier,
    },
    ModuleDoesNotExist(FQName),
    ItemDoesNotExist(types::ItemId),
    ItemNotExported(FQName, Identifier),
    UndeclaredVariable(Identifier),
    ImplNotOnStruct(FQName),
    MismatchedAssignmentType {
        target_variable: Identifier,
        variable_type: types::GenericType,
        assigned_type: types::GenericType,
    },
    CallingNotCallableItem(types::GenericType),
    MismatchedReturnType {
        actual: types::GenericType,
        expected: types::GenericType,
    },
}

impl TypeCheckErrorDescription {
    pub(super) const fn at(self, location: ast::SourceSpan) -> TypeCheckError {
        TypeCheckError {
            description: self,
            location,
        }
    }
}

impl Display for TypeCheckErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedArgumentTypeInCall {
                target,
                argument_name,
                expected_type,
                actual_type,
            } => write!(
                f,
                "Incorrect argument type {actual_type} for argument {argument_name} of type {expected_type} in a call to {target}"
            ),
            Self::IncorrectNumberOfArgumentsPassed(name) => {
                write!(f, "Incorrect number of arguments passed to {name}")
            }
            Self::FunctionArgumentCannotBeVoid { argument_name } => {
                write!(f, "Argument {argument_name} cannot be of type void")
            }
            Self::ModuleDoesNotExist(module_path) => {
                write!(f, "Module {module_path} does not exist")
            }
            Self::ItemDoesNotExist(item_path) => write!(f, "Item {item_path} does not exist"),
            Self::ItemNotExported(module_path, identifier) => write!(
                f,
                "Item {identifier} exists in module {module_path}, but is not exported"
            ),
            Self::UndeclaredVariable(identifier) => {
                write!(f, "Variable {identifier} does not exist")
            }
            Self::ImplNotOnStruct(identifier) => {
                write!(f, "{identifier} is not a struct, impl is not allowed")
            }
            Self::MismatchedAssignmentType {
                target_variable,
                variable_type,
                assigned_type,
            } => write!(
                f,
                "Cannot assign value of type {assigned_type} to variiable {target_variable} of typee {variable_type}"
            ),
            Self::CallingNotCallableItem(identifier) => {
                write!(f, "{identifier} cannot be called")
            }
            Self::MismatchedReturnType { actual, expected } => write!(
                f,
                "Function was expected to return {expected}, but returns {actual}"
            ),
        }
    }
}
