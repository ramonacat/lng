use std::collections::HashMap;

use crate::ast::{Expression, Function, FunctionBody, SourceFile, Statement, Type};
use thiserror::Error;

pub struct Program(pub Vec<SourceFile>);

#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("Calling an undeclared function: {0}")]
    CallingUndeclaredFunction(String),
    #[error("Unexpected type of argument {1} in a call to {0}")]
    UnexpectedArgumentTypeInFunctionCall(String, usize),
    #[error("Incorrect number of arguments passed to function {0}")]
    IncorrectNumberOfArgumentsPassed(String),
}

pub fn type_check(program: &Program) -> Result<(), TypeCheckError> {
    let mut declared_functions = HashMap::new();

    for file in &program.0 {
        for function in &file.functions {
            declared_functions.insert(function.name.to_string(), function.clone());
        }
    }

    for (_, declared_function) in declared_functions.iter() {
        let FunctionBody::Statements(body_statements) = &declared_function.body else {
            continue;
        };
        for statement in body_statements.iter() {
            match statement {
                Statement::Expression(expression) => {
                    type_check_expression(expression, &declared_functions)?
                }
            }
        }
    }

    Ok(())
}

fn type_check_expression(
    expression: &Expression,
    declared_functions: &HashMap<String, Function>,
) -> Result<(), TypeCheckError> {
    match expression {
        Expression::FunctionCall { name, arguments } => {
            let Some(called_function) = declared_functions.get(name.as_str()) else {
                return Err(TypeCheckError::CallingUndeclaredFunction(name.to_string()));
            };

            if arguments.len() != called_function.arguments.len() {
                return Err(TypeCheckError::IncorrectNumberOfArgumentsPassed(
                    called_function.name.clone(),
                ));
            }

            for (idx, argument) in arguments.iter().enumerate() {
                let type_ = determine_expression_type(declared_functions, argument)?;
                let expected_type = &called_function.arguments.get(idx).unwrap().type_;

                if &type_ != expected_type {
                    return Err(TypeCheckError::UnexpectedArgumentTypeInFunctionCall(
                        name.to_string(),
                        idx,
                    ));
                }
            }
        }
        Expression::Literal(_) => {}
    }

    Ok(())
}

fn determine_expression_type(
    declared_functions: &HashMap<String, Function>,
    expression: &Expression,
) -> Result<Type, TypeCheckError> {
    match expression {
        Expression::FunctionCall { name, .. } => Ok(declared_functions
            .get(name)
            .map(|x| Ok(x))
            .unwrap_or_else(|| Err(TypeCheckError::CallingUndeclaredFunction(name.clone())))?
            .return_type
            .clone()),
        Expression::Literal(literal) => match literal {
            crate::ast::Literal::String(_) => Ok(Type::Named("string".to_string())),
        },
    }
}
