use std::{collections::HashMap, error::Error, fmt::Display};

use crate::ast::{Expression, Function, FunctionBody, SourceFile, SourceRange, Statement, Type};

#[derive(Debug)]
pub struct Program(pub Vec<SourceFile>);

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    position: SourceRange,
}

impl Error for TypeCheckError {}
impl Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error {} at {}", self.description, self.position)
    }
}

#[derive(Debug)]
pub enum TypeCheckErrorDescription {
    CallingUndeclaredFunction(String),
    UnexpectedArgumentTypeInFunctionCall {
        function_name: String,
        argument_name: String,
    },
    IncorrectNumberOfArgumentsPassed(String),
    FunctionArgumentCannotBeVoid {
        function_name: String,
        argument_name: String,
    },
}

impl TypeCheckErrorDescription {
    fn at(self, position: SourceRange) -> TypeCheckError {
        TypeCheckError {
            description: self,
            position,
        }
    }
}

impl Display for TypeCheckErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeCheckErrorDescription::CallingUndeclaredFunction(name) => write!(f, "The function {name} has not been declared"),
            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall { function_name, argument_name } => write!(f, "Incorrect argument type for argument {argument_name} in a call to {function_name}"),
            TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(name) => write!(f, "Incorrect number of arguments passed to {name}"),
            TypeCheckErrorDescription::FunctionArgumentCannotBeVoid { function_name, argument_name } => write!(f, "Argument {argument_name} in the declaration of {function_name} cannot be of type void"),
        }
    }
}

pub fn type_check(program: &Program) -> Result<(), TypeCheckError> {
    let mut declared_functions = HashMap::new();

    for file in &program.0 {
        for declaration in &file.declarations {
            match declaration {
                crate::ast::Declaration::Function(function, _) => {
                    for arg in &function.arguments {
                        if matches!(arg.type_, Type::Void) {
                            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                                function_name: function.name.to_owned(),
                                argument_name: arg.name.to_owned(),
                            }
                            .at(arg.position));
                        }
                    }
                    declared_functions.insert(function.name.to_string(), function.clone())
                }
            };
        }
    }

    for (_, declared_function) in declared_functions.iter() {
        let FunctionBody::Statements(body_statements, _) = &declared_function.body else {
            continue;
        };
        for statement in body_statements.iter() {
            match statement {
                Statement::Expression(expression, _) => {
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
        Expression::FunctionCall {
            name,
            arguments,
            position,
        } => {
            let Some(called_function) = declared_functions.get(name.as_str()) else {
                return Err(
                    TypeCheckErrorDescription::CallingUndeclaredFunction(name.to_string())
                        .at(*position),
                );
            };

            if arguments.len() != called_function.arguments.len() {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    called_function.name.clone(),
                )
                .at(*position));
            }

            for (argument, called_function_argument) in
                arguments.iter().zip(&called_function.arguments)
            {
                let type_ = determine_expression_type(declared_functions, argument)?;
                let expected_type = &called_function_argument.type_;

                if &type_ != expected_type {
                    return Err(
                        TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                            function_name: name.to_string(),
                            argument_name: called_function_argument.name.to_string(),
                        }
                        .at(argument.position()),
                    );
                }
            }
        }
        Expression::Literal(_, _) => {}
    }

    Ok(())
}

// TODO this type should not be an AST Type probably, but some type-system internal representation
fn determine_expression_type(
    declared_functions: &HashMap<String, Function>,
    expression: &Expression,
) -> Result<Type, TypeCheckError> {
    match expression {
        Expression::FunctionCall { name, position, .. } => Ok(declared_functions
            .get(name)
            .map(Ok)
            .unwrap_or_else(|| {
                Err(
                    TypeCheckErrorDescription::CallingUndeclaredFunction(name.clone())
                        .at(*position),
                )
            })?
            .return_type
            .clone()),
        Expression::Literal(literal, _) => match literal {
            crate::ast::Literal::String(_, _) => Ok(Type::Named("string".to_string())),
        },
    }
}
