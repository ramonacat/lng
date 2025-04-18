use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{ast, types};

#[derive(Debug)]
pub struct Program(pub Vec<ast::SourceFile>);

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    position: ast::SourceRange,
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
    fn at(self, position: ast::SourceRange) -> TypeCheckError {
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

fn convert_type(type_: &ast::TypeDescription) -> types::Type {
    match type_ {
        ast::TypeDescription::Array(type_description) => {
            types::Type::Array(Box::new(convert_type(type_description)))
        }
        ast::TypeDescription::Named(name) if name == "void" => types::Type::Void,
        ast::TypeDescription::Named(name) => types::Type::Object(name.clone()),
    }
}

pub fn type_check(program: &Program) -> Result<types::Program, TypeCheckError> {
    let mut modules = HashMap::new();
    let mut declared_functions = HashMap::new();

    for file in &program.0 {
        let mut items = vec![];

        for declaration in &file.declarations {
            match declaration {
                ast::Declaration::Function(function) => {
                    let mut arguments = vec![];

                    for arg in &function.arguments {
                        if matches!(arg.type_, ast::TypeDescription::Named(ref name) if name == "void")
                        {
                            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                                function_name: function.name.to_owned(),
                                argument_name: arg.name.to_owned(),
                            }
                            .at(arg.position));
                        }

                        arguments.push(types::Argument {
                            name: types::Identifier(arg.name.clone()),
                            type_: convert_type(&arg.type_),
                        });
                    }

                    declared_functions.insert(function.name.to_string(), function.clone());
                    items.push(types::Item::Function(types::Function {
                        name: types::Identifier(function.name.clone()),
                        arguments,
                        body: function.body.clone(),
                        export: function.export,
                        location: function.position,
                    }));
                }
            };
        }

        for import in &file.imports {
            let module_name = import.path[0].clone();
            let item_name = import.path[1].clone();

            items.push(types::Item::Import(types::Import {
                path: types::ModulePath(types::Identifier(module_name)),
                item: types::Identifier(item_name),
                location: import.position,
            }));
        }

        modules.insert(
            types::ModulePath(types::Identifier(file.name.clone())),
            types::Module { items },
        );
    }

    for (_, declared_function) in declared_functions.iter() {
        let ast::FunctionBody::Statements(body_statements, _) = &declared_function.body else {
            continue;
        };
        for statement in body_statements.iter() {
            match statement {
                ast::Statement::Expression(expression, _) => {
                    type_check_expression(expression, &declared_functions)?;
                }
            }
        }
    }

    Ok(types::Program { modules })
}

fn type_check_expression(
    expression: &ast::Expression,
    declared_functions: &HashMap<String, ast::Function>,
) -> Result<types::Type, TypeCheckError> {
    match expression {
        ast::Expression::FunctionCall {
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
                let type_ = type_check_expression(argument, declared_functions)?;
                let expected_type = convert_type(&called_function_argument.type_);

                if type_ != expected_type {
                    return Err(
                        TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                            function_name: name.to_string(),
                            argument_name: called_function_argument.name.to_string(),
                        }
                        .at(argument.position()),
                    );
                }
            }

            Ok(convert_type(&called_function.return_type))
        }
        ast::Expression::Literal(literal, _) => match literal {
            ast::Literal::String(_, _) => Ok(types::Type::Object("string".to_string())),
        },
    }
}
