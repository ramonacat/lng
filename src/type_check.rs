use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast,
    types::{self, Identifier, ModulePath},
};

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
// TODO get rid of the strings and replace with the right objects (Identifier, ModulePath, etc.)
pub enum TypeCheckErrorDescription {
    UnexpectedArgumentTypeInFunctionCall {
        target: ast::Expression,
        argument_name: String,
    },
    IncorrectNumberOfArgumentsPassed(types::Type),
    FunctionArgumentCannotBeVoid {
        function_name: String,
        argument_name: String,
    },
    ModuleDoesNotExist(types::ModulePath),
    ItemDoesNotExist(types::ModulePath, types::Identifier),
    ItemNotExported(types::ModulePath, types::Identifier),
    UndeclaredVariable(types::Identifier),
    ImplNotOnStruct(Identifier),
    MismatchedAssignmentType {
        target_variable: String,
        variable_type: types::Type,
        assigned_type: types::Type,
    },
    CallingNotCallableItem(types::Type),
}

impl TypeCheckErrorDescription {
    // TODO this should also tell us which module it is in!
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
            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall { target, argument_name } => write!(f, "Incorrect argument type for argument {argument_name} in a call to {target}"),
            TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(name) => write!(f, "Incorrect number of arguments passed to {name}"),
            TypeCheckErrorDescription::FunctionArgumentCannotBeVoid { function_name, argument_name } => write!(f, "Argument {argument_name} in the declaration of {function_name} cannot be of type void"),
            TypeCheckErrorDescription::ModuleDoesNotExist(module_path) => write!(f, "Module {module_path} does not exist"),
            TypeCheckErrorDescription::ItemDoesNotExist(module_path, identifier) => write!(f, "Item {identifier} does not exist in module {module_path}"),
            TypeCheckErrorDescription::ItemNotExported(module_path, identifier) => write!(f, "Item {identifier} exists in module {module_path}, but is not exported"),
            TypeCheckErrorDescription::UndeclaredVariable(identifier) => write!(f, "Variable {identifier} does not exist"),
            TypeCheckErrorDescription::ImplNotOnStruct(identifier) => write!(f, "{identifier} is not a struct, impl is not allowed"),
            TypeCheckErrorDescription::MismatchedAssignmentType { target_variable, variable_type, assigned_type } => write!(f, "Cannot assign value of type {assigned_type} to variiable {target_variable} of typee {variable_type}"),
            TypeCheckErrorDescription::CallingNotCallableItem(identifier) => write!(f, "{identifier} cannot be called"),
        }
    }
}

fn convert_type(type_: &ast::TypeDescription) -> types::Type {
    match type_ {
        ast::TypeDescription::Array(type_description) => {
            types::Type::Array(Box::new(convert_type(type_description)))
        }
        ast::TypeDescription::Named(name) if name == "void" => types::Type::Void,
        ast::TypeDescription::Named(name) => types::Type::Object(types::Identifier(name.clone())),
    }
}

pub fn type_check(program: &Program) -> Result<types::Program, TypeCheckError> {
    let mut modules = HashMap::new();

    for file in &program.0 {
        let mut items = HashMap::new();

        for declaration in &file.declarations {
            match declaration {
                ast::Declaration::Function(function) => {
                    let typed_function = type_check_function(function)?;
                    items.insert(
                        typed_function.name.clone(),
                        types::Item::Function(typed_function),
                    );
                }
                ast::Declaration::Struct(struct_) => {
                    let struct_name = types::Identifier(struct_.name.to_string());
                    let mut fields = vec![];

                    for field in &struct_.fields {
                        fields.push(types::StructField {
                            name: types::Identifier(field.name.clone()),
                            type_: convert_type(&field.type_),
                            static_: false,
                        });
                    }

                    items.insert(
                        struct_name.clone(),
                        types::Item::Struct(types::Struct {
                            name: struct_name,
                            fields,
                            impls: HashMap::new(),
                        }),
                    );
                }
                ast::Declaration::Impl(_) => {} // impls are handled in the second pass, so
                                                // that they can be declared before structs
                                                // that they are for
            };
        }

        let path = types::ModulePath(types::Identifier(file.name.clone()));
        modules.insert(path.clone(), types::Module { items });
    }

    for file in &program.0 {
        for import in &file.imports {
            let module_name = types::ModulePath(types::Identifier(import.path[0].clone()));
            let item_name = types::Identifier(import.path[1].clone());
            let Some(exporting_module) = modules.get_mut(&module_name) else {
                return Err(
                    TypeCheckErrorDescription::ModuleDoesNotExist(module_name).at(import.position)
                );
            };

            let Some(item) = exporting_module.items.get(&item_name) else {
                return Err(
                    TypeCheckErrorDescription::ItemDoesNotExist(module_name, item_name)
                        .at(import.position),
                );
            };

            match item {
                types::Item::Function(function) => {
                    if !function.export {
                        return Err(TypeCheckErrorDescription::ItemNotExported(
                            module_name,
                            item_name,
                        )
                        .at(import.position));
                    }
                    let importing_module_path =
                        types::ModulePath(types::Identifier(file.name.clone()));
                    let Some(importing_module) = modules.get_mut(&importing_module_path) else {
                        return Err(TypeCheckErrorDescription::ModuleDoesNotExist(
                            importing_module_path,
                        )
                        .at(import.position));
                    };

                    importing_module.items.insert(
                        item_name.clone(),
                        types::Item::ImportFunction(types::ImportFunction {
                            path: module_name,
                            item: item_name,
                            location: import.position,
                        }),
                    );
                }
                types::Item::ImportFunction(_) => {
                    return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                        module_name,
                        item_name,
                    )
                    .at(import.position));
                }
                types::Item::Struct(_) => todo!(),
            }
        }

        let module_name = ModulePath(Identifier(file.name.clone()));

        for item in &file.declarations {
            match item {
                ast::Declaration::Function(_) => {}
                ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_) => {
                    let position = item.position();
                    let struct_name = types::Identifier(impl_.struct_name.to_string());
                    let functions = impl_
                        .functions
                        .iter()
                        .map(type_check_function)
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = modules
                        .get_mut(&module_name)
                        .unwrap()
                        .items
                        .get_mut(&struct_name)
                    else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            module_name,
                            struct_name,
                        )
                        .at(position));
                    };

                    let types::Item::Struct(struct_inner) = struct_ else {
                        return Err(
                            TypeCheckErrorDescription::ImplNotOnStruct(struct_name).at(position)
                        );
                    };

                    for function in &functions {
                        struct_inner.fields.push(types::StructField {
                            name: function.name.clone(),
                            type_: types::Type::Callable {
                                self_type: Some(struct_inner.name.clone()),
                                arguments: function.arguments.clone(),
                                return_type: Box::new(function.return_type.clone()),
                            },
                            static_: true,
                        });

                        struct_inner
                            .impls
                            .insert(function.name.clone(), function.clone());
                    }
                }
            }
        }
    }

    for module in modules.values() {
        for declared_item in module.items.values() {
            let types::Item::Function(declared_function) = declared_item else {
                continue;
            };
            //let module = modules.get(declared_function.)
            let ast::FunctionBody::Statements(body_statements, _) = &declared_function.body else {
                continue;
            };

            let mut locals: HashMap<Identifier, types::Type> = HashMap::new();

            for (name, declared_item) in module.items.iter() {
                locals.insert(name.clone(), declared_item.type_(None, &modules));
            }

            for argument in &declared_function.arguments {
                locals.insert(argument.name.clone(), argument.type_.clone());
            }

            for statement in body_statements.iter() {
                match statement {
                    ast::Statement::Expression(expression, _) => {
                        type_check_expression(expression, &locals)?;
                    }
                    ast::Statement::Let(name, type_, expression) => {
                        let expression_type = type_check_expression(expression, &locals)?;
                        let type_ = convert_type(type_);

                        if expression_type != type_ {
                            return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                                target_variable: name.clone(),
                                variable_type: type_,
                                assigned_type: expression_type,
                            }
                            .at(expression.position()));
                        }

                        locals.insert(types::Identifier(name.clone()), type_);
                    }
                }
            }
        }
    }

    Ok(types::Program { modules })
}

fn type_check_function(function: &ast::Function) -> Result<types::Function, TypeCheckError> {
    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(&arg.type_);

        if type_ == types::Type::Void {
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

    Ok(types::Function {
        name: types::Identifier(function.name.clone()),
        arguments,
        body: function.body.clone(),
        export: function.export,
        location: function.position,
        // TODO support actual return types!
        return_type: types::Type::Void,
    })
}

fn type_check_expression(
    expression: &ast::Expression,
    locals: &HashMap<Identifier, types::Type>,
) -> Result<types::Type, TypeCheckError> {
    match expression {
        ast::Expression::FunctionCall {
            target,
            arguments: passed_arguments,
            position,
        } => {
            let target_type = type_check_expression(target, locals)?;

            let types::Type::Callable {
                self_type,
                arguments: callable_arguments,
                return_type,
            } = target_type.clone()
            else {
                return Err(
                    TypeCheckErrorDescription::CallingNotCallableItem(target_type).at(*position),
                );
            };
            let mut callable_arguments = callable_arguments.clone();

            let self_adjustment = if self_type.is_some() { 1 } else { 0 };

            if passed_arguments.len() + self_adjustment != callable_arguments.len() {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    target_type,
                )
                .at(*position));
            }

            if let Some(self_type) = self_type {
                if callable_arguments[0].name == Identifier("self".to_string()) {
                    let mut callable_arguments_iter = callable_arguments.into_iter();
                    let self_argument = callable_arguments_iter.next().unwrap();

                    callable_arguments = callable_arguments_iter.collect();

                    let Some(types::Type::StructDescriptor(target_struct_name, _)) =
                        locals.get(&self_type)
                    else {
                        panic!();
                    };

                    if self_argument.type_ != types::Type::Object(target_struct_name.clone()) {
                        return Err(
                            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                                target: *target.clone(),
                                argument_name: self_argument.name.to_string(),
                            }
                            .at(*position),
                        );
                    }
                }
            }

            for (argument, called_function_argument) in
                passed_arguments.iter().zip(callable_arguments)
            {
                let type_ = type_check_expression(argument, locals)?;
                let expected_type = &called_function_argument.type_;

                if &type_ != expected_type {
                    return Err(
                        TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                            target: *target.clone(),
                            argument_name: called_function_argument.name.to_string(),
                        }
                        .at(argument.position()),
                    );
                }
            }

            Ok(*return_type.clone())
        }
        ast::Expression::Literal(literal, _) => match literal {
            ast::Literal::String(_, _) => {
                Ok(types::Type::Object(types::Identifier("string".to_string())))
            }
        },
        ast::Expression::VariableReference(name, position) => {
            let id = Identifier(name.clone());

            locals
                .get(&id)
                .ok_or_else(|| TypeCheckErrorDescription::UndeclaredVariable(id).at(*position))
                .cloned()
        }
        ast::Expression::StructConstructor(struct_name, position) => {
            let id = Identifier(struct_name.clone());

            let types::Type::StructDescriptor(name, _) = locals
                .get(&id)
                .ok_or_else(|| TypeCheckErrorDescription::UndeclaredVariable(id).at(*position))
                .cloned()
                .unwrap()
            else {
                panic!();
            };

            Ok(types::Type::Object(name))
        }
        ast::Expression::FieldAccess {
            target,
            field_name,
            position: _,
        } => {
            let target = type_check_expression(target, locals)?;

            let types::Type::Object(type_name) = target else {
                panic!("{target:?}")
            };
            let types::Type::StructDescriptor(_, fields) = locals.get(&type_name).unwrap() else {
                panic!()
            };

            // TODO make fields a HashMap so we don't have to .find?
            let field_type = fields
                .iter()
                .find(|x| x.name == types::Identifier(field_name.to_string()))
                .unwrap()
                .type_
                .clone();

            Ok(field_type)
        }
    }
}
