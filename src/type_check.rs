use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{self, SourceRange},
    types::{self, Identifier, ImportFunction, ModulePath},
};

#[derive(Debug)]
pub struct Program(pub Vec<ast::SourceFile>);

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    module: types::ModulePath,
    position: ast::SourceRange,
}

impl Error for TypeCheckError {}
impl Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error {} at {} in module {}",
            self.description, self.position, self.module
        )
    }
}

#[derive(Debug)]
pub enum TypeCheckErrorDescription {
    UnexpectedArgumentTypeInFunctionCall {
        target: ast::Expression,
        argument_name: Identifier,
    },
    IncorrectNumberOfArgumentsPassed(types::Type),
    FunctionArgumentCannotBeVoid {
        function_name: Identifier,
        argument_name: Identifier,
    },
    ModuleDoesNotExist(types::ModulePath),
    ItemDoesNotExist(types::ModulePath, types::Identifier),
    ItemNotExported(types::ModulePath, types::Identifier),
    UndeclaredVariable(types::Identifier),
    ImplNotOnStruct(Identifier),
    MismatchedAssignmentType {
        target_variable: Identifier,
        variable_type: types::Type,
        assigned_type: types::Type,
    },
    CallingNotCallableItem(types::Type),
}

impl TypeCheckErrorDescription {
    fn at(self, module: types::ModulePath, position: ast::SourceRange) -> TypeCheckError {
        TypeCheckError {
            description: self,
            module,
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
        ast::TypeDescription::Named(name) => types::Type::Object(types::Identifier::parse(name)),
    }
}

#[derive(Debug, Clone)]
struct DeclaredArgument {
    name: Identifier,
    type_: types::Type,
    position: SourceRange,
}

#[derive(Debug, Clone)]
struct DeclaredFunction {
    name: Identifier,
    self_type: Option<Identifier>,
    arguments: Vec<DeclaredArgument>,
    return_type: types::Type,
    ast: ast::Function,
    // TODO this should be a higher level concept that applies to all items
    export: bool,
}

#[derive(Debug, Clone)]
struct DeclaredStructField {
    type_: types::Type,
    static_: bool,
}

// TODO support aliased imports
#[derive(Debug, Clone)]
struct DeclaredImport {
    from: ModulePath,
    item: Box<DeclaredItem>,
    position: SourceRange,
}

#[derive(Debug, Clone)]
enum DeclaredItem {
    Function(DeclaredFunction),
    Struct {
        name: Identifier,
        fields: HashMap<Identifier, DeclaredStructField>,
    },
    Import(DeclaredImport),
}

impl DeclaredItem {
    fn type_(&self) -> types::Type {
        match self {
            DeclaredItem::Function(declared_function) => types::Type::Callable {
                self_type: declared_function.self_type.clone(),
                arguments: declared_function
                    .arguments
                    .iter()
                    .map(|declaration| types::Argument {
                        name: declaration.name.clone(),
                        type_: declaration.type_.clone(),
                        position: declaration.position,
                    })
                    .collect(),
                return_type: Box::new(declared_function.return_type.clone()),
            },
            DeclaredItem::Struct { name, fields, .. } => types::Type::StructDescriptor(
                name.clone(),
                fields
                    .iter()
                    .map(|(name, declaration)| types::StructField {
                        name: name.clone(),
                        type_: declaration.type_.clone(),
                        static_: declaration.static_,
                    })
                    .collect(),
            ),
            DeclaredItem::Import(DeclaredImport { item, .. }) => item.type_(),
        }
    }
}

pub fn type_check(program: &Program) -> Result<types::Program, TypeCheckError> {
    let mut modules = HashMap::new();

    for file in &program.0 {
        let mut items: HashMap<Identifier, DeclaredItem> = HashMap::new();
        let module_path = ModulePath::parse(&file.name);

        for declaration in &file.declarations {
            match declaration {
                ast::Declaration::Function(function) => {
                    let declaration =
                        type_check_function_declaration(function, None, module_path.clone())?;

                    items.insert(
                        Identifier::parse(&function.name),
                        DeclaredItem::Function(declaration.clone()),
                    );
                }
                ast::Declaration::Struct(struct_) => {
                    let mut fields = HashMap::new();
                    // TODO check the types exist, possibly in separate pass
                    for field in &struct_.fields {
                        fields.insert(
                            types::Identifier::parse(&field.name),
                            DeclaredStructField {
                                type_: convert_type(&field.type_),
                                static_: false,
                            },
                        );
                    }
                    let name = Identifier::parse(&struct_.name);
                    items.insert(name.clone(), DeclaredItem::Struct { name, fields });
                }
                ast::Declaration::Impl(_) => {} // impls are handled in the second pass, so
                                                // that they can be declared before structs
                                                // that they are for
            };
        }

        let path = types::ModulePath::parse(&file.name.clone());
        modules.insert(path.clone(), items);
    }

    let mut impls: HashMap<(ModulePath, Identifier), HashMap<Identifier, types::Function>> =
        HashMap::new();

    for file in &program.0 {
        let module_path = ModulePath::parse(&file.name);

        for import in &file.imports {
            let (name, path) = import.path.split_last().unwrap();
            let exporting_module_name =
                types::ModulePath::from_parts(path.iter().map(String::as_str));
            let item_name = types::Identifier::parse(name);

            let Some(exporting_module) = modules.get(&exporting_module_name) else {
                return Err(
                    TypeCheckErrorDescription::ModuleDoesNotExist(exporting_module_name)
                        .at(module_path, import.position),
                );
            };

            let Some(item) = exporting_module.get(&item_name).cloned() else {
                return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                    exporting_module_name,
                    item_name,
                )
                .at(module_path, import.position));
            };

            match item {
                DeclaredItem::Function(DeclaredFunction { export, .. }) => {
                    if !export {
                        return Err(TypeCheckErrorDescription::ItemNotExported(
                            exporting_module_name,
                            item_name,
                        )
                        .at(module_path, import.position));
                    }
                    let importing_module_path = types::ModulePath::parse(&file.name);
                    let Some(importing_module) = modules.get_mut(&importing_module_path) else {
                        return Err(TypeCheckErrorDescription::ModuleDoesNotExist(
                            importing_module_path,
                        )
                        .at(module_path, import.position));
                    };

                    importing_module.insert(
                        // TODO support aliases here
                        item_name.clone(),
                        DeclaredItem::Import(DeclaredImport {
                            from: exporting_module_name,
                            item: Box::new(item.clone()),
                            position: import.position,
                        }),
                    );
                }
                DeclaredItem::Import(_) => {
                    return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                        exporting_module_name,
                        item_name,
                    )
                    .at(module_path, import.position));
                }
                DeclaredItem::Struct { .. } => todo!(),
            }
        }

        let module_name = ModulePath::parse(&file.name);
        let mut globals = HashMap::new();

        for (name, declared_item) in modules.get(&module_name).unwrap().iter() {
            globals.insert(name.clone(), declared_item.type_());
        }

        for item in &file.declarations {
            match item {
                ast::Declaration::Function(_) => {}
                ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_) => {
                    let position = item.position();
                    let struct_name = types::Identifier::parse(&impl_.struct_name);
                    let functions = impl_
                        .functions
                        .iter()
                        .map(|f| {
                            let checked_declaration = type_check_function_declaration(
                                f,
                                Some(struct_name.clone()),
                                module_path.clone(),
                            )?;

                            type_check_function(&checked_declaration, &globals, module_path.clone())
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) =
                        modules.get_mut(&module_name).unwrap().get_mut(&struct_name)
                    else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            module_name,
                            struct_name,
                        )
                        .at(module_path, position));
                    };

                    let DeclaredItem::Struct { fields, name, .. } = struct_ else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(struct_name)
                            .at(module_path, position));
                    };

                    for function in &functions {
                        fields.insert(
                            function.name.clone(),
                            DeclaredStructField {
                                type_: types::Type::Callable {
                                    self_type: Some(name.clone()),
                                    arguments: function.arguments.clone(),
                                    return_type: Box::new(function.return_type.clone()),
                                },
                                static_: true,
                            },
                        );

                        impls
                            .entry((module_name.clone(), struct_name.clone()))
                            .or_default()
                            .insert(function.name.clone(), function.clone());
                    }
                }
            }
        }
    }

    let mut checked_modules: HashMap<ModulePath, types::Module> = HashMap::new();

    for (module_name, module) in modules {
        let mut current_module_items: HashMap<Identifier, types::Item> = HashMap::new();

        let mut globals = HashMap::new();

        for (name, declared_item) in module.iter() {
            globals.insert(name.clone(), declared_item.type_());
        }
        for declared_item in module.values() {
            match declared_item {
                DeclaredItem::Function(declared_function) => {
                    let body =
                        type_check_function(declared_function, &globals, module_name.clone())?;

                    current_module_items
                        .insert(declared_function.name.clone(), types::Item::Function(body));
                }
                DeclaredItem::Struct { name, fields } => {
                    current_module_items.insert(
                        name.clone(),
                        types::Item::Struct(types::Struct {
                            name: name.clone(),
                            fields: fields
                                .iter()
                                .map(|(name, declaration)| types::StructField {
                                    name: name.clone(),
                                    type_: declaration.type_.clone(),
                                    static_: declaration.static_,
                                })
                                .collect(),
                            impls: impls
                                .get(&(module_name.clone(), name.clone()))
                                .cloned()
                                .unwrap_or_else(HashMap::new),
                        }),
                    );
                }
                DeclaredItem::Import(DeclaredImport {
                    from,
                    item,
                    position,
                }) => {
                    match item.as_ref() {
                        DeclaredItem::Function(declared_function) => current_module_items.insert(
                            declared_function.name.clone(),
                            types::Item::ImportFunction(ImportFunction {
                                path: from.clone(),
                                item: declared_function.name.clone(),
                                location: *position,
                            }),
                        ),
                        DeclaredItem::Struct { .. } => todo!(),
                        DeclaredItem::Import(_) => todo!(),
                    };
                }
            }
        }
        checked_modules.insert(
            module_name.clone(),
            types::Module {
                items: current_module_items,
            },
        );
    }

    Ok(types::Program {
        modules: checked_modules,
    })
}

fn type_check_function(
    declared_function: &DeclaredFunction,
    globals: &HashMap<Identifier, types::Type>,
    module_path: ModulePath,
) -> Result<types::Function, TypeCheckError> {
    let mut locals: HashMap<Identifier, types::Type> = globals.clone();
    for argument in &declared_function.arguments {
        locals.insert(argument.name.clone(), argument.type_.clone());
    }
    let body = match &declared_function.ast.body {
        ast::FunctionBody::Statements(body_statements, _) => {
            let mut checked_statements = vec![];

            for statement in body_statements.iter() {
                let checked_statement = match statement {
                    ast::Statement::Expression(expression, _) => types::Statement::Expression(
                        type_check_expression(expression, &locals, module_path.clone())?,
                    ),

                    ast::Statement::Let(name, type_, expression) => {
                        let checked_expression =
                            type_check_expression(expression, &locals, module_path.clone())?;
                        let type_ = convert_type(type_);

                        if checked_expression.type_ != type_ {
                            return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                                target_variable: types::Identifier::parse(name),
                                variable_type: type_,
                                assigned_type: checked_expression.type_,
                            }
                            .at(module_path, expression.position()));
                        }

                        locals.insert(types::Identifier::parse(name), type_);

                        types::Statement::Let(types::LetStatement {
                            binding: Identifier::parse(name),
                            value: checked_expression,
                        })
                    }
                };

                checked_statements.push(checked_statement);
            }

            types::FunctionBody::Statements(checked_statements)
        }
        ast::FunctionBody::Extern(_) => types::FunctionBody::Extern,
    };
    Ok(types::Function {
        name: declared_function.name.clone(),
        arguments: declared_function
            .arguments
            .iter()
            .map(|argument| types::Argument {
                name: argument.name.clone(),
                type_: argument.type_.clone(),
                position: argument.position,
            })
            .collect(),
        return_type: declared_function.return_type.clone(),
        body,
        export: declared_function.export,
        location: declared_function.ast.position,
    })
}

fn type_check_function_declaration(
    function: &ast::Function,
    self_type: Option<Identifier>,
    module_path: ModulePath,
) -> Result<DeclaredFunction, TypeCheckError> {
    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(&arg.type_);

        if type_ == types::Type::Void {
            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                function_name: types::Identifier::parse(&function.name),
                argument_name: types::Identifier::parse(&arg.name),
            }
            .at(module_path, arg.position));
        }

        arguments.push(DeclaredArgument {
            name: types::Identifier::parse(&arg.name),
            type_: convert_type(&arg.type_),
            position: arg.position,
        });
    }

    Ok(DeclaredFunction {
        name: Identifier::parse(&function.name),
        arguments,
        return_type: convert_type(&function.return_type),
        ast: function.clone(),
        export: function.export,
        self_type,
    })
}

fn type_check_expression(
    expression: &ast::Expression,
    locals: &HashMap<Identifier, types::Type>,
    module_path: ModulePath,
) -> Result<types::Expression, TypeCheckError> {
    match expression {
        ast::Expression::FunctionCall {
            target,
            arguments: passed_arguments,
            position,
        } => {
            let checked_target = type_check_expression(target, locals, module_path.clone())?;

            let types::Type::Callable {
                ref self_type,
                arguments: ref callable_arguments,
                ref return_type,
            } = checked_target.type_
            else {
                return Err(TypeCheckErrorDescription::CallingNotCallableItem(
                    checked_target.type_.clone(),
                )
                .at(module_path, *position));
            };
            let mut callable_arguments = callable_arguments.clone();

            let self_adjustment = if self_type.is_some() { 1 } else { 0 };

            if passed_arguments.len() + self_adjustment != callable_arguments.len() {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    checked_target.type_.clone(),
                )
                .at(module_path.clone(), *position));
            }

            let mut checked_arguments = vec![];
            if let Some(self_type) = self_type {
                if callable_arguments[0].name == Identifier::parse("self") {
                    let mut callable_arguments_iter = callable_arguments.into_iter();
                    let self_argument = callable_arguments_iter.next().unwrap();

                    callable_arguments = callable_arguments_iter.collect();

                    let Some(full_self_type @ types::Type::StructDescriptor(target_struct_name, _)) =
                        locals.get(self_type)
                    else {
                        todo!();
                    };

                    if self_argument.type_ != types::Type::Object(target_struct_name.clone()) {
                        return Err(
                            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                                target: *target.clone(),
                                argument_name: self_argument.name,
                            }
                            .at(module_path.clone(), *position),
                        );
                    }

                    checked_arguments.push(types::Expression {
                        position: self_argument.position,
                        type_: full_self_type.clone(),
                        kind: types::ExpressionKind::SelfAccess,
                    });
                }
            }

            for (argument, called_function_argument) in
                passed_arguments.iter().zip(callable_arguments)
            {
                let checked_argument =
                    type_check_expression(argument, locals, module_path.clone())?;
                let expected_type = &called_function_argument.type_;

                if &checked_argument.type_ != expected_type {
                    return Err(
                        TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                            target: *target.clone(),
                            argument_name: called_function_argument.name,
                        }
                        .at(module_path.clone(), argument.position()),
                    );
                }

                checked_arguments.push(checked_argument);
            }

            Ok(types::Expression {
                position: *position,
                type_: *return_type.clone(),
                kind: types::ExpressionKind::FunctionCall {
                    target: Box::new(checked_target),
                    arguments: checked_arguments,
                },
            })
        }
        ast::Expression::Literal(literal, _) => match literal {
            ast::Literal::String(value, position) => Ok(types::Expression {
                position: *position,
                type_: types::Type::Object(types::Identifier::parse("string")),
                kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
            }),
        },
        ast::Expression::VariableReference(name, position) => {
            let id = Identifier::parse(name);

            let value_type = locals
                .get(&id)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(module_path.clone(), *position)
                })
                .cloned()?;

            Ok(types::Expression {
                position: *position,
                type_: value_type,
                kind: types::ExpressionKind::VariableAccess(id),
            })
        }
        ast::Expression::StructConstructor(struct_name, position) => {
            let id = Identifier::parse(struct_name);

            let types::Type::StructDescriptor(name, _) = locals
                .get(&id)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(module_path.clone(), *position)
                })
                .cloned()
                .unwrap()
            else {
                panic!();
            };

            Ok(types::Expression {
                position: *position,
                type_: types::Type::Object(name),
                kind: types::ExpressionKind::StructConstructor(id),
            })
        }
        ast::Expression::FieldAccess {
            target,
            field_name,
            position,
        } => {
            let target = type_check_expression(target, locals, module_path.clone())?;

            let types::Type::Object(ref type_name) = target.type_ else {
                todo!("{target:?}")
            };
            let types::Type::StructDescriptor(_, fields) = locals.get(type_name).unwrap() else {
                todo!()
            };

            let field_name = types::Identifier::parse(field_name);

            // TODO make fields a HashMap so we don't have to .find?
            let field_type = fields
                .iter()
                .find(|x| x.name == field_name)
                .unwrap()
                .type_
                .clone();

            Ok(types::Expression {
                position: *position,
                type_: field_type,
                kind: types::ExpressionKind::FieldAccess {
                    target: Box::new(target),
                    field: field_name,
                },
            })
        }
    }
}
