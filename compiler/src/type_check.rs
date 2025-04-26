use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{self, SourceRange},
    name_mangler::{mangle_field, mangle_item, nomangle_item},
    parse,
    types::{self, Identifier, Import, ModulePath},
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
            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                target,
                argument_name,
            } => write!(
                f,
                "Incorrect argument type for argument {argument_name} in a call to {target}"
            ),
            TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(name) => {
                write!(f, "Incorrect number of arguments passed to {name}")
            }
            TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                function_name,
                argument_name,
            } => write!(
                f,
                "Argument {argument_name} in the declaration of {function_name} cannot be of type void"
            ),
            TypeCheckErrorDescription::ModuleDoesNotExist(module_path) => {
                write!(f, "Module {module_path} does not exist")
            }
            TypeCheckErrorDescription::ItemDoesNotExist(module_path, identifier) => write!(
                f,
                "Item {identifier} does not exist in module {module_path}"
            ),
            TypeCheckErrorDescription::ItemNotExported(module_path, identifier) => write!(
                f,
                "Item {identifier} exists in module {module_path}, but is not exported"
            ),
            TypeCheckErrorDescription::UndeclaredVariable(identifier) => {
                write!(f, "Variable {identifier} does not exist")
            }
            TypeCheckErrorDescription::ImplNotOnStruct(identifier) => {
                write!(f, "{identifier} is not a struct, impl is not allowed")
            }
            TypeCheckErrorDescription::MismatchedAssignmentType {
                target_variable,
                variable_type,
                assigned_type,
            } => write!(
                f,
                "Cannot assign value of type {assigned_type} to variiable {target_variable} of typee {variable_type}"
            ),
            TypeCheckErrorDescription::CallingNotCallableItem(identifier) => {
                write!(f, "{identifier} cannot be called")
            }
        }
    }
}

fn convert_type(type_: &ast::TypeDescription) -> types::Type {
    match type_ {
        ast::TypeDescription::Array(type_description) => {
            types::Type::Array(Box::new(convert_type(type_description)))
        }
        ast::TypeDescription::Named(name) if name == "void" => types::Type::Void,
        ast::TypeDescription::Named(name) if name == "u64" => types::Type::U64,
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
    item: Identifier,
    position: SourceRange,
}

#[derive(Debug, Clone)]
enum DeclaredItem {
    Function(DeclaredFunction),
    Struct {
        name: Identifier,
        module: ModulePath,
        fields: HashMap<Identifier, DeclaredStructField>,
        // TODO visibility should be handled at module level
        export: bool,
    },
    Import(DeclaredImport),
}

impl DeclaredItem {
    fn type_(
        &self,
        declared_modules: &HashMap<ModulePath, HashMap<Identifier, DeclaredItem>>,
    ) -> types::Type {
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
            DeclaredItem::Struct {
                name: struct_name,
                module,
                fields,
                ..
            } => types::Type::StructDescriptor(
                struct_name.clone(),
                fields
                    .iter()
                    .map(|(field_name, declaration)| types::StructField {
                        name: field_name.clone(),
                        mangled_name: mangle_field(
                            module.clone(),
                            struct_name.clone(),
                            field_name.clone(),
                        ),
                        type_: declaration.type_.clone(),
                        static_: declaration.static_,
                    })
                    .collect(),
            ),
            DeclaredItem::Import(DeclaredImport { from, item, .. }) => declared_modules
                .get(from)
                .unwrap()
                .get(item)
                .unwrap()
                .type_(declared_modules),
        }
    }
}

pub fn type_check(mut program: Program) -> Result<types::Program, TypeCheckError> {
    let parsed_std = parse::parse_file("std", include_str!("../stdlib/std.lng")).unwrap();
    program.0.push(parsed_std);

    let mut declared_modules: HashMap<ModulePath, HashMap<Identifier, DeclaredItem>> =
        HashMap::new();
    let mut declared_impls: HashMap<
        (ModulePath, Identifier),
        HashMap<Identifier, DeclaredFunction>,
    > = HashMap::new();

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
                    items.insert(
                        name.clone(),
                        DeclaredItem::Struct {
                            name,
                            module: module_path.clone(),
                            fields,
                            export: true, // TODO structs should also have an explicit "export"
                                          // keyword that sets this
                        },
                    );
                }
                ast::Declaration::Impl(_) => {}
            };
        }

        let path = types::ModulePath::parse(&file.name.clone());
        declared_modules.insert(path.clone(), items);
    }

    let mut impls: HashMap<(ModulePath, Identifier), HashMap<Identifier, types::Function>> =
        HashMap::new();

    for file in &program.0 {
        let module_path = ModulePath::parse(&file.name);
        for declaration in &file.declarations {
            let position = declaration.position();

            match declaration {
                ast::Declaration::Function(_) => {}
                ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_) => {
                    let struct_name = types::Identifier::parse(&impl_.struct_name);
                    let functions = impl_
                        .functions
                        .iter()
                        .map(|f| {
                            type_check_function_declaration(
                                f,
                                Some(struct_name.clone()),
                                module_path.clone(),
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = declared_modules
                        .get_mut(&module_path)
                        .unwrap()
                        .get_mut(&struct_name)
                    else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            module_path.clone(),
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
                                    arguments: function
                                        .arguments
                                        .iter()
                                        .map(|x| types::Argument {
                                            name: x.name.clone(),
                                            type_: x.type_.clone(),
                                            position,
                                        })
                                        .collect(),
                                    return_type: Box::new(function.return_type.clone()),
                                },
                                static_: true,
                            },
                        );

                        declared_impls
                            .entry((module_path.clone(), struct_name.clone()))
                            .or_default()
                            .insert(function.name.clone(), function.clone());
                    }
                }
            }
        }
    }

    for file in &program.0 {
        let module_path = ModulePath::parse(&file.name);

        // TODO the system imports should be defined somewhere else so they're in one obvious
        // place
        let mut system_imports = vec![];
        if file.name != "std" {
            system_imports.push(ast::Import {
                path: vec!["std".to_string(), "u64".to_string()],
                position: SourceRange::Internal,
            });
        }

        for import in file.imports.iter().chain(system_imports.iter()) {
            let (name, path) = import.path.split_last().unwrap();
            let exporting_module_name =
                types::ModulePath::from_parts(path.iter().map(String::as_str));
            let item_name = types::Identifier::parse(name);

            let Some(exporting_module) = declared_modules.get(&exporting_module_name) else {
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
                DeclaredItem::Function(DeclaredFunction { export, .. })
                | DeclaredItem::Struct { export, .. } => {
                    if !export {
                        return Err(TypeCheckErrorDescription::ItemNotExported(
                            exporting_module_name,
                            item_name,
                        )
                        .at(module_path, import.position));
                    }
                    let importing_module_path = types::ModulePath::parse(&file.name);
                    let Some(importing_module) = declared_modules.get_mut(&importing_module_path)
                    else {
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
                            item: item_name,
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
            }
        }

        let module_name = ModulePath::parse(&file.name);
        let mut globals = HashMap::new();

        for (name, declared_item) in declared_modules.get(&module_name).unwrap().iter() {
            globals.insert(name.clone(), declared_item.type_(&declared_modules));
        }

        for item in &file.declarations {
            match item {
                ast::Declaration::Function(_) => {}
                ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_) => {
                    let position = item.position();
                    let struct_name = types::Identifier::parse(&impl_.struct_name);
                    let declared_impl: &HashMap<Identifier, DeclaredFunction> = declared_impls
                        .get(&(module_path.clone(), struct_name.clone()))
                        .unwrap();

                    let functions = declared_impl
                        .iter()
                        .map(|(_, f)| type_check_function(f, &globals, module_path.clone()))
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = declared_modules
                        .get_mut(&module_name)
                        .unwrap()
                        .get_mut(&struct_name)
                    else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            module_name,
                            struct_name,
                        )
                        .at(module_path, position));
                    };

                    let DeclaredItem::Struct { .. } = struct_ else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(struct_name)
                            .at(module_path, position));
                    };

                    for function in &functions {
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

    for (module_name, module) in &declared_modules {
        let mut current_module_items: HashMap<Identifier, types::Item> = HashMap::new();

        // TODO do we really need to declare globals twice?
        let mut globals = HashMap::new();

        for (name, declared_item) in module.iter() {
            globals.insert(name.clone(), declared_item.type_(&declared_modules));
        }

        for declared_item in module.values() {
            match declared_item {
                DeclaredItem::Function(declared_function) => {
                    let body =
                        type_check_function(declared_function, &globals, module_name.clone())?;

                    current_module_items
                        .insert(declared_function.name.clone(), types::Item::Function(body));
                }
                DeclaredItem::Struct {
                    name: struct_name,
                    fields,
                    module,
                    export: _,
                } => {
                    current_module_items.insert(
                        struct_name.clone(),
                        types::Item::Struct(types::Struct {
                            name: struct_name.clone(),
                            mangled_name: mangle_item(module.clone(), struct_name.clone()),
                            fields: fields
                                .iter()
                                .map(|(field_name, declaration)| types::StructField {
                                    name: field_name.clone(),
                                    type_: declaration.type_.clone(),
                                    static_: declaration.static_,
                                    mangled_name: mangle_field(
                                        module.clone(),
                                        struct_name.clone(),
                                        field_name.clone(),
                                    ),
                                })
                                .collect(),
                            impls: impls
                                .get(&(module_name.clone(), struct_name.clone()))
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
                    match declared_modules.get(from).unwrap().get(item).unwrap() {
                        DeclaredItem::Struct {
                            name: item_name, ..
                        }
                        | DeclaredItem::Function(DeclaredFunction {
                            name: item_name, ..
                        }) => current_module_items.insert(
                            item_name.clone(),
                            types::Item::Import(Import {
                                path: from.clone(),
                                item: item_name.clone(),
                                location: *position,
                            }),
                        ),
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
                            .at(module_path, expression.position));
                        }

                        locals.insert(types::Identifier::parse(name), type_);

                        types::Statement::Let(types::LetStatement {
                            binding: Identifier::parse(name),
                            value: checked_expression,
                        })
                    }
                    ast::Statement::Return(expression, _) => {
                        // TODO verify it matches the declared function type for the function!
                        // TODO verify that all paths return a value
                        let checked_expression =
                            type_check_expression(expression, &locals, module_path.clone())?;

                        types::Statement::Return(checked_expression)
                    }
                };

                checked_statements.push(checked_statement);
            }

            types::FunctionBody::Statements(checked_statements)
        }
        ast::FunctionBody::Extern(_) => types::FunctionBody::Extern,
    };

    let mangled_name = {
        if matches!(body, types::FunctionBody::Extern) {
            nomangle_item(declared_function.name.clone())
        } else {
            match &declared_function.self_type {
                Some(self_type) => mangle_field(
                    module_path.clone(),
                    self_type.clone(),
                    declared_function.name.clone(),
                ),
                None => mangle_item(module_path.clone(), declared_function.name.clone()),
            }
        }
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
        mangled_name,
    })
}

fn type_check_function_declaration(
    function: &ast::Function,
    self_type: Option<Identifier>,
    module_path: ModulePath,
) -> Result<DeclaredFunction, TypeCheckError> {
    let name = types::Identifier::parse(&function.name);
    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(&arg.type_);

        if type_ == types::Type::Void {
            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                function_name: name.clone(),
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
    let position = expression.position;

    match &expression.kind {
        ast::ExpressionKind::FunctionCall {
            target,
            arguments: passed_arguments,
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
                .at(module_path, position));
            };
            let mut callable_arguments = callable_arguments.clone();

            let self_adjustment = if self_type.is_some() { 1 } else { 0 };

            if passed_arguments.len() + self_adjustment != callable_arguments.len() {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    checked_target.type_.clone(),
                )
                .at(module_path.clone(), position));
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

                    // TODO the struct descriptor should have a method that returns its instance
                    // type, so that this code does not rot
                    let expected_type = match target_struct_name.raw() {
                        "u64" => types::Type::U64,
                        _ => types::Type::Object(target_struct_name.clone()),
                    };

                    if self_argument.type_ != expected_type {
                        return Err(
                            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                                target: *target.clone(),
                                argument_name: self_argument.name,
                            }
                            .at(module_path.clone(), position),
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
                        .at(module_path.clone(), argument.position),
                    );
                }

                checked_arguments.push(checked_argument);
            }

            Ok(types::Expression {
                position,
                type_: *return_type.clone(),
                kind: types::ExpressionKind::FunctionCall {
                    target: Box::new(checked_target),
                    arguments: checked_arguments,
                },
            })
        }
        ast::ExpressionKind::Literal(literal) => match literal {
            ast::Literal::String(value, _) => Ok(types::Expression {
                position,
                type_: types::Type::Object(types::Identifier::parse("string")),
                kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
            }),
            ast::Literal::UnsignedInteger(value) => Ok(types::Expression {
                position,
                type_: types::Type::U64,
                kind: types::ExpressionKind::Literal(types::Literal::UnsignedInteger(*value)),
            }),
        },
        ast::ExpressionKind::VariableReference(name) => {
            let id = Identifier::parse(name);

            let value_type = locals
                .get(&id)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(module_path.clone(), position)
                })
                .cloned()?;

            Ok(types::Expression {
                position,
                type_: value_type,
                kind: types::ExpressionKind::VariableAccess(id),
            })
        }
        ast::ExpressionKind::StructConstructor(struct_name) => {
            let id = Identifier::parse(struct_name);

            let types::Type::StructDescriptor(name, _) = locals
                .get(&id)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(module_path.clone(), position)
                })
                .cloned()
                .unwrap()
            else {
                panic!();
            };

            Ok(types::Expression {
                position,
                type_: types::Type::Object(name),
                kind: types::ExpressionKind::StructConstructor(id),
            })
        }
        ast::ExpressionKind::FieldAccess { target, field_name } => {
            let target = type_check_expression(target, locals, module_path.clone())?;

            let type_name = match &target.type_ {
                types::Type::Void => todo!(),
                types::Type::Object(identifier) => identifier,
                types::Type::Array(_) => todo!(),
                types::Type::StructDescriptor(_, _) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => &Identifier::parse("u64"),
                types::Type::Pointer => todo!(),
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
                position,
                type_: field_type,
                kind: types::ExpressionKind::FieldAccess {
                    target: Box::new(target),
                    field: field_name,
                },
            })
        }
    }
}
