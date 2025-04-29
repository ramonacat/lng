mod declarations;

use std::{collections::HashMap, error::Error, fmt::Display};

use declarations::{
    DeclaredArgument, DeclaredAssociatedFunction, DeclaredFunction, DeclaredFunctionDefinition,
    DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule, DeclaredStruct,
    DeclaredStructField,
};

use crate::{
    ast,
    compile::ErrorLocation,
    std::{TYPE_NAME_STRING, TYPE_NAME_U64},
    types::{self, FQName},
};

impl types::Item {
    fn type_(&self, root_module: &DeclaredModule) -> types::Type {
        match &self.kind {
            types::ItemKind::Function(function) => function.type_(),
            types::ItemKind::Struct(struct_) => {
                types::Type::StructDescriptor(types::StructDescriptorType {
                    name: struct_.name,
                    fields: struct_.fields.clone(),
                })
            }
            types::ItemKind::Import(import) => root_module
                .get_item(import.imported_item)
                .unwrap()
                .type_(root_module),
            types::ItemKind::Module(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    location: ErrorLocation,
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
        target: ast::Expression,
        argument_name: types::Identifier,
        expected_type: types::Type,
        actual_type: types::Type,
    },
    IncorrectNumberOfArgumentsPassed(types::Type),
    FunctionArgumentCannotBeVoid {
        argument_name: types::Identifier,
    },
    ModuleDoesNotExist(types::FQName),
    ItemDoesNotExist(types::FQName),
    ItemNotExported(types::FQName, types::Identifier),
    UndeclaredVariable(types::Identifier),
    ImplNotOnStruct(types::FQName),
    MismatchedAssignmentType {
        target_variable: types::Identifier,
        variable_type: types::Type,
        assigned_type: types::Type,
    },
    CallingNotCallableItem(types::Type),
    MismatchedReturnType {
        actual: types::Type,
        expected: types::Type,
    },
}

impl TypeCheckErrorDescription {
    const fn at(self, location: ErrorLocation) -> TypeCheckError {
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

fn convert_type(module: types::FQName, type_: &ast::TypeDescription) -> types::Type {
    match type_ {
        ast::TypeDescription::Array(type_description) => {
            types::Type::Array(Box::new(convert_type(module, type_description)))
        }
        ast::TypeDescription::Named(name) if name == "()" => types::Type::Unit,
        ast::TypeDescription::Named(name) if name == "u64" => types::Type::U64,
        // TODO instead of special-casing the types, do an automatic import?
        ast::TypeDescription::Named(name) if name == "string" => {
            types::Type::Object(*TYPE_NAME_STRING)
        }
        ast::TypeDescription::Named(name) => {
            types::Type::Object(module.with_part(types::Identifier::parse(name)))
        }
    }
}

// TODO split into smaller functions
#[allow(clippy::too_many_lines)]
pub fn type_check(
    program: &Vec<ast::SourceFile>,
    std: Option<&types::Module>,
) -> Result<types::Module, TypeCheckError> {
    let mut root_module_declaration = DeclaredModule::new();

    if let Some(std) = std {
        for (item_name, item) in std.items() {
            let visibility = item.visibility;

            root_module_declaration.declare(
                item_name,
                DeclaredItem {
                    kind: DeclaredItemKind::Predeclared(item.clone()),
                    visibility,
                },
            );
        }
    }

    // this is equivalent-ish to topo-sort, as fewer parts in the name means it is higher in the
    // hierarchy (i.e. main.test will definitely appear after main)
    let mut modules_to_declare = program
        .iter()
        .map(|x| types::FQName::parse(&x.name))
        .collect::<Vec<_>>();
    modules_to_declare.sort_by_key(|name| name.len());

    for module_path in modules_to_declare {
        root_module_declaration.declare(
            module_path,
            DeclaredItem {
                kind: DeclaredItemKind::Module(DeclaredModule::new()),
                // TODO how do we determine the visibility for modules?
                visibility: types::Visibility::Export,
            },
        );
    }

    for file in program {
        let module_path = types::FQName::parse(&file.name);

        for declaration in &file.declarations {
            match &declaration.kind {
                ast::DeclarationKind::Function(function) => {
                    let function_declaration = type_check_function_declaration(
                        function,
                        module_path,
                        declaration.position,
                    )?;

                    root_module_declaration.declare(
                        module_path.with_part(types::Identifier::parse(&function.name)),
                        DeclaredItem {
                            kind: DeclaredItemKind::Function(function_declaration),
                            visibility: convert_visibility(declaration.visibility),
                        },
                    );
                }
                ast::DeclarationKind::Struct(struct_) => {
                    let mut fields = HashMap::new();
                    // TODO check the types exist, possibly in separate pass
                    for field in &struct_.fields {
                        fields.insert(
                            types::Identifier::parse(&field.name),
                            DeclaredStructField {
                                type_: convert_type(module_path, &field.type_),

                                static_: false,
                            },
                        );
                    }
                    let name = module_path.with_part(types::Identifier::parse(&struct_.name));
                    root_module_declaration.declare(
                        name,
                        DeclaredItem {
                            kind: DeclaredItemKind::Struct(DeclaredStruct { name, fields }),
                            visibility: convert_visibility(declaration.visibility),
                        },
                    );
                }
                ast::DeclarationKind::Impl(_) => {}
            }
        }
    }
    // TODO check if we still need this
    let mut declared_impls: HashMap<types::FQName, DeclaredAssociatedFunction> = HashMap::new();

    for file in program {
        let module_path = types::FQName::parse(&file.name);
        for declaration in &file.declarations {
            let position = declaration.position;

            match &declaration.kind {
                ast::DeclarationKind::Function(_) | ast::DeclarationKind::Struct(_) => {}
                ast::DeclarationKind::Impl(impl_declaration) => {
                    let struct_name = types::Identifier::parse(&impl_declaration.struct_name);
                    let struct_path = module_path.with_part(struct_name);
                    let error_location = ErrorLocation::Position(struct_path, position);

                    let functions = impl_declaration
                        .functions
                        .iter()
                        .map(|f| {
                            type_check_associated_function_declaration(f, struct_path, position)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = root_module_declaration.get_item_mut(struct_path) else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(struct_path)
                            .at(error_location));
                    };

                    let DeclaredItem {
                        kind: DeclaredItemKind::Struct(DeclaredStruct { fields, .. }),
                        ..
                    } = struct_
                    else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(struct_path)
                            .at(error_location));
                    };

                    for function in &functions {
                        fields.insert(
                            function.name,
                            DeclaredStructField {
                                type_: types::Type::Callable {
                                    arguments: function
                                        .definition
                                        .arguments
                                        .iter()
                                        .map(|x| types::Argument {
                                            name: x.name,
                                            type_: x.type_.clone(),
                                            position,
                                        })
                                        .collect(),
                                    return_type: Box::new(function.definition.return_type.clone()),
                                },
                                static_: true,
                            },
                        );

                        declared_impls
                            .insert(struct_path.with_part(function.name), function.clone());
                    }
                }
            }
        }
    }

    let mut impls: HashMap<types::FQName, types::AssociatedFunction> = HashMap::new();

    for file in program {
        let module_path = types::FQName::parse(&file.name);

        for import in &file.imports {
            let (name, path) = import.path.split_last().unwrap();
            let exporting_module_name = types::FQName::from_parts(path.iter().map(String::as_str));
            let item_name = types::Identifier::parse(name);

            let exported_item_path = exporting_module_name.with_part(item_name);
            let imported_item_path = module_path.with_part(item_name);

            let Some(imported_item) = root_module_declaration.get_item(exported_item_path) else {
                return Err(
                    TypeCheckErrorDescription::ItemDoesNotExist(exported_item_path)
                        .at(ErrorLocation::Position(imported_item_path, import.position)),
                );
            };

            match &imported_item.kind {
                DeclaredItemKind::Function(DeclaredFunction { .. })
                | DeclaredItemKind::Struct { .. }
                | DeclaredItemKind::Predeclared(types::Item {
                    kind:
                        types::ItemKind::Function(types::Function { .. })
                        | types::ItemKind::Struct(types::Struct { .. }),
                    ..
                }) => {
                    if imported_item.visibility != types::Visibility::Export {
                        return Err(TypeCheckErrorDescription::ItemNotExported(
                            exporting_module_name,
                            item_name,
                        )
                        .at(ErrorLocation::Position(imported_item_path, import.position)));
                    }
                    let importing_module_path = types::FQName::parse(&file.name);
                    let imported_as = importing_module_path.with_part(
                        import
                            .alias
                            .as_ref()
                            .map_or_else(|| item_name, |x| types::Identifier::parse(x)),
                    );
                    root_module_declaration.declare(
                        imported_as,
                        DeclaredItem {
                            kind: DeclaredItemKind::Import(DeclaredImport {
                                position: import.position,
                                imported_item: exporting_module_name.with_part(item_name),
                            }),
                            visibility: imported_item.visibility,
                        },
                    );
                }
                DeclaredItemKind::Import(_)
                | DeclaredItemKind::Predeclared(types::Item {
                    kind: types::ItemKind::Import(_),
                    ..
                }) => {
                    return Err(
                        TypeCheckErrorDescription::ItemDoesNotExist(exported_item_path)
                            .at(ErrorLocation::Position(imported_item_path, import.position)),
                    );
                }
                DeclaredItemKind::Module(_)
                | DeclaredItemKind::Predeclared(types::Item {
                    kind: types::ItemKind::Module(_),
                    visibility: _,
                }) => todo!(),
            }
        }

        for item in &file.declarations {
            let position = item.position;

            match &item.kind {
                ast::DeclarationKind::Function(_) | ast::DeclarationKind::Struct(_) => {}
                ast::DeclarationKind::Impl(impl_declaration) => {
                    let struct_name = types::Identifier::parse(&impl_declaration.struct_name);
                    let struct_path = module_path.with_part(struct_name);

                    let declared_impl = declared_impls
                        .iter()
                        .filter(|x| x.0.without_last() == struct_path);

                    let functions = declared_impl
                        .map(|(_, f)| {
                            type_check_associated_function(
                                f,
                                &root_module_declaration,
                                ErrorLocation::Position(struct_path.with_part(f.name), position),
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = root_module_declaration.get_item_mut(struct_path) else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(struct_path)
                            .at(ErrorLocation::Position(struct_path, position)));
                    };

                    // TODO DeclaredItem should have a is_struct method for this!
                    let DeclaredItem {
                        kind: DeclaredItemKind::Struct(DeclaredStruct { fields, .. }),
                        ..
                    } = struct_
                    else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(struct_path)
                            .at(ErrorLocation::Position(struct_path, position)));
                    };

                    for function in &functions {
                        impls.insert(function.struct_.with_part(function.name), function.clone());

                        fields.insert(
                            function.name,
                            DeclaredStructField {
                                type_: function.type_(),
                                static_: true,
                            },
                        );
                    }
                }
            }
        }
    }

    type_check_definitions(
        &root_module_declaration,
        &root_module_declaration,
        &impls,
        None,
    )
}

// TODO split into smaller functions
#[allow(clippy::too_many_lines)]
fn type_check_definitions(
    root_module_declaration: &DeclaredModule,
    declaration_to_check: &DeclaredModule,
    impls: &HashMap<types::FQName, types::AssociatedFunction>,
    root_path: Option<types::FQName>,
) -> Result<types::Module, TypeCheckError> {
    let mut root_module: types::Module = types::Module::new();
    let root_path = root_path.unwrap_or_else(|| FQName::parse(""));

    for (item_path, declared_item) in declaration_to_check.items() {
        match &declared_item.kind {
            DeclaredItemKind::Function(declared_function) => {
                let body = type_check_function(
                    declared_function,
                    root_module_declaration,
                    ErrorLocation::Position(
                        declared_function.name,
                        declared_function.definition.position,
                    ),
                )?;

                root_module.declare_item(
                    *item_path,
                    types::Item {
                        kind: types::ItemKind::Function(body),
                        visibility: declared_item.visibility,
                    },
                );
            }
            DeclaredItemKind::Struct(DeclaredStruct {
                name: struct_name,
                fields,
            }) => {
                root_module.declare_item(
                    *item_path,
                    types::Item {
                        kind: types::ItemKind::Struct(types::Struct {
                            name: *struct_name,
                            fields: fields
                                .iter()
                                .map(|(field_name, declaration)| types::StructField {
                                    struct_name: *struct_name,
                                    name: *field_name,
                                    type_: declaration.type_.clone(),
                                    static_: declaration.static_,
                                })
                                .collect(),
                            impls: impls
                                .iter()
                                .filter(|f| &f.0.without_last() == struct_name)
                                .map(|(k, v)| (k.last(), v.clone()))
                                .collect(),
                        }),
                        visibility: declared_item.visibility,
                    },
                );
            }
            DeclaredItemKind::Import(DeclaredImport {
                imported_item,
                position,
            }) => {
                let imported_item = root_module_declaration.get_item(*imported_item).unwrap();
                match &imported_item.kind {
                    DeclaredItemKind::Struct(DeclaredStruct {
                        name: imported_item_name,
                        ..
                    })
                    | DeclaredItemKind::Function(DeclaredFunction {
                        name: imported_item_name,
                        ..
                    })
                    | DeclaredItemKind::Predeclared(types::Item {
                        kind:
                            types::ItemKind::Function(types::Function {
                                name: imported_item_name,
                                ..
                            })
                            | types::ItemKind::Struct(types::Struct {
                                name: imported_item_name,
                                ..
                            }),
                        ..
                    }) => root_module.declare_item(
                        *item_path,
                        types::Item {
                            kind: types::ItemKind::Import(types::Import {
                                imported_item: *imported_item_name,
                                position: *position,
                            }),
                            visibility: types::Visibility::Internal, // TODO can imports be reexported?
                        },
                    ),
                    DeclaredItemKind::Import(_) => todo!(),
                    DeclaredItemKind::Predeclared(types::Item {
                        kind: types::ItemKind::Import(_),
                        ..
                    }) => todo!(),
                    DeclaredItemKind::Module(_) => todo!(),
                    DeclaredItemKind::Predeclared(types::Item {
                        kind: types::ItemKind::Module(_),
                        ..
                    }) => todo!(),
                }
            }
            DeclaredItemKind::Predeclared(_) => {}
            DeclaredItemKind::Module(module_declaration) => root_module.declare_item(
                *item_path,
                // TODO ensure the visibility here is set correctly
                types::Item {
                    kind: types::ItemKind::Module(
                        type_check_definitions(
                            root_module_declaration,
                            module_declaration,
                            impls,
                            Some(root_path),
                        )
                        .unwrap(),
                    ),
                    visibility: types::Visibility::Export,
                },
            ),
        }
    }

    Ok(root_module)
}

const fn convert_visibility(visibility: ast::Visibility) -> types::Visibility {
    match visibility {
        ast::Visibility::Export => types::Visibility::Export,
        ast::Visibility::Internal => types::Visibility::Internal,
    }
}

// TODO make type checker an object, so that we don't have to pass stuff like available_types
// around
fn type_check_function(
    declared_function: &DeclaredFunction,
    available_types: &DeclaredModule,
    error_location: ErrorLocation,
) -> Result<types::Function, TypeCheckError> {
    let definition = type_check_function_definition(
        &declared_function.definition,
        available_types,
        declared_function.name.without_last(),
        error_location,
    )?;

    Ok(types::Function {
        name: declared_function.name,
        definition,
    })
}

fn type_check_associated_function(
    declared_function: &DeclaredAssociatedFunction,
    available_types: &DeclaredModule,
    error_location: ErrorLocation,
) -> Result<types::AssociatedFunction, TypeCheckError> {
    let definition = type_check_function_definition(
        &declared_function.definition,
        available_types,
        declared_function.struct_.without_last(),
        error_location,
    )?;

    Ok(types::AssociatedFunction {
        struct_: declared_function.struct_,
        name: declared_function.name,
        definition,
    })
}

fn type_check_function_definition(
    declared_function: &DeclaredFunctionDefinition,
    available_types: &DeclaredModule,
    module: types::FQName,
    error_location: ErrorLocation,
) -> Result<types::FunctionDefinition, TypeCheckError> {
    let mut locals: HashMap<types::Identifier, types::Type> = HashMap::new();

    if let Some(DeclaredItem {
        kind: DeclaredItemKind::Module(local_module),
        visibility: _,
    }) = available_types.get_item(module)
    {
        // TODO locals should be a struct that handles this
        for global in &local_module.items {
            match &global.1.kind {
                DeclaredItemKind::Function(_)
                | DeclaredItemKind::Struct(_)
                | DeclaredItemKind::Import(_)
                | DeclaredItemKind::Predeclared(_) => {
                    locals.insert(*global.0, global.1.type_(available_types));
                }
                DeclaredItemKind::Module(_) => {} // TODO we want to include modules here as well,
                                                  // once they exist as objects in the type system
            }
        }
    }

    for argument in &declared_function.arguments {
        locals.insert(argument.name, argument.type_.clone());
    }

    let body = match &declared_function.ast.body {
        ast::FunctionBody::Statements(body_statements, _) => {
            let mut checked_statements = vec![];

            for statement in body_statements {
                let checked_statement = match statement {
                    ast::Statement::Expression(expression, _) => {
                        types::Statement::Expression(type_check_expression(
                            expression,
                            &locals,
                            available_types,
                            error_location.clone(),
                        )?)
                    }

                    ast::Statement::Let(name, type_, expression) => {
                        let checked_expression = type_check_expression(
                            expression,
                            &locals,
                            available_types,
                            error_location.clone(),
                        )?;
                        // TODO we should resolve the FQDN by looking at locals, then imports, instead of
                        // convert_type just assuming current module!
                        let type_ = convert_type(module, type_);

                        if checked_expression.type_ != type_ {
                            return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                                target_variable: types::Identifier::parse(name),
                                variable_type: type_,
                                assigned_type: checked_expression.type_,
                            }
                            .at(error_location));
                        }

                        locals.insert(types::Identifier::parse(name), type_);

                        types::Statement::Let(types::LetStatement {
                            binding: types::Identifier::parse(name),
                            value: checked_expression,
                        })
                    }
                    ast::Statement::Return(expression, _) => {
                        // TODO verify that all paths return a value
                        let checked_expression = type_check_expression(
                            expression,
                            &locals,
                            available_types,
                            error_location.clone(),
                        )?;

                        if checked_expression.type_ != declared_function.return_type {
                            return Err(TypeCheckErrorDescription::MismatchedReturnType {
                                actual: checked_expression.type_,
                                expected: declared_function.return_type.clone(),
                            }
                            .at(error_location));
                        }

                        types::Statement::Return(checked_expression)
                    }
                };

                checked_statements.push(checked_statement);
            }

            // TODO validate that the return type actually exists!

            types::FunctionBody::Statements(checked_statements)
        }
        ast::FunctionBody::Extern(foreign_name, _) => {
            types::FunctionBody::Extern(types::Identifier::parse(foreign_name))
        }
    };

    Ok(types::FunctionDefinition {
        arguments: declared_function
            .arguments
            .iter()
            .map(|argument| types::Argument {
                name: argument.name,
                type_: argument.type_.clone(),
                position: argument.position,
            })
            .collect(),
        return_type: declared_function.return_type.clone(),
        body,
        position: declared_function.position,
    })
}

fn type_check_associated_function_declaration(
    function: &ast::Function,
    self_type: types::FQName,
    position: ast::SourceRange,
) -> Result<DeclaredAssociatedFunction, TypeCheckError> {
    let function_name = types::Identifier::parse(&function.name);
    let function_path = self_type.with_part(function_name);
    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(self_type.without_last(), &arg.type_);

        if type_ == types::Type::Unit {
            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                argument_name: types::Identifier::parse(&arg.name),
            }
            .at(ErrorLocation::Position(function_path, arg.position)));
        }

        arguments.push(DeclaredArgument {
            name: types::Identifier::parse(&arg.name),
            type_: convert_type(self_type.without_last(), &arg.type_),
            position: arg.position,
        });
    }

    Ok(DeclaredAssociatedFunction {
        struct_: function_path.without_last(),
        name: function_name,
        definition: DeclaredFunctionDefinition {
            arguments,
            return_type: convert_type(self_type.without_last(), &function.return_type),
            ast: function.clone(),
            position,
        },
    })
}

fn type_check_function_declaration(
    function: &ast::Function,
    module_path: types::FQName,
    position: ast::SourceRange,
) -> Result<DeclaredFunction, TypeCheckError> {
    let function_name = types::Identifier::parse(&function.name);
    let function_path = module_path.with_part(function_name);

    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(module_path, &arg.type_);

        if type_ == types::Type::Unit {
            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                argument_name: types::Identifier::parse(&arg.name),
            }
            .at(ErrorLocation::Position(function_path, arg.position)));
        }

        arguments.push(DeclaredArgument {
            name: types::Identifier::parse(&arg.name),
            type_: convert_type(module_path, &arg.type_),
            position: arg.position,
        });
    }

    Ok(DeclaredFunction {
        name: function_path,
        definition: DeclaredFunctionDefinition {
            arguments,
            return_type: convert_type(module_path, &function.return_type),
            ast: function.clone(),
            position,
        },
    })
}

// TODO split it into smaller functions
#[allow(clippy::too_many_lines)]
fn type_check_expression(
    expression: &ast::Expression,
    locals: &HashMap<types::Identifier, types::Type>,
    available_types: &DeclaredModule,
    error_location: ErrorLocation,
) -> Result<types::Expression, TypeCheckError> {
    let position = expression.position;

    match &expression.kind {
        ast::ExpressionKind::Call {
            target,
            arguments: passed_arguments,
        } => {
            let checked_target =
                type_check_expression(target, locals, available_types, error_location.clone())?;

            let types::Type::Callable {
                arguments: ref callable_arguments,
                ref return_type,
            } = checked_target.type_
            else {
                return Err(TypeCheckErrorDescription::CallingNotCallableItem(
                    checked_target.type_,
                )
                .at(error_location));
            };
            let mut callable_arguments = callable_arguments.clone();

            let self_argument = callable_arguments
                .first()
                .and_then(|a| {
                    if a.name == types::Identifier::parse("self") {
                        Some(a)
                    } else {
                        None
                    }
                })
                .cloned();

            if passed_arguments.len() + usize::from(self_argument.is_some())
                != callable_arguments.len()
            {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    checked_target.type_.clone(),
                )
                .at(error_location));
            }

            let mut checked_arguments = vec![];
            if let Some(self_argument) = self_argument {
                let mut callable_arguments_iter = callable_arguments.into_iter();

                callable_arguments_iter.next().unwrap();

                callable_arguments = callable_arguments_iter.collect();

                let types::Type::StructDescriptor(target_struct_type_descriptor) = available_types
                    .get_item(self_argument.type_.name())
                    .unwrap()
                    .type_(available_types)
                else {
                    todo!("{:?}", self_argument.type_);
                };

                let expected_type = target_struct_type_descriptor.object_type();

                if self_argument.type_ != expected_type {
                    return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                        target: *target.clone(),
                        argument_name: self_argument.name,
                        expected_type,
                        actual_type: self_argument.type_,
                    }
                    .at(error_location));
                }

                checked_arguments.push(types::Expression {
                    position: self_argument.position,
                    type_: self_argument.type_,
                    kind: types::ExpressionKind::SelfAccess,
                });
            }

            for (argument, called_function_argument) in
                passed_arguments.iter().zip(callable_arguments)
            {
                let checked_argument = type_check_expression(
                    argument,
                    locals,
                    available_types,
                    error_location.clone(),
                )?;
                let expected_type = &called_function_argument.type_;

                if &checked_argument.type_ != expected_type {
                    return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                        target: *target.clone(),
                        argument_name: called_function_argument.name,
                        expected_type: expected_type.clone(),
                        actual_type: checked_argument.type_,
                    }
                    .at(error_location));
                }

                checked_arguments.push(checked_argument);
            }

            Ok(types::Expression {
                position,
                type_: *return_type.clone(),
                kind: types::ExpressionKind::Call {
                    target: Box::new(checked_target),
                    arguments: checked_arguments,
                },
            })
        }
        ast::ExpressionKind::Literal(literal) => match literal {
            ast::Literal::String(value, _) => Ok(types::Expression {
                position,
                type_: types::Type::Object(*TYPE_NAME_STRING),
                kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
            }),
            ast::Literal::UnsignedInteger(value) => Ok(types::Expression {
                position,
                type_: types::Type::U64,
                kind: types::ExpressionKind::Literal(types::Literal::UnsignedInteger(*value)),
            }),
        },
        ast::ExpressionKind::VariableReference(name) => {
            let id = types::Identifier::parse(name);

            let value_type = locals
                .get(&id)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id).at(error_location.clone())
                })
                .cloned()?;

            Ok(types::Expression {
                position,
                type_: value_type,
                kind: types::ExpressionKind::VariableAccess(id),
            })
        }
        ast::ExpressionKind::StructConstructor(struct_name) => {
            let id = types::Identifier::parse(struct_name);

            let types::Type::StructDescriptor(types::StructDescriptorType { name, fields: _ }) =
                locals
                    .get(&id)
                    .ok_or_else(|| {
                        TypeCheckErrorDescription::UndeclaredVariable(id).at(error_location.clone())
                    })
                    .cloned()
                    .unwrap()
            else {
                todo!();
            };

            Ok(types::Expression {
                position,
                type_: types::Type::Object(name),
                kind: types::ExpressionKind::StructConstructor(id),
            })
        }
        ast::ExpressionKind::FieldAccess { target, field_name } => {
            let target = type_check_expression(target, locals, available_types, error_location)?;

            let type_name = match &target.type_ {
                types::Type::Unit => todo!(),
                types::Type::Object(identifier) => *identifier,
                types::Type::Array(_) => todo!(),
                types::Type::StructDescriptor(_) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => *TYPE_NAME_U64,
                types::Type::Pointer(_) => todo!(),
                types::Type::U8 => todo!(),
            };

            let target_type = available_types
                .get_item(type_name)
                .unwrap()
                .type_(available_types);
            let types::Type::StructDescriptor(types::StructDescriptorType { name: _, fields }) =
                target_type
            else {
                todo!("{target_type}");
            };

            let field_name = types::Identifier::parse(field_name);

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
