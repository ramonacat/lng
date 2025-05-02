use std::collections::HashMap;

use crate::{
    ast,
    errors::ErrorLocation,
    std::{TYPE_NAME_STRING, TYPE_NAME_U64},
    types::{self, AssociatedFunction, FQName, Identifier, RootModule},
};

use super::{
    DeclaredArgument, DeclaredAssociatedFunction, DeclaredFunction, DeclaredFunctionDefinition,
    DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule, DeclaredStruct,
    DeclaredStructField,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

struct Locals<'globals> {
    values: HashMap<types::Identifier, types::Type>,
    globals: &'globals DeclaredModule,
    scope_module_name: FQName,
}

impl<'globals> Locals<'globals> {
    fn from_globals(root_module: &'globals DeclaredModule, module_name: FQName) -> Self {
        Self {
            values: HashMap::new(),
            globals: root_module,
            scope_module_name: module_name,
        }
    }

    fn push_arguments(&mut self, arguments: &[DeclaredArgument]) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    fn resolve_type(&self, type_: &ast::TypeDescription) -> Option<types::Type> {
        match type_ {
            ast::TypeDescription::Array(type_description) => Some(types::Type::Array(Box::new(
                self.resolve_type(type_description)?,
            ))),
            ast::TypeDescription::Named(name) => {
                let id = Identifier::parse(name);

                self.values
                    .get(&id)
                    .cloned()
                    // TODO should this type_ method be integrated into this?
                    .or_else(|| {
                        self.globals
                            .get_item(self.scope_module_name.with_part(id))
                            .map(|x| x.type_(self.globals))
                    })
            }
        }
    }

    fn push_variable(&mut self, name: Identifier, r#type: types::Type) {
        self.values.insert(name, r#type);
    }

    fn get(&self, id: Identifier) -> Option<types::Type> {
        self.values.get(&id).cloned().or_else(|| {
            self.globals
                .get_item(self.scope_module_name.with_part(id))
                .map(|x| x.type_(self.globals))
        })
    }
}

pub(super) struct DefinitionChecker {
    root_module_declaration: DeclaredModule,
    declared_impls: HashMap<types::FQName, DeclaredAssociatedFunction>,
    main: Option<FQName>,
}

impl DefinitionChecker {
    pub(super) fn check(&self) -> Result<types::RootModule, TypeCheckError> {
        let root_module = self.type_check_definitions(&self.root_module_declaration, None)?;

        if let Some(main) = self.main {
            return Ok(RootModule::App {
                main,
                module: root_module,
            });
        }

        Ok(RootModule::Library {
            module: root_module,
        })
    }

    fn type_check_definitions(
        &self,
        declaration_to_check: &DeclaredModule,
        root_path: Option<types::FQName>,
    ) -> Result<types::Module, TypeCheckError> {
        let mut root_module: types::Module = types::Module::new();
        let root_path = root_path.unwrap_or_else(|| FQName::parse(""));
        let mut impls = self.type_check_associated_function_definitions()?;

        for (item_path, declared_item) in declaration_to_check.items() {
            match &declared_item.kind {
                DeclaredItemKind::Function(declared_function) => {
                    let body = self.type_check_function(
                        declared_function,
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
                    let struct_impls = impls.remove(struct_name).unwrap();
                    let item =
                        Self::type_check_struct(*struct_name, fields, struct_impls, declared_item);
                    root_module.declare_item(*item_path, item);
                }
                DeclaredItemKind::Import(DeclaredImport {
                    imported_item,
                    position,
                }) => {
                    self.type_check_import(&mut root_module, *item_path, *imported_item, position);
                }
                DeclaredItemKind::Predeclared(_) => {}
                DeclaredItemKind::Module(module_declaration) => {
                    let submodule_root = self
                        .type_check_definitions(module_declaration, Some(root_path))
                        .unwrap();
                    root_module.declare_item(
                        *item_path,
                        // TODO ensure the visibility here is set correctly
                        types::Item {
                            kind: types::ItemKind::Module(submodule_root),
                            visibility: types::Visibility::Export,
                        },
                    );
                }
            }
        }

        Ok(root_module)
    }

    fn type_check_associated_function_definitions(
        &self,
    ) -> Result<HashMap<FQName, HashMap<types::Identifier, AssociatedFunction>>, TypeCheckError>
    {
        let mut impls: HashMap<FQName, HashMap<types::Identifier, AssociatedFunction>> =
            HashMap::new();
        for (associated_function_name, declared_associated_function) in &self.declared_impls {
            let struct_name = associated_function_name.without_last();
            let struct_path = struct_name.without_last();

            let position = declared_associated_function.definition.position;

            let associated_function = self.type_check_associated_function(
                declared_associated_function,
                ErrorLocation::Position(
                    struct_path.with_part(associated_function_name.last()),
                    position,
                ),
            )?;

            impls
                .entry(struct_name)
                .or_default()
                .insert(associated_function.name, associated_function);
        }
        Ok(impls)
    }

    fn type_check_function(
        &self,
        declared_function: &DeclaredFunction,
        error_location: ErrorLocation,
    ) -> Result<types::Function, TypeCheckError> {
        let definition = self.type_check_function_definition(
            &declared_function.definition,
            declared_function.name.without_last(),
            error_location,
        )?;

        Ok(types::Function {
            name: declared_function.name,
            definition,
        })
    }

    fn type_check_struct(
        struct_name: FQName,
        fields: &HashMap<types::Identifier, DeclaredStructField>,
        impls: HashMap<types::Identifier, AssociatedFunction>,
        declared_item: &DeclaredItem,
    ) -> types::Item {
        let mut fields: Vec<_> = fields
            .iter()
            .map(|(field_name, declaration)| types::StructField {
                struct_name,
                name: *field_name,
                type_: declaration.type_.clone(),
                static_: declaration.static_,
            })
            .collect();

        for (name, impl_) in &impls {
            fields.push(types::StructField {
                struct_name,
                name: *name,
                type_: impl_.type_(),
                static_: true,
            });
        }

        types::Item {
            kind: types::ItemKind::Struct(types::Struct {
                name: struct_name,
                fields,
                impls,
            }),
            visibility: declared_item.visibility,
        }
    }

    fn type_check_import(
        &self,
        root_module: &mut types::Module,
        item_path: types::Identifier,
        imported_item: FQName,
        position: &ast::SourceRange,
    ) {
        let imported_item = self
            .root_module_declaration
            .get_item(imported_item)
            .unwrap();
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
                item_path,
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

    fn type_check_associated_function(
        &self,
        declared_function: &DeclaredAssociatedFunction,
        error_location: ErrorLocation,
    ) -> Result<types::AssociatedFunction, TypeCheckError> {
        let definition = self.type_check_function_definition(
            &declared_function.definition,
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
        &self,
        declared_function: &DeclaredFunctionDefinition,
        module: types::FQName,
        error_location: ErrorLocation,
    ) -> Result<types::FunctionDefinition, TypeCheckError> {
        let mut locals = Locals::from_globals(&self.root_module_declaration, module);

        locals.push_arguments(&declared_function.arguments);

        let body = match &declared_function.ast.body {
            ast::FunctionBody::Statements(body_statements, _) => {
                let mut checked_statements = vec![];

                for statement in body_statements {
                    let checked_statement = match statement {
                        ast::Statement::Expression(expression, _) => {
                            types::Statement::Expression(self.type_check_expression(
                                expression,
                                &locals,
                                error_location.clone(),
                            )?)
                        }

                        ast::Statement::Let(name, type_, expression) => {
                            let checked_expression = self.type_check_expression(
                                expression,
                                &locals,
                                error_location.clone(),
                            )?;

                            let type_ = locals.resolve_type(type_).unwrap().instance_type();

                            if checked_expression.type_ != type_ {
                                return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                                    target_variable: types::Identifier::parse(name),
                                    variable_type: type_,
                                    assigned_type: checked_expression.type_,
                                }
                                .at(error_location));
                            }

                            locals.push_variable(types::Identifier::parse(name), type_);

                            types::Statement::Let(types::LetStatement {
                                binding: types::Identifier::parse(name),
                                value: checked_expression,
                            })
                        }
                        ast::Statement::Return(expression, _) => {
                            // TODO verify that all paths return a value
                            let checked_expression = self.type_check_expression(
                                expression,
                                &locals,
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
    fn type_check_expression(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        error_location: ErrorLocation,
    ) -> Result<types::Expression, TypeCheckError> {
        let position = expression.position;

        match &expression.kind {
            ast::ExpressionKind::Call {
                target,
                arguments: passed_arguments,
            } => {
                let expression_type = self.type_check_call(
                    target,
                    passed_arguments,
                    locals,
                    error_location,
                    position,
                )?;
                Ok(expression_type)
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

                let value_type = locals.get(id).ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id).at(error_location.clone())
                })?;

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
                        .get(id)
                        .ok_or_else(|| {
                            TypeCheckErrorDescription::UndeclaredVariable(id)
                                .at(error_location.clone())
                        })
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
                let target = self.type_check_expression(target, locals, error_location)?;

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

                let target_type = self
                    .root_module_declaration
                    .get_item(type_name)
                    .unwrap()
                    .type_(&self.root_module_declaration);
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
    fn type_check_call(
        &self,
        target: &ast::Expression,
        passed_arguments: &[ast::Expression],
        locals: &Locals,

        error_location: ErrorLocation,
        position: ast::SourceRange,
    ) -> Result<types::Expression, TypeCheckError> {
        let checked_target = self.type_check_expression(target, locals, error_location.clone())?;
        let types::Type::Callable {
            arguments: ref callable_arguments,
            ref return_type,
        } = checked_target.type_
        else {
            return Err(
                TypeCheckErrorDescription::CallingNotCallableItem(checked_target.type_)
                    .at(error_location),
            );
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
        if passed_arguments.len() + usize::from(self_argument.is_some()) != callable_arguments.len()
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

            let types::Type::StructDescriptor(target_struct_type_descriptor) = self
                .root_module_declaration
                .get_item(self_argument.type_.name())
                .unwrap()
                .type_(&self.root_module_declaration)
            else {
                todo!("{:?}", self_argument.type_);
            };

            let expected_type = target_struct_type_descriptor.object_type();

            if self_argument.type_ != expected_type {
                return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                    target: target.clone(),
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
        for (argument, called_function_argument) in passed_arguments.iter().zip(callable_arguments)
        {
            let checked_argument =
                self.type_check_expression(argument, locals, error_location.clone())?;
            let expected_type = &called_function_argument.type_;

            if &checked_argument.type_ != expected_type {
                return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                    target: target.clone(),
                    argument_name: called_function_argument.name,
                    expected_type: expected_type.clone(),
                    actual_type: checked_argument.type_,
                }
                .at(error_location));
            }

            checked_arguments.push(checked_argument);
        }
        let expression_type = types::Expression {
            position,
            type_: *return_type.clone(),
            kind: types::ExpressionKind::Call {
                target: Box::new(checked_target),
                arguments: checked_arguments,
            },
        };
        Ok(expression_type)
    }

    pub(super) const fn new(
        root_module_declaration: DeclaredModule,
        declared_impls: HashMap<FQName, DeclaredAssociatedFunction>,
        main: Option<FQName>,
    ) -> Self {
        Self {
            root_module_declaration,
            declared_impls,
            main,
        }
    }
}
