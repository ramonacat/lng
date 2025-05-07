use crate::type_check::declarations::DeclaredRootModule;
use std::{cell::RefCell, collections::HashMap};

use crate::{ast, errors::ErrorLocation, std::TYPE_NAME_STRING, types};

use super::{
    DeclaredFunction, DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

struct Locals<'globals, 'pre> {
    values: HashMap<types::Identifier, types::Type>,
    globals: &'globals DeclaredModule<'pre>,
    scope_module_name: types::FQName,
}

impl<'globals, 'pre> Locals<'globals, 'pre> {
    fn from_globals(
        root_module: &'globals DeclaredModule<'pre>,
        module_name: types::FQName,
    ) -> Self {
        Self {
            values: HashMap::new(),
            globals: root_module,
            scope_module_name: module_name,
        }
    }

    fn push_arguments(&mut self, arguments: &[types::functions::Argument]) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    fn resolve_type(
        &self,
        type_: &ast::TypeDescription,
        current_module: types::FQName,
        error_location: ErrorLocation,
    ) -> Result<types::Type, TypeCheckError> {
        match type_ {
            ast::TypeDescription::Array(type_description) => {
                Ok(types::Type::new_not_generic(types::TypeKind::Array {
                    element_type: Box::new(self.resolve_type(
                        type_description,
                        current_module,
                        error_location,
                    )?),
                }))
            }
            ast::TypeDescription::Named(name) => {
                let id = types::Identifier::parse(name);

                self.values
                    .get(&id)
                    .cloned()
                    .map(Ok)
                    .or_else(|| {
                        self.globals
                            .get_item(self.scope_module_name.with_part(id))
                            .map(|x| x.type_(self.globals, current_module, error_location))
                    })
                    .unwrap_or_else(|| {
                        Err(
                            TypeCheckErrorDescription::ItemDoesNotExist(types::FQName::parse(name))
                                .at(error_location),
                        )
                    })
            }
        }
    }

    fn push_variable(&mut self, name: types::Identifier, r#type: types::Type) {
        self.values.insert(name, r#type);
    }

    fn get(
        &self,
        id: types::Identifier,
        current_module: types::FQName,
        error_location: ErrorLocation,
    ) -> Result<Option<types::Type>, TypeCheckError> {
        self.values
            .get(&id)
            .cloned()
            .map(Ok)
            .or_else(|| {
                self.globals
                    .get_item(self.scope_module_name.with_part(id))
                    .map(|x| x.type_(self.globals, current_module, error_location))
            })
            .transpose()
    }
}

pub(super) struct DefinitionChecker<'pre> {
    root_module_declaration: DeclaredRootModule<'pre>,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    functions: RefCell<HashMap<types::functions::FunctionId, types::functions::Function>>,
    main: Option<types::functions::FunctionId>,
}

impl<'pre> DefinitionChecker<'pre> {
    pub(super) fn check(self) -> Result<types::RootModule, TypeCheckError> {
        let root_module =
            self.type_check_definitions(&self.root_module_declaration.module.borrow(), None)?;

        let Self {
            root_module_declaration,
            declared_impls: _,
            functions: _,
            main,
        } = self;

        let DeclaredRootModule {
            structs,
            functions: _,
            module: _,
            predeclared_functions: _,
        } = root_module_declaration;

        if let Some(main) = main {
            return Ok(types::RootModule::new_app(
                main,
                root_module,
                structs.into_inner(),
                self.functions.take(),
            ));
        }

        Ok(types::RootModule::new_library(
            root_module,
            structs.into_inner(),
            self.functions.take(),
        ))
    }

    fn type_check_definitions(
        &self,
        declaration_to_check: &DeclaredModule,
        root_path: Option<types::FQName>,
    ) -> Result<types::Module, TypeCheckError> {
        let mut root_module: types::Module = types::Module::new();
        let root_path = root_path.unwrap_or_else(|| types::FQName::parse(""));
        let mut impls = self.type_check_associated_function_definitions()?;

        for (item_path, declared_item) in declaration_to_check.items() {
            match &declared_item.kind {
                DeclaredItemKind::Function(function_id) => {
                    let body = self.type_check_function(
                        self.root_module_declaration
                            .functions
                            .borrow()
                            .get(function_id)
                            .unwrap(),
                        root_path,
                        // TODO figure out the real location here
                        ErrorLocation::Indeterminate,
                    )?;

                    self.functions
                        .borrow_mut()
                        .insert(*function_id, body.clone());

                    root_module.declare_item(
                        *item_path,
                        types::Item {
                            kind: types::ItemKind::Function(*function_id),
                            visibility: declared_item.visibility,
                        },
                    );
                }
                DeclaredItemKind::Struct(struct_id) => {
                    let item = self.type_check_struct(
                        *struct_id,
                        impls.remove(struct_id).unwrap_or_default(),
                        declared_item,
                    );

                    root_module.declare_item(*item_path, item);
                }
                DeclaredItemKind::Import(DeclaredImport { imported_item }) => {
                    self.type_check_import(&mut root_module, *item_path, *imported_item);
                }
                DeclaredItemKind::Predeclared(_) => {}
                DeclaredItemKind::Module(module_declaration) => {
                    let submodule_root = self.type_check_definitions(
                        module_declaration,
                        Some(root_path.with_part(*item_path)),
                    )?;
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
    ) -> Result<
        HashMap<
            types::structs::StructId,
            HashMap<types::functions::FunctionId, types::functions::Function>,
        >,
        TypeCheckError,
    > {
        let mut impls: HashMap<
            types::structs::StructId,
            HashMap<types::functions::FunctionId, types::functions::Function>,
        > = HashMap::new();
        for (struct_id, declared_impls) in &self.declared_impls {
            for function_id in declared_impls {
                let functions = self.root_module_declaration.functions.borrow();
                let function = functions.get(function_id).unwrap();
                let function = self.type_check_function(
                    function,
                    // TODO we should not be matching on the struct_id here, but instead have access to
                    // the module name through other means!
                    match function_id {
                        types::functions::FunctionId::FQName(fqname) => {
                            fqname.without_last().without_last()
                        }
                        types::functions::FunctionId::Extern(_) => todo!(),
                    },
                    // TODO figure out the correct location to pass for errors
                    ErrorLocation::Indeterminate,
                )?;

                impls
                    .entry(*struct_id)
                    .or_default()
                    .insert(*function_id, function.clone());

                self.functions
                    .borrow_mut()
                    .insert(*function_id, function.clone());
            }
        }
        Ok(impls)
    }

    fn type_check_struct(
        &self,
        struct_id: types::structs::StructId,
        impls: HashMap<types::functions::FunctionId, types::functions::Function>,
        declared_item: &DeclaredItem,
    ) -> types::Item {
        let all_structs = &mut self.root_module_declaration.structs.borrow_mut();
        let struct_ = all_structs.get_mut(&struct_id).unwrap();

        for (name, impl_) in impls {
            struct_.fields.push(types::structs::StructField {
                struct_id,
                // TODO don't match on the FunctionId here, instead get the name thorugh other
                // means
                name: match name {
                    types::functions::FunctionId::FQName(fqname) => fqname.last(),
                    types::functions::FunctionId::Extern(_) => todo!(),
                },
                type_: impl_.type_(),
                static_: true,
            });
            struct_.impls.insert(name, impl_);
        }

        types::Item {
            kind: types::ItemKind::Struct(struct_id),
            visibility: declared_item.visibility,
        }
    }

    fn type_check_import(
        &self,
        root_module: &mut types::Module,
        item_path: types::Identifier,
        imported_item: types::FQName,
    ) {
        let root_module_declaration = self.root_module_declaration.module.borrow();
        let imported_item = root_module_declaration.get_item(imported_item).unwrap();
        match &imported_item.kind {
            DeclaredItemKind::Function(imported_item_id)
            | DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Function(imported_item_id),
                ..
            }) => {
                // TODO this match should be removed and we should pass the ID directly to the
                // import
                let imported_item_id = match imported_item_id {
                    types::functions::FunctionId::FQName(fqname) => fqname,
                    types::functions::FunctionId::Extern(_) => todo!(),
                };
                root_module.declare_item(
                    item_path,
                    types::Item {
                        kind: types::ItemKind::Import(types::Import {
                            imported_item: *imported_item_id,
                        }),
                        visibility: types::Visibility::Internal, // TODO can imports be reexported?
                    },
                );
            }
            DeclaredItemKind::Import(_) => todo!(),
            DeclaredItemKind::Struct(struct_id)
            | DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Struct(struct_id),
                ..
            }) => {
                root_module.declare_item(
                    item_path,
                    types::Item {
                        kind: types::ItemKind::StructImport(*struct_id),
                        visibility: types::Visibility::Internal, // TODO can imports be reexported?
                    },
                );
            }
            DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Import(_) | types::ItemKind::StructImport(_),
                ..
            }) => todo!(),
            DeclaredItemKind::Module(_) => todo!(),
            DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Module(_),
                ..
            }) => todo!(),
        }
    }

    fn type_check_function(
        &self,
        declared_function: &DeclaredFunction,
        module: types::FQName,
        error_location: ErrorLocation,
    ) -> Result<types::functions::Function, TypeCheckError> {
        let root_module = self.root_module_declaration.module.borrow();
        let mut locals = Locals::from_globals(&root_module, module);
        let function_name = module.with_part(types::Identifier::parse(&declared_function.ast.name));

        locals.push_arguments(&declared_function.arguments);

        let body = match &declared_function.ast.body {
            ast::FunctionBody::Statements(body_statements, _) => {
                let mut checked_statements = vec![];

                for statement in body_statements {
                    let type_check_statement = self.type_check_statement(
                        declared_function,
                        module,
                        error_location,
                        &mut locals,
                        function_name,
                        statement,
                    )?;

                    checked_statements.push(type_check_statement);
                }

                types::functions::FunctionBody::Statements(checked_statements)
            }
            ast::FunctionBody::Extern(foreign_name, _) => {
                types::functions::FunctionBody::Extern(types::Identifier::parse(foreign_name))
            }
        };

        Ok(types::functions::Function {
            id: declared_function.id,
            visibility: declared_function.visibility,
            arguments: declared_function
                .arguments
                .iter()
                .map(|argument| {
                    Ok(types::functions::Argument {
                        name: argument.name,
                        type_: argument.type_.clone(),
                        position: argument.position,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            return_type: declared_function.return_type.clone(),
            body,
            position: declared_function.position,
        })
    }

    fn type_check_statement(
        &self,
        declared_function: &DeclaredFunction,
        module: types::FQName,
        error_location: ErrorLocation,
        locals: &mut Locals<'_, 'pre>,
        function_name: types::FQName,
        statement: &ast::Statement,
    ) -> Result<types::Statement, TypeCheckError> {
        Ok(match statement {
            ast::Statement::Expression(expression, _) => types::Statement::Expression(
                self.type_check_expression(expression, &*locals, module, error_location)?,
            ),

            ast::Statement::Let(name, type_, expression) => {
                let checked_expression =
                    self.type_check_expression(expression, &*locals, module, error_location)?;

                let type_ = locals
                    .resolve_type(
                        type_,
                        module,
                        ErrorLocation::Position(function_name, expression.position),
                    )
                    .unwrap();

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
                let checked_expression =
                    self.type_check_expression(expression, &*locals, module, error_location)?;

                if checked_expression.type_ != declared_function.return_type {
                    return Err(TypeCheckErrorDescription::MismatchedReturnType {
                        actual: checked_expression.type_,
                        expected: declared_function.return_type.clone(),
                    }
                    .at(error_location));
                }

                types::Statement::Return(checked_expression)
            }
        })
    }

    fn type_check_expression(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: types::FQName,
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
                    module_path,
                    error_location,
                    position,
                )?;

                Ok(expression_type)
            }
            ast::ExpressionKind::Literal(literal) => match literal {
                ast::Literal::String(value, _) => Ok(types::Expression {
                    position,
                    type_: types::Type::new_not_generic(types::TypeKind::Object {
                        type_name: types::structs::InstantiatedStructId(
                            types::structs::StructId::FQName(*TYPE_NAME_STRING),
                            types::TypeArgumentValues::new_empty(),
                        ),
                    }),
                    kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
                }),
                ast::Literal::UnsignedInteger(value) => Ok(types::Expression {
                    position,
                    type_: types::Type::u64(),
                    kind: types::ExpressionKind::Literal(types::Literal::UnsignedInteger(*value)),
                }),
            },
            ast::ExpressionKind::VariableReference(name) => {
                let id = types::Identifier::parse(name);

                let value_type = locals
                    .get(id, module_path, error_location)?
                    .ok_or_else(|| {
                        TypeCheckErrorDescription::UndeclaredVariable(id).at(error_location)
                    })?;

                Ok(types::Expression {
                    position,
                    type_: value_type,
                    kind: types::ExpressionKind::VariableAccess(id),
                })
            }
            ast::ExpressionKind::StructConstructor(struct_name) => {
                let id = types::Identifier::parse(struct_name);

                let struct_type =
                    locals
                        .get(id, module_path, error_location)?
                        .ok_or_else(|| {
                            TypeCheckErrorDescription::UndeclaredVariable(id).at(error_location)
                        })?;

                let types::TypeKind::Object {
                    type_name: instantiated_struct_id,
                } = struct_type.kind()
                else {
                    todo!();
                };

                Ok(types::Expression {
                    position,
                    type_: types::Type::new_not_generic(types::TypeKind::Object {
                        type_name: instantiated_struct_id.clone(),
                    }),
                    kind: types::ExpressionKind::StructConstructor(instantiated_struct_id.clone()),
                })
            }
            ast::ExpressionKind::FieldAccess { target, field_name } => {
                let target =
                    self.type_check_expression(target, locals, module_path, error_location)?;

                let field_name = types::Identifier::parse(field_name);
                let struct_name = target.type_.struct_name();

                let field_type = self
                    .root_module_declaration
                    .structs
                    .borrow()
                    .get(&struct_name.0)
                    .unwrap()
                    .instantiate(&struct_name.1)
                    .field_type(field_name);

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
        module_path: types::FQName,
        error_location: ErrorLocation,
        position: ast::SourceRange,
    ) -> Result<types::Expression, TypeCheckError> {
        let checked_target =
            self.type_check_expression(target, locals, module_path, error_location)?;
        let types::TypeKind::Callable(function_id) = checked_target.type_.kind() else {
            return Err(
                TypeCheckErrorDescription::CallingNotCallableItem(checked_target.type_)
                    .at(error_location),
            );
        };
        let (mut callable_arguments, return_type) = {
            let root_module = self.root_module_declaration.functions.borrow();
            if let Some(DeclaredFunction {
                arguments: callable_arguments,
                return_type,
                ..
            }) = root_module.get(function_id)
            {
                (callable_arguments.clone(), return_type.clone())
            } else if let Some(types::functions::Function {
                id: _,
                arguments,
                return_type,
                body: _,
                position: _,
                visibility: _,
            }) = self
                .root_module_declaration
                .predeclared_functions
                .borrow()
                .get(function_id)
            {
                (arguments.clone(), return_type.clone())
            } else {
                todo!();
            }
        };
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

            let first_argument = callable_arguments_iter.next().unwrap();

            callable_arguments = callable_arguments_iter.collect();

            let expected_type = first_argument.type_;

            if self_argument.type_ != expected_type {
                return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                    target: checked_target.type_,
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
                self.type_check_expression(argument, locals, module_path, error_location)?;

            let expected_type = &called_function_argument.type_;

            if &checked_argument.type_ != expected_type {
                return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                    target: checked_target.type_,
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
            type_: return_type,
            kind: types::ExpressionKind::Call {
                target: Box::new(checked_target),
                arguments: checked_arguments,
            },
        };
        Ok(expression_type)
    }

    pub(super) fn new(
        root_module_declaration: DeclaredRootModule<'pre>,
        declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
        main: Option<types::functions::FunctionId>,
    ) -> Self {
        Self {
            root_module_declaration,
            declared_impls,
            main,
            functions: RefCell::new(HashMap::new()),
        }
    }
}
