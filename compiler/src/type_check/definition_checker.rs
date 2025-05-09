use crate::{
    identifier::{FQName, Identifier},
    type_check::declarations::DeclaredRootModule,
    types::TypeArgumentValues,
};
use std::{cell::RefCell, collections::HashMap};

use crate::{ast, std::TYPE_NAME_STRING, types};

use super::{
    DeclaredFunction, DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule,
    declarations::resolve_type,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

struct Locals {
    values: HashMap<Identifier, types::Type>,
}

impl Locals {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn push_arguments(&mut self, arguments: &[types::functions::Argument]) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    fn push_variable(&mut self, name: Identifier, r#type: types::Type) {
        self.values.insert(name, r#type);
    }

    fn get(&self, id: Identifier) -> Option<types::Type> {
        self.values.get(&id).cloned()
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
            self.type_check_definitions(&self.root_module_declaration.module, None)?;

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
            predeclared_functions,
        } = root_module_declaration;

        let mut functions = self.functions.take();

        for (name, function) in predeclared_functions.take() {
            functions.insert(name, function);
        }

        if let Some(main) = main {
            return Ok(types::RootModule::new_app(
                main,
                root_module,
                structs.into_inner(),
                functions,
            ));
        }

        Ok(types::RootModule::new_library(
            root_module,
            structs.into_inner(),
            functions,
        ))
    }

    fn type_check_definitions(
        &self,
        declaration_to_check: &DeclaredModule,
        root_path: Option<FQName>,
    ) -> Result<types::Module, TypeCheckError> {
        let mut root_module: types::Module = types::Module::new();
        let root_path = root_path.unwrap_or_else(|| FQName::parse(""));
        let mut impls = self.type_check_associated_function_definitions()?;

        for (item_name, declared_item) in declaration_to_check.items() {
            match &declared_item.kind {
                DeclaredItemKind::Function(function_id) => {
                    let body = self.type_check_function(
                        self.root_module_declaration
                            .functions
                            .borrow()
                            .get(function_id)
                            .unwrap(),
                    )?;

                    self.functions
                        .borrow_mut()
                        .insert(*function_id, body.clone());

                    root_module.declare_item(
                        *item_name,
                        types::Item {
                            kind: types::ItemKind::Function(*function_id),
                            visibility: declared_item.visibility,
                            position: declared_item.position,
                        },
                    );
                }
                DeclaredItemKind::Struct(struct_id) => {
                    let item = self.type_check_struct(
                        *struct_id,
                        impls.remove(struct_id).unwrap_or_default(),
                        declared_item,
                        *item_name,
                    );

                    root_module.declare_item(*item_name, item);
                }
                DeclaredItemKind::Import(DeclaredImport { imported_item }) => {
                    self.type_check_import(&mut root_module, *item_name, *imported_item);
                }
                DeclaredItemKind::Predeclared(_) => {}
                DeclaredItemKind::Module(module_declaration) => {
                    let submodule_root = self.type_check_definitions(
                        module_declaration,
                        Some(dbg!(root_path.with_part(*item_name))),
                    )?;
                    root_module.declare_item(
                        *item_name,
                        // TODO ensure the visibility here is set correctly
                        types::Item {
                            kind: types::ItemKind::Module(submodule_root),
                            visibility: types::Visibility::Export,
                            position: declared_item.position,
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
                let function = self.type_check_function(function)?;

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
        declared_name: Identifier,
    ) -> types::Item {
        let all_structs = &mut self.root_module_declaration.structs.borrow_mut();
        let struct_ = all_structs.get_mut(&struct_id).unwrap();

        for (name, impl_) in impls {
            struct_.fields.push(types::structs::StructField {
                struct_id,
                name: declared_name,
                type_: impl_.type_(),
                static_: true,
            });
            struct_.impls.insert(name, impl_);
        }

        types::Item {
            kind: types::ItemKind::Struct(struct_id),
            visibility: declared_item.visibility,
            position: declared_item.position,
        }
    }

    fn type_check_import(
        &self,
        root_module: &mut types::Module,
        item_path: Identifier,
        imported_item: FQName,
    ) {
        let root_module_declaration = &self.root_module_declaration.module;
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
                            position: imported_item.position,
                        }),
                        visibility: types::Visibility::Internal, // TODO can imports be reexported?
                        position: imported_item.position,
                    },
                );
            }
            DeclaredItemKind::Import(_) => todo!(),
            DeclaredItemKind::Struct(_)
            | DeclaredItemKind::Predeclared(types::Item {
                kind: types::ItemKind::Struct(_),
                ..
            }) => {}
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

    fn type_check_function(
        &self,
        declared_function: &DeclaredFunction,
    ) -> Result<types::functions::Function, TypeCheckError> {
        let mut locals = Locals::new();

        locals.push_arguments(&declared_function.arguments);

        let body = match &declared_function.ast.body {
            ast::FunctionBody::Statements(body_statements, _) => {
                let mut checked_statements = vec![];

                for statement in body_statements {
                    let type_check_statement =
                        self.type_check_statement(declared_function, &mut locals, statement)?;

                    checked_statements.push(type_check_statement);
                }

                types::functions::FunctionBody::Statements(checked_statements)
            }
            ast::FunctionBody::Extern(foreign_name, _) => {
                types::functions::FunctionBody::Extern(*foreign_name)
            }
        };

        Ok(types::functions::Function {
            id: declared_function.id,
            module_name: declared_function.module_name,
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
        locals: &mut Locals,
        statement: &ast::Statement,
    ) -> Result<types::Statement, TypeCheckError> {
        Ok(match statement {
            ast::Statement::Expression(expression, _) => types::Statement::Expression(
                self.type_check_expression(expression, &*locals, declared_function.module_name)?,
            ),

            ast::Statement::Let(name, type_, expression) => {
                let checked_expression = self.type_check_expression(
                    expression,
                    &*locals,
                    declared_function.module_name,
                )?;

                let type_ = resolve_type(
                    &self.root_module_declaration.module,
                    declared_function.module_name,
                    type_,
                    expression.position,
                )?;

                if checked_expression.type_ != type_ {
                    return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                        target_variable: *name,
                        variable_type: type_,
                        assigned_type: checked_expression.type_,
                    }
                    .at(expression.position));
                }

                locals.push_variable(*name, type_);

                types::Statement::Let(types::LetStatement {
                    binding: *name,
                    value: checked_expression,
                })
            }
            ast::Statement::Return(expression, _) => {
                // TODO verify that all paths return a value
                let checked_expression = self.type_check_expression(
                    expression,
                    &*locals,
                    declared_function.module_name,
                )?;

                if checked_expression.type_ != declared_function.return_type {
                    return Err(TypeCheckErrorDescription::MismatchedReturnType {
                        actual: checked_expression.type_,
                        expected: declared_function.return_type.clone(),
                    }
                    .at(expression.position));
                }

                types::Statement::Return(checked_expression)
            }
        })
    }

    fn type_check_expression(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: FQName,
    ) -> Result<types::Expression, TypeCheckError> {
        let position = expression.position;

        match &expression.kind {
            ast::ExpressionKind::Call {
                target,
                arguments: passed_arguments,
            } => {
                let expression_type =
                    self.type_check_call(target, passed_arguments, locals, module_path, position)?;

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
                self.type_check_variable_reference(expression, locals, module_path, *name)
            }
            // TODO instead of resolving the struct name here, we should make it take an expression
            // instead of an identifier
            ast::ExpressionKind::StructConstructor(struct_name) => {
                let struct_type = locals.get(*struct_name);

                let (type_, kind) = if let Some(struct_type) = struct_type {
                    let types::TypeKind::Object {
                        type_name: instantiated_struct_id,
                    } = struct_type.kind()
                    else {
                        todo!();
                    };

                    (
                        types::Type::new_not_generic(types::TypeKind::Object {
                            type_name: instantiated_struct_id.clone(),
                        }),
                        types::ExpressionKind::StructConstructor(instantiated_struct_id.clone()),
                    )
                } else if let Some(global) = self
                    .root_module_declaration
                    .module
                    .get_item(module_path.with_part(*struct_name))
                {
                    let DeclaredItemKind::Struct(struct_id) = global.kind else {
                        todo!();
                    };
                    (
                        global.type_(
                            &self.root_module_declaration.module,
                            module_path,
                            expression.position,
                        )?,
                        types::ExpressionKind::StructConstructor(
                            types::structs::InstantiatedStructId(
                                struct_id,
                                TypeArgumentValues::new_empty(),
                            ),
                        ),
                    )
                } else {
                    return Err(TypeCheckErrorDescription::UndeclaredVariable(*struct_name)
                        .at(expression.position));
                };

                Ok(types::Expression {
                    position,
                    type_,
                    kind,
                })
            }
            ast::ExpressionKind::FieldAccess { target, field_name } => {
                let target = self.type_check_expression(target, locals, module_path)?;

                let struct_name = target.type_.struct_name();

                let field_type = self
                    .root_module_declaration
                    .structs
                    .borrow()
                    .get(&struct_name.0)
                    .unwrap()
                    .instantiate(&struct_name.1)
                    .field_type(*field_name);

                Ok(types::Expression {
                    position,
                    type_: field_type,
                    kind: types::ExpressionKind::FieldAccess {
                        target: Box::new(target),
                        field: *field_name,
                    },
                })
            }
        }
    }

    fn type_check_variable_reference(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: FQName,
        name: Identifier,
    ) -> Result<types::Expression, TypeCheckError> {
        let value_type = locals.get(name);

        let (kind, type_) = if let Some(value_type) = value_type {
            (types::ExpressionKind::LocalVariableAccess(name), value_type)
        } else if let Some(global) = &self
            .root_module_declaration
            .module
            .get_item(module_path.with_part(name))
        {
            (
                types::ExpressionKind::GlobalVariableAccess(module_path.with_part(name)),
                global.type_(
                    &self.root_module_declaration.module,
                    module_path,
                    expression.position,
                )?,
            )
        } else {
            return Err(TypeCheckErrorDescription::UndeclaredVariable(name).at(expression.position));
        };

        Ok(types::Expression {
            position: expression.position,
            type_,
            kind,
        })
    }

    fn type_check_call(
        &self,
        target: &ast::Expression,
        passed_arguments: &[ast::Expression],
        locals: &Locals,
        module_path: FQName,
        position: ast::SourceSpan,
    ) -> Result<types::Expression, TypeCheckError> {
        let checked_target = self.type_check_expression(target, locals, module_path)?;

        let types::TypeKind::Callable(function_id) = checked_target.type_.kind() else {
            return Err(
                TypeCheckErrorDescription::CallingNotCallableItem(checked_target.type_)
                    .at(position),
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
                module_name: _,
            }) = self
                .root_module_declaration
                .predeclared_functions
                .borrow()
                .get(function_id)
            {
                (arguments.clone(), return_type.clone())
            } else {
                panic!("calling unknown function: {function_id}");
            }
        };

        let self_argument = callable_arguments
            .first()
            .and_then(|a| {
                if a.name == Identifier::parse("self") {
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
            .at(position));
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
                .at(position));
            }

            checked_arguments.push(types::Expression {
                position: self_argument.position,
                type_: self_argument.type_,
                kind: types::ExpressionKind::SelfAccess,
            });
        }

        for (argument, called_function_argument) in passed_arguments.iter().zip(callable_arguments)
        {
            let checked_argument = self.type_check_expression(argument, locals, module_path)?;

            let expected_type = &called_function_argument.type_;

            if &checked_argument.type_ != expected_type {
                return Err(TypeCheckErrorDescription::UnexpectedArgumentTypeInCall {
                    target: checked_target.type_,
                    argument_name: called_function_argument.name,
                    expected_type: expected_type.clone(),
                    actual_type: checked_argument.type_,
                }
                .at(position));
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
