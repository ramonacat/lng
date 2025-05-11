use itertools::Itertools;

use crate::{
    identifier::Identifier,
    type_check::declarations::DeclaredRootModule,
    types::{GenericType, TypeArguments},
};
use std::{cell::RefCell, collections::HashMap};

use crate::{ast, std::TYPE_NAME_STRING, types};

use super::{
    DeclaredFunction,
    declarations::resolve_type,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

struct Locals {
    values: HashMap<Identifier, types::GenericType>,
}

impl Locals {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn push_arguments(&mut self, arguments: &[types::functions::Argument<types::GenericType>]) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    fn push_variable(&mut self, name: Identifier, r#type: types::GenericType) {
        self.values.insert(name, r#type);
    }

    fn get(&self, id: Identifier) -> Option<types::GenericType> {
        self.values.get(&id).cloned()
    }
}

pub(super) struct DefinitionChecker {
    root_module_declaration: DeclaredRootModule,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    functions:
        RefCell<HashMap<types::functions::FunctionId, types::functions::Function<GenericType>>>,
    main: Option<types::functions::FunctionId>,
}

impl DefinitionChecker {
    pub(super) fn check(self) -> Result<types::modules::RootModule, TypeCheckError> {
        self.type_check_definitions()?;

        let Self {
            root_module_declaration,
            declared_impls: _,
            functions: _,
            main,
        } = self;

        let DeclaredRootModule {
            structs,
            functions: _,
            predeclared_functions,
            modules,
            imports: _,
        } = root_module_declaration;

        let mut functions = self.functions.take();

        for (name, function) in predeclared_functions.take() {
            functions.insert(name, function);
        }

        if let Some(main) = main {
            return Ok(types::modules::RootModule::new_app(
                main,
                modules,
                structs.into_inner(),
                functions,
            ));
        }

        Ok(types::modules::RootModule::new_library(
            modules,
            structs.into_inner(),
            functions,
        ))
    }

    fn type_check_definitions(&self) -> Result<(), TypeCheckError> {
        let mut impls = self.type_check_associated_function_definitions()?;

        for (function_id, function) in self.root_module_declaration.functions.borrow().iter() {
            let function = self.type_check_function(function)?;

            self.functions
                .borrow_mut()
                .insert(*function_id, function.clone());
        }

        let struct_ids = {
            let binding = self.root_module_declaration.structs.borrow();

            binding.keys().copied().collect_vec()
        };

        for struct_id in struct_ids {
            self.type_check_struct(impls.remove(&struct_id).unwrap_or_default(), struct_id);
        }

        Ok(())
    }

    fn type_check_associated_function_definitions(
        &self,
    ) -> Result<
        HashMap<
            types::structs::StructId,
            HashMap<types::functions::FunctionId, types::functions::Function<GenericType>>,
        >,
        TypeCheckError,
    > {
        let mut impls: HashMap<
            types::structs::StructId,
            HashMap<types::functions::FunctionId, types::functions::Function<GenericType>>,
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
        impls: HashMap<types::functions::FunctionId, types::functions::Function<GenericType>>,
        struct_id: types::structs::StructId,
    ) {
        let all_structs = &mut self.root_module_declaration.structs.borrow_mut();
        let struct_ = all_structs.get_mut(&struct_id).unwrap();

        for (name, impl_) in impls {
            struct_.fields.push(types::structs::StructField {
                struct_id: struct_.id,
                name: name.local(),
                type_: impl_.type_(),
                static_: true,
            });
            struct_.impls.push(name);
        }
    }

    fn type_check_function(
        &self,
        declared_function: &DeclaredFunction,
    ) -> Result<types::functions::Function<GenericType>, TypeCheckError> {
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
            type_: types::GenericType::new(
                types::GenericTypeKind::Function(declared_function.id),
                TypeArguments::new_empty(),
            ),
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
    ) -> Result<types::Statement<GenericType>, TypeCheckError> {
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
                    &self.root_module_declaration,
                    declared_function.module_name,
                    type_,
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
        module_path: types::modules::ModuleId,
    ) -> Result<types::Expression<GenericType>, TypeCheckError> {
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
                    type_: types::GenericType::new(
                        types::GenericTypeKind::Object {
                            type_name: *TYPE_NAME_STRING,
                        },
                        TypeArguments::new_empty(),
                    ),
                    kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
                }),
                ast::Literal::UnsignedInteger(value) => Ok(types::Expression {
                    position,
                    type_: types::GenericType::u64(),
                    kind: types::ExpressionKind::Literal(types::Literal::UnsignedInteger(*value)),
                }),
            },
            ast::ExpressionKind::VariableReference(name) => {
                self.type_check_variable_reference(expression, locals, module_path, *name)
            }
            // TODO instead of resolving the struct name here, we should make it take an expression
            // instead of an identifier
            ast::ExpressionKind::StructConstructor(target) => {
                let target = self.type_check_expression(target, locals, module_path)?;

                let (type_, kind) = {
                    let types::GenericTypeKind::Object {
                        type_name: instantiated_struct_id,
                    } = target.type_.kind()
                    else {
                        todo!();
                    };

                    (
                        types::GenericType::new(
                            types::GenericTypeKind::Object {
                                type_name: *instantiated_struct_id,
                            },
                            types::TypeArguments::new_empty(),
                        ),
                        types::ExpressionKind::StructConstructor(Box::new(target)),
                    )
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
                    .get(&struct_name)
                    .unwrap()
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
        module_path: types::modules::ModuleId,
        name: Identifier,
    ) -> Result<types::Expression<GenericType>, TypeCheckError> {
        let value_type = locals.get(name);

        let (kind, type_) = if let Some(value_type) = value_type {
            (types::ExpressionKind::LocalVariableAccess(name), value_type)
        } else {
            let (module_path, name) = self
                .root_module_declaration
                .resolve_import(module_path, name);
            if let Some(global) = &self.root_module_declaration.get_item(module_path, name) {
                (
                    types::ExpressionKind::GlobalVariableAccess(module_path, name),
                    global.type_(),
                )
            } else {
                return Err(
                    TypeCheckErrorDescription::UndeclaredVariable(name).at(expression.position)
                );
            }
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
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
    ) -> Result<types::Expression<GenericType>, TypeCheckError> {
        let checked_target = self.type_check_expression(target, locals, module_path)?;

        let types::GenericTypeKind::Callable(function_id) = checked_target.type_.kind() else {
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
                type_: _,
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
        root_module_declaration: DeclaredRootModule,
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
