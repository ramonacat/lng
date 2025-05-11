use std::collections::HashMap;

use crate::ast;
use crate::identifier::Identifier;
use crate::std::TYPE_NAME_STRING;
use crate::types;

use super::{
    DeclaredFunction, DeclaredRootModule,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct Locals {
    values: HashMap<Identifier, types::GenericType>,
}

impl Locals {
    pub(super) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(super) fn push_arguments(
        &mut self,
        arguments: &[types::functions::Argument<types::GenericType>],
    ) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    pub(super) fn push_variable(&mut self, name: Identifier, r#type: types::GenericType) {
        self.values.insert(name, r#type);
    }

    pub(super) fn get(&self, id: Identifier) -> Option<types::GenericType> {
        self.values.get(&id).cloned()
    }
}

pub(super) struct ExpressionChecker<'root> {
    root_module_declaration: &'root DeclaredRootModule,
}

impl<'root> ExpressionChecker<'root> {
    pub(super) fn type_check_expression(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: types::modules::ModuleId,
    ) -> Result<types::Expression<types::GenericType>, TypeCheckError> {
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
                        types::TypeArguments::new_empty(),
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

    fn type_check_call(
        &self,
        target: &ast::Expression,
        passed_arguments: &[ast::Expression],
        locals: &Locals,
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
    ) -> Result<types::Expression<types::GenericType>, TypeCheckError> {
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

    fn type_check_variable_reference(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: types::modules::ModuleId,
        name: Identifier,
    ) -> Result<types::Expression<types::GenericType>, TypeCheckError> {
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

    pub(crate) const fn new(root_module_declaration: &'root DeclaredRootModule) -> Self {
        Self {
            root_module_declaration,
        }
    }
}
