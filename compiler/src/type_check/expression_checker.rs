use std::collections::HashMap;

use crate::identifier::Identifier;
use crate::std::TYPE_NAME_STRING;
use crate::types;
use crate::{ast, std::TYPE_NAME_U64};

use super::{
    DeclaredFunction, DeclaredRootModule,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct Locals {
    values: HashMap<Identifier, types::Type>,
}

impl Locals {
    pub(super) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(super) fn push_arguments(&mut self, arguments: &[types::functions::Argument]) {
        for argument in arguments {
            self.values.insert(argument.name, argument.type_.clone());
        }
    }

    pub(super) fn push_variable(&mut self, name: Identifier, r#type: types::Type) {
        self.values.insert(name, r#type);
    }

    pub(super) fn get(&self, id: Identifier) -> Option<types::Type> {
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
                    type_: types::Type::new(types::TypeKind::Object(
                        types::structs::InstantiatedStructId::new(
                            *TYPE_NAME_STRING,
                            types::generics::TypeArgumentValues::new_empty(),
                        ),
                    )),
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
            ast::ExpressionKind::StructConstructor(target, field_values) => {
                let target = self.type_check_expression(target, locals, module_path)?;

                let (type_, kind) = {
                    let types::TypeKind::Object(instantiated_struct_id) = target.type_.kind()
                    else {
                        todo!();
                    };

                    let field_values = field_values
                        .iter()
                        .map(|x| {
                            Ok(types::structs::FieldValue {
                                name: x.name,
                                value: self.type_check_expression(&x.value, locals, module_path)?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    (
                        types::Type::new(types::TypeKind::Object(instantiated_struct_id.clone())),
                        types::ExpressionKind::StructConstructor(Box::new(target), field_values),
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

                let field_type = self.field_type(&target.type_, *field_name);

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
    ) -> Result<types::Expression, TypeCheckError> {
        let checked_target = self.type_check_expression(target, locals, module_path)?;
        let target_type_kind = checked_target.type_.kind();

        let (mut callable_arguments, return_type) =
            self.retrieve_callable_info(&checked_target, target_type_kind);

        let self_argument = dbg!(&callable_arguments)
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
                checked_target.type_,
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

    fn retrieve_callable_info(
        &self,
        checked_target: &types::Expression,
        target_type_kind: &types::TypeKind,
    ) -> (Vec<types::functions::Argument>, types::Type) {
        let (callable_arguments, return_type) = {
            if let types::TypeKind::Callable(function_id) = target_type_kind {
                let root_module = &self.root_module_declaration.functions;
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
                    .get(function_id)
                {
                    (arguments.clone(), return_type.clone())
                } else {
                    panic!("calling unknown function: {function_id}");
                }
            } else if let types::TypeKind::IndirectCallable(interface_id, function_name) =
                checked_target.type_.kind()
            {
                let interfaces = &self.root_module_declaration.interfaces;

                let function = interfaces
                    .get(&interface_id.id())
                    .unwrap()
                    .functions
                    .get(function_name)
                    .unwrap();

                (function.arguments.clone(), function.return_type.clone())
            } else {
                todo!()
            }
        };
        (callable_arguments, return_type)
    }

    fn type_check_variable_reference(
        &self,
        expression: &ast::Expression,
        locals: &Locals,
        module_path: types::modules::ModuleId,
        name: Identifier,
    ) -> Result<types::Expression, TypeCheckError> {
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

    fn field_type(&self, r#type: &types::Type, field_name: Identifier) -> types::Type {
        match r#type.kind() {
            types::TypeKind::Generic(_) => todo!(),
            types::TypeKind::Unit => todo!(),
            types::TypeKind::Object(instantiated_struct_id) => self
                .root_module_declaration
                .structs
                .get(&instantiated_struct_id.id())
                .map(|x| x.field_type(field_name))
                .unwrap(),
            types::TypeKind::Array { .. } => todo!(),
            types::TypeKind::Callable(_) => todo!(),
            types::TypeKind::U64 => self
                .root_module_declaration
                .structs
                .get(&*TYPE_NAME_U64)
                .map(|x| x.field_type(field_name))
                .unwrap(),
            types::TypeKind::U8 => todo!(),
            types::TypeKind::Pointer(_) => todo!(),
            types::TypeKind::Struct(struct_id) => self
                .root_module_declaration
                .structs
                .get(&struct_id.id())
                .map(|x| x.field_type(field_name))
                .unwrap(),
            types::TypeKind::Function(_) => todo!(),
            types::TypeKind::Interface(_) => todo!(),
            types::TypeKind::InterfaceObject(instantiated_inteface_id) => self
                .root_module_declaration
                .interfaces
                .get(&instantiated_inteface_id.id())
                .map(|i| i.functions.get(&field_name).unwrap())
                .map(|_| {
                    types::Type::new_generic(
                        types::TypeKind::IndirectCallable(
                            instantiated_inteface_id.clone(),
                            field_name,
                        ),
                        r#type.arguments().clone(),
                        types::generics::TypeArgumentValues::new_empty(),
                    )
                })
                .unwrap(),
            types::TypeKind::IndirectCallable(_, _) => todo!(),
        }
    }
}
