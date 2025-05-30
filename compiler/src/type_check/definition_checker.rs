use itertools::Itertools;

use crate::{type_check::declarations::DeclaredRootModule, types::store::TypeStore as _};
use std::{cell::RefCell, collections::HashMap};

use crate::{ast, types};

use super::{
    DeclaredFunction,
    declarations::resolve_type,
    errors::{TypeCheckError, TypeCheckErrorDescription},
    expression_checker::{ExpressionChecker, Locals},
};

pub(super) struct DefinitionChecker {
    root_module_declaration: DeclaredRootModule,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    functions: HashMap<types::functions::FunctionId, types::functions::Function>,
    main: Option<types::functions::FunctionId>,
    // TODO get rid of the RefCell, once we use TypeId everywhere it should be possible
    types: RefCell<types::store::MultiStore>,
}

impl DefinitionChecker {
    pub(super) fn check(mut self) -> Result<types::modules::RootModule, TypeCheckError> {
        self.type_check_definitions()?;

        let Self {
            root_module_declaration,
            declared_impls: _,
            functions: _,
            main,
            types,
        } = self;

        let DeclaredRootModule {
            structs,
            functions: _,
            predeclared_functions,
            modules,
            imports: _,
            interfaces: _,
        } = root_module_declaration;

        let mut functions = self.functions;

        for (name, function) in predeclared_functions {
            functions.insert(name, function);
        }

        if let Some(main) = main {
            return Ok(types::modules::RootModule::new_app(
                main,
                modules,
                structs,
                functions,
                types.into_inner(),
            ));
        }

        Ok(types::modules::RootModule::new_library(
            modules,
            structs,
            functions,
            types.into_inner(),
        ))
    }

    fn type_check_definitions(&mut self) -> Result<(), TypeCheckError> {
        let mut impls = self.type_check_associated_function_definitions()?;

        for (function_id, function) in &self.root_module_declaration.functions {
            let function = self.type_check_function(function)?;

            self.functions.insert(*function_id, function.clone());
        }

        let struct_ids = {
            let binding = &self.root_module_declaration.structs;

            binding.keys().copied().collect_vec()
        };

        for struct_id in struct_ids {
            self.type_check_struct(impls.remove(&struct_id).unwrap_or_default(), struct_id);
        }

        Ok(())
    }

    fn type_check_associated_function_definitions(
        &mut self,
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
                let functions = &self.root_module_declaration.functions;
                let function = functions.get(function_id).unwrap();
                let function = self.type_check_function(function)?;

                impls
                    .entry(*struct_id)
                    .or_default()
                    .insert(*function_id, function.clone());

                self.functions.insert(*function_id, function.clone());
            }
        }
        Ok(impls)
    }

    fn type_check_struct(
        &mut self,
        impls: HashMap<types::functions::FunctionId, types::functions::Function>,
        struct_id: types::structs::StructId,
    ) {
        let all_structs = &mut self.root_module_declaration.structs;
        let struct_ = all_structs.get_mut(&struct_id).unwrap();

        for (name, impl_) in impls {
            struct_.fields.push(types::structs::StructField {
                struct_id: struct_.id,
                name: name.local(),
                type_: impl_.type_id,
                static_: true,
            });
            struct_.impls.push(name);
        }
    }

    fn type_check_function(
        &self,
        declared_function: &DeclaredFunction,
    ) -> Result<types::functions::Function, TypeCheckError> {
        let mut locals = Locals::new();

        locals.push_arguments(&declared_function.arguments);

        let expression_checker = ExpressionChecker::new(&self.root_module_declaration, &self.types);

        let body = match &declared_function.ast.body {
            ast::FunctionBody::Statements(body_statements, _) => {
                let mut checked_statements = vec![];

                for statement in body_statements {
                    let type_check_statement = self.type_check_statement(
                        declared_function,
                        &mut locals,
                        statement,
                        &expression_checker,
                    )?;

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
            type_id: declared_function.type_id,
            module_name: declared_function.module_name,
            visibility: declared_function.visibility,
            arguments: declared_function
                .arguments
                .iter()
                .map(|argument| {
                    Ok(types::functions::Argument {
                        name: argument.name,
                        type_id: argument.type_id,
                        position: argument.position,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            return_type: declared_function.return_type,
            body,
            position: declared_function.position,
        })
    }

    fn type_check_statement(
        &self,
        declared_function: &DeclaredFunction,
        locals: &mut Locals,
        statement: &ast::Statement,
        expression_checker: &ExpressionChecker<'_>,
    ) -> Result<types::Statement, TypeCheckError> {
        Ok(match statement {
            ast::Statement::Expression(expression, _) => {
                types::Statement::Expression(expression_checker.type_check_expression(
                    expression,
                    &*locals,
                    declared_function.module_name,
                )?)
            }

            ast::Statement::Let(name, type_, expression) => {
                let checked_expression = expression_checker.type_check_expression(
                    expression,
                    &*locals,
                    declared_function.module_name,
                )?;

                let type_ = resolve_type(
                    &self.root_module_declaration,
                    declared_function.module_name,
                    type_,
                    &mut *self.types.borrow_mut(),
                )?;

                if !type_.can_assign_to(self.types.borrow().get(checked_expression.type_id), |id| {
                    self.root_module_declaration.structs.get(&id).cloned()
                }) {
                    return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                        target_variable: *name,
                        variable_type: type_,
                        assigned_type: checked_expression.type_id,
                    }
                    .at(expression.position));
                }

                locals.push_variable(*name, self.types.borrow_mut().add(type_));

                types::Statement::Let(types::LetStatement {
                    binding: *name,
                    value: checked_expression,
                })
            }
            ast::Statement::Return(expression, _) => {
                // TODO verify that all paths return a value
                let checked_expression = expression_checker.type_check_expression(
                    expression,
                    &*locals,
                    declared_function.module_name,
                )?;

                if checked_expression.type_id != declared_function.return_type {
                    return Err(TypeCheckErrorDescription::MismatchedReturnType {
                        actual: checked_expression.type_id,
                        expected: declared_function.return_type,
                    }
                    .at(expression.position));
                }

                types::Statement::Return(checked_expression)
            }
        })
    }

    pub(super) fn new(
        root_module_declaration: DeclaredRootModule,
        declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
        main: Option<types::functions::FunctionId>,
        types: types::store::MultiStore,
    ) -> Self {
        Self {
            root_module_declaration,
            declared_impls,
            main,
            functions: HashMap::new(),
            types: RefCell::new(types),
        }
    }
}
