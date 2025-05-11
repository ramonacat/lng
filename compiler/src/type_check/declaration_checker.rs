// TODO add checking around the correctnes of visibility (i.e. that only visible items can be
// imported)
use std::collections::HashMap;

use crate::{
    ast,
    identifier::FQName,
    std::TYPE_NAME_STRING,
    types::{self, TypeArguments, modules::ModuleId},
};

use super::{
    DeclaredFunction, DeclaredStructField,
    declarations::{DeclaredRootModule, resolve_type},
    definition_checker::DefinitionChecker,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct DeclarationChecker {
    root_module_declaration: DeclaredRootModule,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    main: Option<types::functions::FunctionId>,
}

impl DeclarationChecker {
    fn type_check_module_declarations(&mut self, program: &[ast::SourceFile]) {
        // this is equivalent-ish to topo-sort, as fewer parts in the name means it is higher in the
        // hierarchy (i.e. main.test will definitely appear after main)
        let mut modules_to_declare = program
            .iter()
            .map(|x| types::modules::ModuleId::parse(&x.name.to_string()))
            .collect::<Vec<_>>();
        modules_to_declare.sort_by_key(|x| types::modules::ModuleId::len(*x));

        for module_path in modules_to_declare {
            self.root_module_declaration
                .declare_module(module_path, types::modules::Module::new(module_path));
        }
    }

    fn type_check_imports(&mut self, program: &[ast::SourceFile]) {
        for file in program {
            for import in &file.imports {
                let exporting_module_name = import.path;
                let importing_module_path = FQName::parse(&file.name.to_string());

                for item in &import.items {
                    self.root_module_declaration.import(
                        (
                            types::modules::ModuleId::FQName(importing_module_path),
                            item.1,
                        ),
                        (
                            types::modules::ModuleId::FQName(exporting_module_name),
                            item.0,
                        ),
                    );
                }
            }
        }
    }

    fn type_check_impl_declarations(
        &mut self,
        program: &[ast::SourceFile],
    ) -> Result<(), TypeCheckError> {
        for file in program {
            let module_path = types::modules::ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                let position = declaration.position;

                match &declaration.kind {
                    ast::DeclarationKind::Function(_) | ast::DeclarationKind::Struct(_) => {}
                    ast::DeclarationKind::Impl(impl_declaration) => {
                        let struct_name = impl_declaration.struct_name;
                        let struct_path =
                            types::structs::StructId::InModule(module_path, struct_name);

                        let functions = impl_declaration
                            .functions
                            .iter()
                            .map(|f| {
                                self.type_check_associated_function_declaration(
                                    f,
                                    module_path,
                                    struct_path,
                                    position,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut fields_to_add = HashMap::new();
                        for function in &functions {
                            function
                                .arguments
                                .iter()
                                .map(|x| {
                                    Ok(types::functions::Argument {
                                        name: x.name,
                                        type_: x.type_.clone(),
                                        position,
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            fields_to_add.insert(
                                function.ast.name,
                                DeclaredStructField {
                                    // TODO we should here handle the case of the type actually
                                    // being generic
                                    type_: types::GenericType::new(
                                        types::GenericTypeKind::Callable(function.id),
                                        TypeArguments::new_empty(),
                                    ),
                                    static_: true,
                                },
                            );

                            self.declared_impls
                                .entry(struct_path)
                                .or_default()
                                .push(function.id);

                            self.root_module_declaration
                                .functions
                                .borrow_mut()
                                .insert(function.id, function.clone());
                        }

                        let mut structs = self.root_module_declaration.structs.borrow_mut();

                        let Some(struct_to_modify) = structs.get_mut(&struct_path) else {
                            return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                                types::ItemId::Struct(struct_path),
                            )
                            .at(position));
                        };

                        for (field_name, field) in fields_to_add {
                            struct_to_modify.fields.push(types::structs::StructField {
                                struct_id: struct_path,
                                name: field_name,
                                type_: field.type_,
                                static_: field.static_,
                            });
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn type_check_declarations(
        &mut self,
        program: &[ast::SourceFile],
    ) -> Result<(), TypeCheckError> {
        for file in program {
            let module_path = ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Struct(struct_) => {
                        self.type_check_struct(module_path, struct_)?;
                    }
                    ast::DeclarationKind::Impl(_) | ast::DeclarationKind::Function(_) => {}
                }
            }
        }

        for file in program {
            let module_path = ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Function(function) => {
                        let function_declaration = self.type_check_function_declaration(
                            function,
                            module_path,
                            declaration.position,
                        );

                        self.detect_potential_entrypoint(
                            function.visibility,
                            &function_declaration,
                        );

                        let function_id = function_declaration.id;

                        self.root_module_declaration
                            .functions
                            .borrow_mut()
                            .insert(function_id, function_declaration.clone());
                    }
                    ast::DeclarationKind::Struct(_) | ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        for file in program {
            let module_path = ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Struct(struct_) => {
                        for field in &struct_.fields {
                            // this is so that resolve_type returns an error in case the type is
                            // invalid
                            let _ = resolve_type(
                                &self.root_module_declaration,
                                module_path,
                                &field.type_,
                            )?;
                        }
                    }
                    ast::DeclarationKind::Function(_) | ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        Ok(())
    }

    fn type_check_struct(
        &self,
        module_path: types::modules::ModuleId,
        struct_: &ast::Struct,
    ) -> Result<(), TypeCheckError> {
        let mut fields = vec![];
        let struct_id = types::structs::StructId::InModule(module_path, struct_.name);
        let struct_type = types::GenericType::new(
            types::GenericTypeKind::Struct(struct_id),
            types::TypeArguments::new_empty(),
        );

        for field in &struct_.fields {
            fields.push(types::structs::StructField {
                type_: resolve_type(&self.root_module_declaration, module_path, &field.type_)?,

                static_: false,
                struct_id,
                name: field.name,
            });
        }
        self.root_module_declaration.structs.borrow_mut().insert(
            struct_id,
            types::structs::Struct {
                id: struct_id,
                fields,
                impls: vec![],
                type_: struct_type,
            },
        );
        Ok(())
    }

    fn detect_potential_entrypoint(
        &mut self,
        visibility: ast::Visibility,
        function_declaration: &DeclaredFunction,
    ) {
        // TODO give it an attributte or something to denote as main?
        if function_declaration.ast.name == *"main" && visibility == ast::Visibility::Export {
            if function_declaration.arguments.len() == 1 {
                if let Some(argument) = function_declaration.arguments.first() {
                    if let types::GenericTypeKind::Array {
                        element_type: array_item_type,
                    } = argument.type_.kind()
                    {
                        if let types::GenericTypeKind::Object { type_name: id } =
                            array_item_type.kind()
                        {
                            if *id == *TYPE_NAME_STRING {
                                if self.main.is_some() {
                                    todo!("show a nice error here, main is already defined");
                                }

                                self.main = Some(function_declaration.id);
                            }
                        }
                    }
                }
            } else if function_declaration.arguments.is_empty() {
                self.main = Some(function_declaration.id);
            }
        }
    }

    const fn convert_visibility(visibility: ast::Visibility) -> types::Visibility {
        match visibility {
            ast::Visibility::Export => types::Visibility::Export,
            ast::Visibility::Internal => types::Visibility::Internal,
        }
    }

    fn type_check_associated_function_declaration(
        &self,
        function: &ast::Function,
        current_module: types::modules::ModuleId,
        self_type: types::structs::StructId,
        position: ast::SourceSpan,
    ) -> Result<DeclaredFunction, TypeCheckError> {
        let mut arguments = vec![];

        for arg in &function.arguments {
            arguments.push(types::functions::Argument {
                name: arg.name,
                type_: resolve_type(&self.root_module_declaration, current_module, &arg.type_)?,
                position: arg.position,
            });
        }

        Ok(DeclaredFunction {
            id: types::functions::FunctionId::InStruct(self_type, function.name),
            module_name: current_module,
            arguments,
            // TODO figure out the correct error location here
            return_type: resolve_type(
                &self.root_module_declaration,
                current_module,
                &function.return_type,
            )?,
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        })
    }

    fn type_check_function_declaration(
        &self,
        function: &ast::Function,
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
    ) -> DeclaredFunction {
        let mut arguments = vec![];

        for arg in &function.arguments {
            arguments.push(types::functions::Argument {
                name: arg.name,
                type_: resolve_type(&self.root_module_declaration, module_path, &arg.type_)
                    .unwrap(),
                position: arg.position,
            });
        }

        DeclaredFunction {
            id: types::functions::FunctionId::InModule(module_path, function.name),
            module_name: module_path,
            arguments,
            return_type: resolve_type(
                &self.root_module_declaration,
                module_path,
                &function.return_type,
            )
            .unwrap(),
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        }
    }

    pub(crate) fn new(root_module_declaration: DeclaredRootModule) -> Self {
        Self {
            root_module_declaration,
            declared_impls: HashMap::new(),
            main: None,
        }
    }

    pub(crate) fn check(
        mut self,
        program: &[ast::SourceFile],
    ) -> Result<DefinitionChecker, TypeCheckError> {
        self.type_check_module_declarations(program);
        self.type_check_imports(program);
        self.type_check_declarations(program)?;
        self.type_check_impl_declarations(program)?;

        Ok(DefinitionChecker::new(
            self.root_module_declaration,
            self.declared_impls,
            self.main,
        ))
    }
}
