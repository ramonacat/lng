// TODO add checking around the correctnes of visibility (i.e. that only visible items can be
// imported)
use std::collections::HashMap;

use crate::{ast, errors::ErrorLocation, std::TYPE_NAME_STRING, types};

use super::{
    DeclaredFunction, DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule,
    DeclaredStructField,
    declarations::{DeclaredRootModule, resolve_type},
    definition_checker::DefinitionChecker,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct DeclarationChecker<'pre> {
    root_module_declaration: DeclaredRootModule<'pre>,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    main: Option<types::functions::FunctionId>,
}

impl<'pre> DeclarationChecker<'pre> {
    fn type_check_module_declarations(&self, program: &[ast::SourceFile]) {
        // this is equivalent-ish to topo-sort, as fewer parts in the name means it is higher in the
        // hierarchy (i.e. main.test will definitely appear after main)
        let mut modules_to_declare = program
            .iter()
            .map(|x| types::FQName::parse(&x.name))
            .collect::<Vec<_>>();
        modules_to_declare.sort_by_key(|name| name.len());

        for module_path in modules_to_declare {
            self.root_module_declaration.module.borrow_mut().declare(
                module_path,
                DeclaredItem {
                    kind: DeclaredItemKind::Module(DeclaredModule::new()),
                    // TODO how do we determine the visibility for modules?
                    visibility: types::Visibility::Export,
                },
            );
        }
    }

    fn type_check_imports(&self, program: &[ast::SourceFile]) {
        for file in program {
            for import in &file.imports {
                let (name, path) = import.path.split_last().unwrap();
                let exporting_module_name =
                    types::FQName::from_parts(path.iter().map(String::as_str));
                let item_name = types::Identifier::parse(name);

                let importing_module_path = types::FQName::parse(&file.name);
                let imported_as = importing_module_path.with_part(
                    import
                        .alias
                        .as_ref()
                        .map_or_else(|| item_name, |x| types::Identifier::parse(x)),
                );
                self.root_module_declaration.module.borrow_mut().declare(
                    imported_as,
                    DeclaredItem {
                        kind: DeclaredItemKind::Import(DeclaredImport {
                            imported_item: exporting_module_name.with_part(item_name),
                        }),
                        visibility: types::Visibility::Internal,
                    },
                );
            }
        }
    }

    fn type_check_impl_declarations(
        &mut self,
        program: &[ast::SourceFile],
    ) -> Result<(), TypeCheckError> {
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
                                function.ast.name.clone(),
                                DeclaredStructField {
                                    // TODO we should here handle the case of the type actually
                                    // being generic
                                    type_: types::Type::new_not_generic(types::TypeKind::Callable(
                                        function.id,
                                    )),
                                    static_: true,
                                },
                            );

                            self.declared_impls
                                .entry(types::structs::StructId::FQName(struct_path))
                                .or_default()
                                .push(function.id);

                            self.root_module_declaration
                                .functions
                                .borrow_mut()
                                .insert(function.id, function.clone());
                        }

                        let mut structs = self.root_module_declaration.structs.borrow_mut();

                        let Some(struct_to_modify) =
                            structs.get_mut(&types::structs::StructId::FQName(struct_path))
                        else {
                            return Err(TypeCheckErrorDescription::ItemDoesNotExist(struct_path)
                                .at(error_location));
                        };

                        for (field_name, field) in fields_to_add {
                            struct_to_modify.fields.push(types::structs::StructField {
                                struct_id: types::structs::StructId::FQName(struct_path),
                                name: types::Identifier::parse(&field_name),
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
            let module_path = types::FQName::parse(&file.name);

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
            let module_path = types::FQName::parse(&file.name);

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

                        self.root_module_declaration.module.borrow_mut().declare(
                            module_path.with_part(types::Identifier::parse(&function.name)),
                            DeclaredItem {
                                visibility: Self::convert_visibility(function.visibility),
                                kind: DeclaredItemKind::Function(function_id),
                            },
                        );
                    }
                    ast::DeclarationKind::Struct(_) | ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        for file in program {
            let module_path = types::FQName::parse(&file.name);

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Struct(struct_) => {
                        for field in &struct_.fields {
                            // this is so that resolve_type returns an error in case the type is
                            // invalid
                            let _ = resolve_type(
                                &self.root_module_declaration.module.borrow(),
                                module_path,
                                &field.type_,
                                ErrorLocation::Position(
                                    module_path.with_part(types::Identifier::parse(&struct_.name)),
                                    field.position,
                                ),
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
        module_path: types::FQName,
        struct_: &ast::Struct,
    ) -> Result<(), TypeCheckError> {
        let mut fields = vec![];
        for field in &struct_.fields {
            fields.push(types::structs::StructField {
                type_: resolve_type(
                    &self.root_module_declaration.module.borrow(),
                    module_path,
                    &field.type_,
                    ErrorLocation::Position(
                        module_path.with_part(types::Identifier::parse(&struct_.name)),
                        field.position,
                    ),
                )?,

                static_: false,
                struct_id: types::structs::StructId::FQName(
                    module_path.with_part(types::Identifier::parse(&struct_.name)),
                ),
                name: types::Identifier::parse(&field.name),
            });
        }
        let name = module_path.with_part(types::Identifier::parse(&struct_.name));
        let id = types::structs::StructId::FQName(name);
        self.root_module_declaration.structs.borrow_mut().insert(
            id,
            types::structs::Struct {
                name,
                fields,
                impls: HashMap::new(),
                type_arguments: types::TypeArguments::new_empty(),
            },
        );
        self.root_module_declaration.module.borrow_mut().declare(
            name,
            DeclaredItem {
                kind: DeclaredItemKind::Struct(id),
                visibility: Self::convert_visibility(struct_.visibility),
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
        if function_declaration.ast.name == "main" && visibility == ast::Visibility::Export {
            if function_declaration.arguments.len() == 1 {
                if let Some(argument) = function_declaration.arguments.first() {
                    if let types::TypeKind::Array {
                        element_type: array_item_type,
                    } = argument.type_.kind()
                    {
                        if let types::TypeKind::Object { type_name: id } = array_item_type.kind() {
                            if id.0 == types::structs::StructId::FQName(*TYPE_NAME_STRING) {
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
        current_module: types::FQName,
        self_type: types::FQName,
        position: ast::SourceRange,
    ) -> Result<DeclaredFunction, TypeCheckError> {
        let function_name = types::Identifier::parse(&function.name);
        let function_path = self_type.with_part(function_name);
        let mut arguments = vec![];

        for arg in &function.arguments {
            arguments.push(types::functions::Argument {
                name: types::Identifier::parse(&arg.name),
                type_: resolve_type(
                    &self.root_module_declaration.module.borrow(),
                    current_module,
                    &arg.type_,
                    ErrorLocation::Position(function_path, arg.position),
                )?,
                position: arg.position,
            });
        }

        Ok(DeclaredFunction {
            id: types::functions::FunctionId::FQName(function_path),
            arguments,
            // TODO figure out the correct error location here
            return_type: resolve_type(
                &self.root_module_declaration.module.borrow(),
                current_module,
                &function.return_type,
                ErrorLocation::Indeterminate,
            )?,
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        })
    }

    fn type_check_function_declaration(
        &self,
        function: &ast::Function,
        module_path: types::FQName,
        position: ast::SourceRange,
    ) -> DeclaredFunction {
        let function_name = types::Identifier::parse(&function.name);
        let function_path = module_path.with_part(function_name);

        let mut arguments = vec![];

        for arg in &function.arguments {
            arguments.push(types::functions::Argument {
                name: types::Identifier::parse(&arg.name),
                type_: resolve_type(
                    &self.root_module_declaration.module.borrow(),
                    module_path,
                    &arg.type_,
                    ErrorLocation::Position(function_path, arg.position),
                )
                .unwrap(),
                position: arg.position,
            });
        }

        DeclaredFunction {
            id: match &function.body {
                ast::FunctionBody::Statements(_, _) => {
                    types::functions::FunctionId::FQName(function_path)
                }
                ast::FunctionBody::Extern(extern_, _) => {
                    types::functions::FunctionId::Extern(types::Identifier::parse(extern_))
                }
            },
            arguments,
            // TODO figure out the correct error location
            return_type: resolve_type(
                &self.root_module_declaration.module.borrow(),
                module_path,
                &function.return_type,
                ErrorLocation::Indeterminate,
            )
            .unwrap(),
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        }
    }

    pub(crate) fn new(root_module_declaration: DeclaredRootModule<'pre>) -> Self {
        Self {
            root_module_declaration,
            declared_impls: HashMap::new(),
            main: None,
        }
    }

    pub(crate) fn check(
        mut self,
        program: &'pre [ast::SourceFile],
    ) -> Result<DefinitionChecker<'pre>, TypeCheckError> {
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
