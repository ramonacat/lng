use std::collections::HashMap;

use crate::{
    ast,
    errors::ErrorLocation,
    std::TYPE_NAME_STRING,
    types::{self, FQName},
};

use super::{
    DeclaredArgument, DeclaredAssociatedFunction, DeclaredFunction, DeclaredFunctionDefinition,
    DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule, DeclaredStruct,
    DeclaredStructField, convert_type,
    definition_checker::DefinitionChecker,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct DeclarationChecker {
    root_module_declaration: DeclaredModule,
    declared_impls: HashMap<types::FQName, DeclaredAssociatedFunction>,
    main: Option<FQName>,
}

impl DeclarationChecker {
    fn type_check_module_declarations(&mut self, program: &[ast::SourceFile]) {
        // this is equivalent-ish to topo-sort, as fewer parts in the name means it is higher in the
        // hierarchy (i.e. main.test will definitely appear after main)
        let mut modules_to_declare = program
            .iter()
            .map(|x| types::FQName::parse(&x.name))
            .collect::<Vec<_>>();
        modules_to_declare.sort_by_key(|name| name.len());

        for module_path in modules_to_declare {
            self.root_module_declaration.declare(
                module_path,
                DeclaredItem {
                    kind: DeclaredItemKind::Module(DeclaredModule::new()),
                    // TODO how do we determine the visibility for modules?
                    visibility: types::Visibility::Export,
                },
            );
        }
    }

    fn type_check_imports(&mut self, program: &[ast::SourceFile]) -> Result<(), TypeCheckError> {
        for file in program {
            let module_path = types::FQName::parse(&file.name);

            for import in &file.imports {
                let (name, path) = import.path.split_last().unwrap();
                let exporting_module_name =
                    types::FQName::from_parts(path.iter().map(String::as_str));
                let item_name = types::Identifier::parse(name);

                let exported_item_path = exporting_module_name.with_part(item_name);
                let imported_item_path = module_path.with_part(item_name);

                let Some(imported_item) = self.root_module_declaration.get_item(exported_item_path)
                else {
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
                        self.root_module_declaration.declare(
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
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            exported_item_path,
                        )
                        .at(ErrorLocation::Position(imported_item_path, import.position)));
                    }
                    DeclaredItemKind::Module(_)
                    | DeclaredItemKind::Predeclared(types::Item {
                        kind: types::ItemKind::Module(_),
                        visibility: _,
                    }) => todo!(),
                }
            }
        }

        Ok(())
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
                                Self::type_check_associated_function_declaration(
                                    f,
                                    struct_path,
                                    position,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let Some(struct_) = self.root_module_declaration.get_item_mut(struct_path)
                        else {
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
                                        return_type: Box::new(
                                            function.definition.return_type.clone(),
                                        ),
                                    },
                                    static_: true,
                                },
                            );

                            self.declared_impls
                                .insert(struct_path.with_part(function.name), function.clone());
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
                    ast::DeclarationKind::Function(function) => {
                        let function_declaration = Self::type_check_function_declaration(
                            function,
                            module_path,
                            declaration.position,
                        )?;

                        if function_declaration.name.last() == types::Identifier::parse("main")
                            && declaration.visibility == ast::Visibility::Export
                        {
                            if function_declaration.definition.arguments.len() == 1 {
                                if let Some(argument) =
                                    function_declaration.definition.arguments.first()
                                {
                                    if let types::Type::Array(ref array_item_type) = argument.type_
                                    {
                                        if let types::Type::Object(obj_type) = **array_item_type {
                                            if obj_type == *TYPE_NAME_STRING {
                                                if self.main.is_some() {
                                                    todo!(
                                                        "show a nice error here, main is already defined"
                                                    );
                                                }

                                                self.main = Some(function_declaration.name);
                                            }
                                        }
                                    }
                                }
                            } else if function_declaration.definition.arguments.is_empty() {
                                self.main = Some(function_declaration.name);
                            }
                        }

                        self.root_module_declaration.declare(
                            module_path.with_part(types::Identifier::parse(&function.name)),
                            DeclaredItem {
                                kind: DeclaredItemKind::Function(function_declaration),
                                visibility: Self::convert_visibility(declaration.visibility),
                            },
                        );
                    }
                    ast::DeclarationKind::Struct(struct_) => {
                        let mut fields = HashMap::new();
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
                        self.root_module_declaration.declare(
                            name,
                            DeclaredItem {
                                kind: DeclaredItemKind::Struct(DeclaredStruct { name, fields }),
                                visibility: Self::convert_visibility(declaration.visibility),
                            },
                        );
                    }
                    ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        for file in program {
            let module_path = types::FQName::parse(&file.name);

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Struct(struct_) => {
                        for field in &struct_.fields {
                            let type_name = convert_type(module_path, &field.type_).name();
                            if self.root_module_declaration.get_item(type_name).is_none() {
                                // TODO there should be a function that can canonicalize this name!
                                return Err(TypeCheckErrorDescription::ItemDoesNotExist(type_name)
                                    .at(ErrorLocation::Position(
                                        module_path
                                            .with_part(types::Identifier::parse(&struct_.name)),
                                        declaration.position,
                                    )));
                            }
                        }
                    }
                    ast::DeclarationKind::Function(_) | ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        Ok(())
    }

    const fn convert_visibility(visibility: ast::Visibility) -> types::Visibility {
        match visibility {
            ast::Visibility::Export => types::Visibility::Export,
            ast::Visibility::Internal => types::Visibility::Internal,
        }
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

    pub(crate) fn new(root_module_declaration: DeclaredModule) -> Self {
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
        self.type_check_declarations(program)?;
        self.type_check_impl_declarations(program)?;
        self.type_check_imports(program)?;

        Ok(DefinitionChecker::new(
            self.root_module_declaration,
            self.declared_impls,
            self.main,
        ))
    }
}
