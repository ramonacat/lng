// TODO add checking around the correctnes of visibility (i.e. that only visible items can be
// imported)
use std::collections::HashMap;

use crate::{
    ast,
    errors::ErrorLocation,
    std::TYPE_NAME_STRING,
    types::{self, Identifier},
};

use super::{
    DeclaredArgument, DeclaredAssociatedFunction, DeclaredFunction, DeclaredFunctionDefinition,
    DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule, DeclaredStructField,
    declarations::resolve_type,
    definition_checker::DefinitionChecker,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct DeclarationChecker<'pre> {
    root_module_declaration: DeclaredModule<'pre>,
    declared_impls: HashMap<(types::StructId, types::Identifier), DeclaredAssociatedFunction>,
    main: Option<types::FQName>,
    structs: HashMap<types::StructId, types::Struct>,
}

impl<'pre> DeclarationChecker<'pre> {
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

    fn type_check_imports(&mut self, program: &[ast::SourceFile]) {
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
                self.root_module_declaration.declare(
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
                                    struct_path,
                                    position,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut fields_to_add = HashMap::new();
                        for function in &functions {
                            fields_to_add.insert(
                                function.name,
                                DeclaredStructField {
                                    // TODO we should here handle the case of the type actually
                                    // being generic
                                    type_: types::Type::new_not_generic(
                                        types::TypeKind::Callable {
                                            arguments: function
                                                .definition
                                                .arguments
                                                .iter()
                                                .map(|x| {
                                                    Ok(types::Argument {
                                                        name: x.name,
                                                        type_: resolve_type(
                                                            &self.root_module_declaration,
                                                            module_path,
                                                            &x.type_,
                                                            ErrorLocation::Position(
                                                                module_path
                                                                    .with_part(function.name),
                                                                x.position,
                                                            ),
                                                        )?
                                                        .instance_type(),
                                                        position,
                                                    })
                                                })
                                                .collect::<Result<Vec<_>, _>>()?,
                                            return_type: Box::new(
                                                resolve_type(
                                                    &self.root_module_declaration,
                                                    module_path,
                                                    &function.definition.return_type,
                                                    // TODO this position should point specifically at
                                                    // the return type, not the whole function
                                                    ErrorLocation::Position(
                                                        module_path.with_part(function.name),
                                                        function.definition.position,
                                                    ),
                                                )?
                                                .instance_type(),
                                            ),
                                        },
                                    ),
                                    static_: true,
                                },
                            );

                            self.declared_impls.insert(
                                (types::StructId::FQName(struct_path), function.name),
                                function.clone(),
                            );
                        }

                        let Some(struct_) =
                            self.structs.get_mut(&types::StructId::FQName(struct_path))
                        else {
                            return Err(TypeCheckErrorDescription::ItemDoesNotExist(struct_path)
                                .at(error_location));
                        };

                        for (field_name, field) in fields_to_add {
                            struct_.fields.push(types::StructField {
                                struct_id: types::StructId::FQName(struct_path),
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
            let module_path = types::FQName::parse(&file.name);

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Function(function) => {
                        let function_declaration = Self::type_check_function_declaration(
                            function,
                            module_path,
                            declaration.position,
                        );

                        self.detect_potential_entrypoint(
                            module_path,
                            function.visibility,
                            &function_declaration,
                        )?;

                        self.root_module_declaration.declare(
                            module_path.with_part(types::Identifier::parse(&function.name)),
                            DeclaredItem {
                                visibility: Self::convert_visibility(function.visibility),
                                kind: DeclaredItemKind::Function(function_declaration),
                            },
                        );
                    }
                    ast::DeclarationKind::Struct(struct_) => {
                        let mut fields = vec![];
                        for field in &struct_.fields {
                            fields.push(types::StructField {
                                type_: resolve_type(
                                    &self.root_module_declaration,
                                    module_path,
                                    &field.type_,
                                    ErrorLocation::Position(
                                        module_path
                                            .with_part(types::Identifier::parse(&struct_.name)),
                                        field.position,
                                    ),
                                )?,

                                static_: false,
                                struct_id: types::StructId::FQName(
                                    module_path.with_part(Identifier::parse(&struct_.name)),
                                ),
                                name: Identifier::parse(&field.name),
                            });
                        }
                        let name = module_path.with_part(types::Identifier::parse(&struct_.name));
                        let id = types::StructId::FQName(name);

                        // TODO get the actual type arguments from the declaration
                        self.structs.insert(
                            id,
                            types::Struct {
                                name,
                                fields,
                                impls: HashMap::new(),
                                type_arguments: types::TypeArguments::new_empty(),
                            },
                        );

                        self.root_module_declaration.declare(
                            name,
                            DeclaredItem {
                                kind: DeclaredItemKind::Struct(id),
                                visibility: Self::convert_visibility(struct_.visibility),
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
                            // this is so that resolve_type returns an error in case the type is
                            // invalid
                            let _ = resolve_type(
                                &self.root_module_declaration,
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

    fn detect_potential_entrypoint(
        &mut self,
        module_path: types::FQName,
        visibility: ast::Visibility,
        function_declaration: &DeclaredFunction,
    ) -> Result<(), TypeCheckError> {
        if function_declaration.name.last() == types::Identifier::parse("main")
            && visibility == ast::Visibility::Export
        {
            if function_declaration.definition.arguments.len() == 1 {
                if let Some(argument) = function_declaration.definition.arguments.first() {
                    if let types::TypeKind::Array {
                        element_type: array_item_type,
                    } = resolve_type(
                        &self.root_module_declaration,
                        module_path,
                        &argument.type_,
                        ErrorLocation::Position(function_declaration.name, argument.position),
                    )?
                    .kind()
                    {
                        if let types::TypeKind::StructDescriptor(id) = array_item_type.kind() {
                            if self.structs.get(id).unwrap().name == *TYPE_NAME_STRING {
                                if self.main.is_some() {
                                    todo!("show a nice error here, main is already defined");
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

        Ok(())
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
        self_type: types::FQName,
        position: ast::SourceRange,
    ) -> Result<DeclaredAssociatedFunction, TypeCheckError> {
        let function_name = types::Identifier::parse(&function.name);
        let function_path = self_type.with_part(function_name);
        let mut arguments = vec![];

        for arg in &function.arguments {
            let type_ = resolve_type(
                &self.root_module_declaration,
                self_type.without_last(),
                &arg.type_,
                ErrorLocation::Position(function_path, arg.position),
            )?;

            if *type_.kind() == types::TypeKind::Unit {
                return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                    argument_name: types::Identifier::parse(&arg.name),
                }
                .at(ErrorLocation::Position(function_path, arg.position)));
            }

            arguments.push(DeclaredArgument {
                function_name: function_path,
                name: types::Identifier::parse(&arg.name),
                type_: arg.type_.clone(),
                position: arg.position,
            });
        }

        Ok(DeclaredAssociatedFunction {
            struct_: function_path.without_last(),
            name: function_name,
            definition: DeclaredFunctionDefinition {
                arguments,
                return_type: function.return_type.clone(),
                ast: function.clone(),
                position,
            },
            visibility: Self::convert_visibility(function.visibility),
        })
    }

    fn type_check_function_declaration(
        function: &ast::Function,
        module_path: types::FQName,
        position: ast::SourceRange,
    ) -> DeclaredFunction {
        let function_name = types::Identifier::parse(&function.name);
        let function_path = module_path.with_part(function_name);

        let mut arguments = vec![];

        for arg in &function.arguments {
            arguments.push(DeclaredArgument {
                function_name: function_path,
                name: types::Identifier::parse(&arg.name),
                type_: arg.type_.clone(),
                position: arg.position,
            });
        }

        DeclaredFunction {
            name: function_path,
            definition: DeclaredFunctionDefinition {
                arguments,
                return_type: function.return_type.clone(),
                ast: function.clone(),
                position,
            },
        }
    }

    pub(crate) fn new(
        root_module_declaration: DeclaredModule<'pre>,
        structs: HashMap<types::StructId, types::Struct>,
    ) -> Self {
        Self {
            root_module_declaration,
            declared_impls: HashMap::new(),
            main: None,
            structs,
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
            self.structs,
        ))
    }
}
