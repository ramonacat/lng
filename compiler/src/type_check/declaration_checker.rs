// TODO add checking around the correctnes of visibility (i.e. that only visible items can be
// imported)
use std::collections::HashMap;

use crate::{
    ast,
    identifier::FQName,
    std::TYPE_NAME_STRING,
    types::{self, store::TypeStore as _},
};

use super::{
    DeclaredFunction, DeclaredStructField,
    declarations::{DeclaredItemKind, DeclaredRootModule, resolve_type},
    definition_checker::DefinitionChecker,
    errors::{TypeCheckError, TypeCheckErrorDescription},
};

pub(super) struct DeclarationChecker {
    root_module_declaration: DeclaredRootModule,
    declared_impls: HashMap<types::structs::StructId, Vec<types::functions::FunctionId>>,
    main: Option<types::functions::FunctionId>,
    types: types::store::MultiStore,
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
                    ast::DeclarationKind::Interface(_)
                    | ast::DeclarationKind::Function(_)
                    | ast::DeclarationKind::Struct(_) => {}
                    ast::DeclarationKind::Impl(impl_declaration) => {
                        let struct_name = impl_declaration.struct_name;
                        let struct_id =
                            types::structs::StructId::InModule(module_path, struct_name);

                        let functions = impl_declaration
                            .functions
                            .iter()
                            .map(|f| {
                                self.type_check_associated_function_declaration(
                                    f,
                                    module_path,
                                    struct_id,
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
                                        type_id: x.type_id,
                                        position,
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            fields_to_add.insert(
                                function.ast.name,
                                DeclaredStructField {
                                    // TODO we should here handle the case of the type actually
                                    // being generic
                                    type_: self.types.add(types::Type::new(
                                        types::TypeKind::Callable(function.id),
                                    )),
                                    static_: true,
                                },
                            );

                            self.declared_impls
                                .entry(struct_id)
                                .or_default()
                                .push(function.id);

                            self.root_module_declaration
                                .functions
                                .insert(function.id, function.clone());
                        }

                        let interface_id = impl_declaration.interface_name.map(|interface_name| {
                            let (module_path, interface_name) = self
                                .root_module_declaration
                                .resolve_import(module_path, interface_name);

                            let DeclaredItemKind::Interface(interface) = self
                                .root_module_declaration
                                .get_item(module_path, interface_name)
                                .unwrap()
                            else {
                                todo!();
                            };

                            interface.id
                        });

                        let structs = &mut self.root_module_declaration.structs;

                        let Some(struct_to_modify) = structs.get_mut(&struct_id) else {
                            return Err(TypeCheckErrorDescription::StructDoesNotExist(struct_id)
                                .at(position));
                        };

                        for (field_name, field) in fields_to_add {
                            struct_to_modify.fields.push(types::structs::StructField {
                                struct_id,
                                name: field_name,
                                type_: field.type_,
                                static_: field.static_,
                            });
                        }

                        if let Some(interface_id) = interface_id {
                            struct_to_modify.implemented_interfaces.insert(
                                interface_id,
                                functions.iter().map(|x| (x.ast.name, x.id)).collect(),
                            );
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
            let module_path = types::modules::ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Interface(interface) => {
                        self.type_check_interface(module_path, interface)?;
                    }
                    ast::DeclarationKind::Struct(_)
                    | ast::DeclarationKind::Impl(_)
                    | ast::DeclarationKind::Function(_) => {}
                }
            }
        }

        for file in program {
            let module_path = types::modules::ModuleId::parse(&file.name.to_string());

            for declaration in &file.declarations {
                match &declaration.kind {
                    ast::DeclarationKind::Struct(struct_) => {
                        self.type_check_struct(module_path, struct_)?;
                    }
                    ast::DeclarationKind::Interface(_)
                    | ast::DeclarationKind::Impl(_)
                    | ast::DeclarationKind::Function(_) => {}
                }
            }
        }

        for file in program {
            let module_path = types::modules::ModuleId::parse(&file.name.to_string());

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
                            .insert(function_id, function_declaration.clone());
                    }
                    ast::DeclarationKind::Struct(_)
                    | ast::DeclarationKind::Impl(_)
                    | ast::DeclarationKind::Interface(_) => {}
                }
            }
        }

        for file in program {
            let module_path = types::modules::ModuleId::parse(&file.name.to_string());

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
                                &mut self.types,
                            )?;
                        }
                    }
                    ast::DeclarationKind::Interface(_)
                    | ast::DeclarationKind::Function(_)
                    | ast::DeclarationKind::Impl(_) => {}
                }
            }
        }

        Ok(())
    }

    fn type_check_interface(
        &mut self,
        module_path: types::modules::ModuleId,
        interface: &ast::Interface,
    ) -> Result<(), TypeCheckError> {
        let interface_id = types::interfaces::InterfaceId::InModule(module_path, interface.name);
        let mut functions = HashMap::new();

        for function in &interface.declarations {
            let return_type = resolve_type(
                &self.root_module_declaration,
                module_path,
                &function.return_type,
                &mut self.types,
            )?;

            // TODO actually support generics here
            let type_ = types::Type::new(types::TypeKind::IndirectCallable(
                types::interfaces::InstantiatedInterfaceId::new(
                    interface_id,
                    types::generics::TypeArguments::new_empty(),
                ),
                function.name,
            ));
            let type_id = self.types.add(type_);

            functions.insert(
                function.name,
                types::interfaces::FunctionDeclaration {
                    type_id,
                    return_type: self.types.add(return_type),
                    arguments: function
                        .arguments
                        .iter()
                        .map(|x| self.type_check_argument(x, module_path))
                        .collect::<Result<Vec<_>, _>>()?,
                },
            );
        }

        self.root_module_declaration.interfaces.insert(
            interface_id,
            types::interfaces::Interface {
                id: interface_id,
                type_id: self
                    .types
                    .add(types::Type::new(types::TypeKind::Interface(interface_id))),
                functions,
            },
        );

        Ok(())
    }

    fn type_check_struct(
        &mut self,
        module_path: types::modules::ModuleId,
        struct_: &ast::Struct,
    ) -> Result<(), TypeCheckError> {
        let mut fields = vec![];
        let struct_id = types::structs::StructId::InModule(module_path, struct_.name);
        let struct_type = types::Type::new(types::TypeKind::Struct(
            types::structs::InstantiatedStructId::new(
                struct_id,
                types::generics::TypeArguments::new_empty(),
            ),
        ));

        for field in &struct_.fields {
            let field_type = resolve_type(
                &self.root_module_declaration,
                module_path,
                &field.type_,
                &mut self.types,
            )?;
            let field_type_id = self.types.add(field_type);
            fields.push(types::structs::StructField {
                type_: field_type_id,
                static_: false,
                struct_id,
                name: field.name,
            });
        }

        self.root_module_declaration.structs.insert(
            struct_id,
            types::structs::Struct {
                id: struct_id,
                fields,
                impls: vec![],
                type_id: self.types.add(struct_type),
                instance_type: self.types.add(types::Type::new(types::TypeKind::Object(
                    types::structs::InstantiatedStructId::new(
                        struct_id,
                        types::generics::TypeArguments::new_empty(),
                    ),
                ))),
                implemented_interfaces: HashMap::new(),
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
                    if let types::TypeKind::Array(array_item_type) =
                        self.types.get(argument.type_id).kind()
                    {
                        if let types::TypeKind::Object(instantiated_struct_id) =
                            self.types.get(*array_item_type).kind()
                        {
                            if instantiated_struct_id.id() == *TYPE_NAME_STRING {
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

    fn type_check_argument(
        &mut self,
        argument: &ast::Argument,
        current_module: types::modules::ModuleId,
    ) -> Result<types::functions::Argument, TypeCheckError> {
        let type_ = resolve_type(
            &self.root_module_declaration,
            current_module,
            &argument.type_,
            &mut self.types,
        )?;
        Ok(types::functions::Argument {
            name: argument.name,
            type_id: self.types.add(type_),
            position: argument.position,
        })
    }

    fn type_check_associated_function_declaration(
        &mut self,
        function: &ast::Function,
        current_module: types::modules::ModuleId,
        self_type: types::structs::StructId,
        position: ast::SourceSpan,
    ) -> Result<DeclaredFunction, TypeCheckError> {
        let mut arguments = vec![];

        for argument in &function.arguments {
            arguments.push(self.type_check_argument(argument, current_module)?);
        }

        let return_type = resolve_type(
            &self.root_module_declaration,
            current_module,
            &function.return_type,
            &mut self.types,
        )?;
        let id = types::functions::FunctionId::InStruct(self_type, function.name);
        Ok(DeclaredFunction {
            id,
            // TODO support generic functions
            type_id: self
                .types
                .add(types::Type::new(types::TypeKind::Callable(id))),
            module_name: current_module,
            arguments,
            // TODO figure out the correct error location here
            return_type: self.types.add(return_type),
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        })
    }

    fn type_check_function_declaration(
        &mut self,
        function: &ast::Function,
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
    ) -> DeclaredFunction {
        let mut arguments = vec![];

        for arg in &function.arguments {
            let type_ = resolve_type(
                &self.root_module_declaration,
                module_path,
                &arg.type_,
                &mut self.types,
            )
            .unwrap();

            arguments.push(types::functions::Argument {
                name: arg.name,
                type_id: self.types.add(type_),
                position: arg.position,
            });
        }

        let return_type = resolve_type(
            &self.root_module_declaration,
            module_path,
            &function.return_type,
            &mut self.types,
        )
        .unwrap();
        let id = types::functions::FunctionId::InModule(module_path, function.name);
        DeclaredFunction {
            id,
            // TODO support generic functions
            type_id: self
                .types
                .add(types::Type::new(types::TypeKind::Callable(id))),
            module_name: module_path,
            arguments,
            return_type: self.types.add(return_type),
            ast: function.clone(),
            position,
            visibility: Self::convert_visibility(function.visibility),
        }
    }

    pub(crate) fn new(
        root_module_declaration: DeclaredRootModule,
        types: types::store::MultiStore,
    ) -> Self {
        Self {
            root_module_declaration,
            declared_impls: HashMap::new(),
            main: None,
            types,
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
            self.types,
        ))
    }
}
