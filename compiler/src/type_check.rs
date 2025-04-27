use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{self, SourceRange},
    // TODO refer to the types as types::*, instead of importing them here
    types::{self, FieldPath, Identifier, Import, ItemPath, ModulePath, Visibility},
};

impl types::Item {
    fn type_(&self, declared_modules: &DeclaredModules<'_>) -> types::Type {
        match &self.kind {
            types::ItemKind::Function(function) => types::Type::Callable {
                kind: function.kind.clone(),
                arguments: function.arguments.clone(),
                return_type: Box::new(function.return_type.clone()),
            },
            types::ItemKind::Struct(struct_) => {
                types::Type::StructDescriptor(struct_.name.clone(), struct_.fields.clone())
            }
            types::ItemKind::Import(import) => declared_modules
                .find_struct(&import.imported_item)
                .unwrap()
                .type_(declared_modules),
        }
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<ast::SourceFile>);

#[derive(Debug)]
pub struct TypeCheckError {
    description: TypeCheckErrorDescription,
    item: types::ItemPath,
    position: ast::SourceRange,
}

impl Error for TypeCheckError {}
impl Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error {} at {} in {}",
            self.description, self.position, self.item
        )
    }
}

#[derive(Debug)]
pub enum TypeCheckErrorDescription {
    UnexpectedArgumentTypeInFunctionCall {
        target: ast::Expression,
        argument_name: Identifier,
        expected_type: types::Type,
        actual_type: types::Type,
    },
    IncorrectNumberOfArgumentsPassed(types::Type),
    FunctionArgumentCannotBeVoid {
        function_path: types::ItemPath,
        argument_name: Identifier,
    },
    ModuleDoesNotExist(types::ModulePath),
    ItemDoesNotExist(types::ItemPath),
    ItemNotExported(types::ModulePath, types::Identifier),
    UndeclaredVariable(types::Identifier),
    ImplNotOnStruct(types::ItemPath),
    MismatchedAssignmentType {
        target_variable: Identifier,
        variable_type: types::Type,
        assigned_type: types::Type,
    },
    CallingNotCallableItem(types::Type),
    MismatchedReturnType {
        actual: types::Type,
        expected: types::Type,
    },
}

impl TypeCheckErrorDescription {
    const fn at(self, item: types::ItemPath, position: ast::SourceRange) -> TypeCheckError {
        TypeCheckError {
            description: self,
            item,
            position,
        }
    }
}

impl Display for TypeCheckErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedArgumentTypeInFunctionCall {
                target,
                argument_name,
                expected_type,
                actual_type,
            } => write!(
                f,
                "Incorrect argument type {actual_type} for argument {argument_name} of type {expected_type} in a call to {target}"
            ),
            Self::IncorrectNumberOfArgumentsPassed(name) => {
                write!(f, "Incorrect number of arguments passed to {name}")
            }
            Self::FunctionArgumentCannotBeVoid {
                function_path,
                argument_name,
            } => write!(
                f,
                "Argument {argument_name} in the declaration of {function_path} cannot be of type void"
            ),
            Self::ModuleDoesNotExist(module_path) => {
                write!(f, "Module {module_path} does not exist")
            }
            Self::ItemDoesNotExist(item_path) => write!(f, "Item {item_path} does not exist"),
            Self::ItemNotExported(module_path, identifier) => write!(
                f,
                "Item {identifier} exists in module {module_path}, but is not exported"
            ),
            Self::UndeclaredVariable(identifier) => {
                write!(f, "Variable {identifier} does not exist")
            }
            Self::ImplNotOnStruct(identifier) => {
                write!(f, "{identifier} is not a struct, impl is not allowed")
            }
            Self::MismatchedAssignmentType {
                target_variable,
                variable_type,
                assigned_type,
            } => write!(
                f,
                "Cannot assign value of type {assigned_type} to variiable {target_variable} of typee {variable_type}"
            ),
            Self::CallingNotCallableItem(identifier) => {
                write!(f, "{identifier} cannot be called")
            }
            Self::MismatchedReturnType { actual, expected } => write!(
                f,
                "Function was expected to return {expected}, but returns {actual}"
            ),
        }
    }
}

fn convert_type(module: &ModulePath, type_: &ast::TypeDescription) -> types::Type {
    match type_ {
        ast::TypeDescription::Array(type_description) => {
            types::Type::Array(Box::new(convert_type(module, type_description)))
        }
        ast::TypeDescription::Named(name) if name == "void" => types::Type::Void,
        ast::TypeDescription::Named(name) if name == "u64" => types::Type::U64,
        ast::TypeDescription::Named(name) if name == "string" => {
            types::Type::Object(types::ItemPath::new(
                types::ModulePath::parse("std"),
                types::Identifier::parse("string"),
            ))
        }
        ast::TypeDescription::Named(name) => types::Type::Object(types::ItemPath::new(
            module.clone(),
            types::Identifier::parse(name),
        )),
    }
}

#[derive(Debug, Clone)]
struct DeclaredArgument {
    name: Identifier,
    type_: types::Type,
    position: SourceRange,
}

#[derive(Debug, Clone)]
struct DeclaredFunction {
    kind: types::CallableKind,
    arguments: Vec<DeclaredArgument>,
    return_type: types::Type,
    ast: ast::Function,
}

#[derive(Debug, Clone)]
struct DeclaredStructField {
    type_: types::Type,
    static_: bool,
}

// TODO support aliased imports
#[derive(Debug, Clone)]
struct DeclaredImport {
    imported_item: ItemPath,
    position: SourceRange,
}

#[derive(Debug, Clone)]
struct DeclaredStruct {
    name: ItemPath,
    fields: HashMap<FieldPath, DeclaredStructField>,
}

#[derive(Debug, Clone)]
enum DeclaredItemKind {
    Function(DeclaredFunction),
    Struct(DeclaredStruct),
    Import(DeclaredImport),
    Checked(types::Item),
}

#[derive(Debug, Clone)]
struct DeclaredItem {
    kind: DeclaredItemKind,
    visibility: types::Visibility,
}

impl DeclaredItem {
    fn type_(&self, declared_modules: &DeclaredModules) -> types::Type {
        match &self.kind {
            DeclaredItemKind::Function(declared_function) => types::Type::Callable {
                kind: declared_function.kind.clone(),
                arguments: declared_function
                    .arguments
                    .iter()
                    .map(|declaration| types::Argument {
                        name: declaration.name.clone(),
                        type_: declaration.type_.clone(),
                        position: declaration.position,
                    })
                    .collect(),
                return_type: Box::new(declared_function.return_type.clone()),
            },
            DeclaredItemKind::Struct(declared_struct) => types::Type::StructDescriptor(
                declared_struct.name.clone(),
                declared_struct
                    .fields
                    .iter()
                    .map(|(field_name, declaration)| types::StructField {
                        name: field_name.clone(),
                        type_: declaration.type_.clone(),
                        static_: declaration.static_,
                    })
                    .collect(),
            ),
            DeclaredItemKind::Import(DeclaredImport { imported_item, .. }) => declared_modules
                .find_struct(imported_item)
                .unwrap()
                .type_(declared_modules),
            DeclaredItemKind::Checked(item) => item.type_(declared_modules),
        }
    }
}

struct DeclaredModules<'src> {
    local: HashMap<ItemPath, DeclaredItem>,
    std: Option<&'src types::Program>,
}
impl<'src> DeclaredModules<'src> {
    fn new(std: Option<&'src types::Program>) -> Self {
        Self {
            local: HashMap::new(),
            std,
        }
    }

    fn declare(&mut self, items: HashMap<ItemPath, DeclaredItem>) {
        for item in items {
            self.local.insert(item.0, item.1);
        }
    }

    // TODO rename to find_item
    fn find_struct(&self, item_path: &ItemPath) -> Option<DeclaredItem> {
        if let Some(local_item) = self.local.get(item_path) {
            return Some(local_item.clone());
        }

        if let Some(std) = self.std {
            if let Some(std_module) = std.modules.get(&item_path.module) {
                if let Some(std_item) = std_module.items.get(item_path) {
                    return Some(DeclaredItem {
                        kind: DeclaredItemKind::Checked(std_item.clone()),
                        visibility: std_item.visibility,
                    });
                }
            }
        }

        None
    }

    fn find_local_mut(&mut self, item_path: &ItemPath) -> Option<&mut DeclaredItem> {
        if let Some(local_item) = self.local.get_mut(item_path) {
            return Some(local_item);
        }

        None
    }

    fn declare_item(&mut self, item_path: ItemPath, item: DeclaredItem) {
        self.local.insert(item_path, item);
    }

    // TODO Return Option<> here, if the module does not exist return None
    fn get_declared_types(&self) -> HashMap<types::ItemPath, types::Type> {
        let mut results: HashMap<types::ItemPath, types::Type> = self
            .local
            .iter()
            .map(|(name, item)| (name.clone(), item.type_(self)))
            .collect();

        if let Some(std) = self.std {
            for module in std.modules.values() {
                for (item_name, item) in &module.items {
                    results.insert(item_name.clone(), item.type_(self));
                }
            }
        }

        results
    }
}

// TODO split into smaller functions
#[allow(clippy::too_many_lines)]
pub fn type_check(
    program: &Program,
    std: Option<&types::Program>,
) -> Result<types::Program, TypeCheckError> {
    let mut declared_modules = DeclaredModules::new(std);

    for file in &program.0 {
        let mut items: HashMap<ItemPath, DeclaredItem> = HashMap::new();
        let module_path = ModulePath::parse(&file.name);

        for declaration in &file.declarations {
            match declaration {
                ast::Declaration::Function(function) => {
                    let declaration =
                        type_check_function_declaration(function, None, &module_path)?;

                    items.insert(
                        ItemPath::new(module_path.clone(), Identifier::parse(&function.name)),
                        DeclaredItem {
                            kind: DeclaredItemKind::Function(declaration.clone()),
                            visibility: if function.export {
                                Visibility::Export
                            } else {
                                Visibility::Internal
                            },
                        },
                    );
                }
                ast::Declaration::Struct(struct_) => {
                    let mut fields = HashMap::new();
                    // TODO check the types exist, possibly in separate pass
                    for field in &struct_.fields {
                        fields.insert(
                            types::FieldPath::new(
                                types::ItemPath::new(
                                    module_path.clone(),
                                    Identifier::parse(&struct_.name),
                                ),
                                Identifier::parse(&field.name),
                            ),
                            DeclaredStructField {
                                type_: convert_type(&module_path, &field.type_),
                                static_: false,
                            },
                        );
                    }
                    let name =
                        types::ItemPath::new(module_path.clone(), Identifier::parse(&struct_.name));
                    items.insert(
                        name.clone(),
                        DeclaredItem {
                            kind: DeclaredItemKind::Struct(DeclaredStruct { name, fields }),
                            // TODO structs should have configurable visibilty
                            visibility: Visibility::Export,
                        },
                    );
                }
                ast::Declaration::Impl(_) => {}
            }
        }

        declared_modules.declare(items);
    }
    // TODO check if we still need this
    let mut declared_impls: HashMap<FieldPath, DeclaredFunction> = HashMap::new();

    for file in &program.0 {
        let module_path = ModulePath::parse(&file.name);
        for declaration in &file.declarations {
            let position = declaration.position();

            match declaration {
                ast::Declaration::Function(_) | ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_declaration) => {
                    let struct_name = types::Identifier::parse(&impl_declaration.struct_name);
                    let struct_path =
                        types::ItemPath::new(module_path.clone(), struct_name.clone());

                    let functions = impl_declaration
                        .functions
                        .iter()
                        .map(|f| {
                            type_check_function_declaration(
                                f,
                                Some(struct_path.clone()),
                                &module_path,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = declared_modules.find_local_mut(&struct_path) else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            struct_path.clone(),
                        )
                        .at(struct_path, position));
                    };

                    let DeclaredItem {
                        kind: DeclaredItemKind::Struct(DeclaredStruct { fields, .. }),
                        ..
                    } = struct_
                    else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(
                            struct_path.clone(),
                        )
                        .at(struct_path, position));
                    };

                    for function in &functions {
                        match &function.kind {
                            types::CallableKind::Free { .. } => todo!(),

                            types::CallableKind::Associated { name, .. } => {
                                fields.insert(
                                    name.clone(),
                                    DeclaredStructField {
                                        type_: types::Type::Callable {
                                            kind: function.kind.clone(),
                                            arguments: function
                                                .arguments
                                                .iter()
                                                .map(|x| types::Argument {
                                                    name: x.name.clone(),
                                                    type_: x.type_.clone(),
                                                    position,
                                                })
                                                .collect(),
                                            return_type: Box::new(function.return_type.clone()),
                                        },
                                        static_: true,
                                    },
                                );

                                declared_impls.insert(name.clone(), function.clone())
                            }
                        };
                    }
                }
            }
        }
    }

    let mut impls: HashMap<types::FieldPath, types::Function> = HashMap::new();

    for file in &program.0 {
        let module_path = ModulePath::parse(&file.name);

        for import in &file.imports {
            let (name, path) = import.path.split_last().unwrap();
            let exporting_module_name =
                types::ModulePath::from_parts(path.iter().map(String::as_str));
            let item_name = types::Identifier::parse(name);

            let exported_item_path =
                ItemPath::new(exporting_module_name.clone(), item_name.clone());
            let imported_item_path = ItemPath::new(module_path.clone(), item_name.clone());

            let Some(imported_item) = declared_modules.find_struct(&exported_item_path) else {
                return Err(
                    TypeCheckErrorDescription::ItemDoesNotExist(exported_item_path)
                        .at(imported_item_path, import.position),
                );
            };

            match &imported_item.kind {
                DeclaredItemKind::Function(DeclaredFunction { .. })
                | DeclaredItemKind::Struct { .. }
                | DeclaredItemKind::Checked(types::Item {
                    kind:
                        types::ItemKind::Function(types::Function { .. })
                        | types::ItemKind::Struct(types::Struct { .. }),
                    ..
                }) => {
                    if imported_item.visibility != Visibility::Export {
                        return Err(TypeCheckErrorDescription::ItemNotExported(
                            exporting_module_name,
                            item_name,
                        )
                        .at(imported_item_path, import.position));
                    }
                    let importing_module_path = types::ModulePath::parse(&file.name);
                    declared_modules.declare_item(
                        ItemPath::new(importing_module_path, item_name.clone()),
                        DeclaredItem {
                            kind: DeclaredItemKind::Import(DeclaredImport {
                                position: import.position,
                                imported_item: types::ItemPath::new(
                                    exporting_module_name,
                                    item_name,
                                ),
                            }),
                            visibility: imported_item.visibility,
                        },
                    );
                }
                DeclaredItemKind::Import(_)
                | DeclaredItemKind::Checked(types::Item {
                    kind: types::ItemKind::Import(_),
                    ..
                }) => {
                    return Err(
                        TypeCheckErrorDescription::ItemDoesNotExist(exported_item_path)
                            .at(imported_item_path, import.position),
                    );
                }
            }
        }

        let globals = declared_modules.get_declared_types();
        dbg!(&globals.keys());

        for item in &file.declarations {
            match item {
                ast::Declaration::Function(_) | ast::Declaration::Struct(_) => {}
                ast::Declaration::Impl(impl_declaration) => {
                    let position = item.position();
                    let struct_name = types::Identifier::parse(&impl_declaration.struct_name);
                    let struct_path = types::ItemPath::new(module_path.clone(), struct_name);

                    let declared_impl =
                        declared_impls.iter().filter(|x| x.0.struct_ == struct_path);

                    let functions = declared_impl
                        .map(|(_, f)| type_check_function(f, &globals))
                        .collect::<Result<Vec<_>, _>>()?;

                    let Some(struct_) = declared_modules.find_local_mut(&struct_path) else {
                        return Err(TypeCheckErrorDescription::ItemDoesNotExist(
                            struct_path.clone(),
                        )
                        .at(struct_path, position));
                    };

                    // TODO DeclaredItem should have a is_struct method for this!
                    let DeclaredItem {
                        kind: DeclaredItemKind::Struct(DeclaredStruct { fields, .. }),
                        ..
                    } = struct_
                    else {
                        return Err(TypeCheckErrorDescription::ImplNotOnStruct(
                            struct_path.clone(),
                        )
                        .at(struct_path, position));
                    };

                    for function in &functions {
                        match &function.kind {
                            types::CallableKind::Free { .. } => todo!(),
                            types::CallableKind::Associated { name, .. } => {
                                impls.insert(name.clone(), function.clone());

                                fields.insert(
                                    name.clone(),
                                    DeclaredStructField {
                                        type_: function.type_(),
                                        static_: true,
                                    },
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    let mut module_items: HashMap<ItemPath, types::Item> = HashMap::new();
    for (item_path, declared_item) in &declared_modules.local {
        let globals = declared_modules.get_declared_types();

        {
            match &declared_item.kind {
                DeclaredItemKind::Function(declared_function) => {
                    let body = type_check_function(declared_function, &globals)?;

                    match &declared_function.kind {
                        types::CallableKind::Free { name } => {
                            module_items.insert(
                                name.clone(),
                                types::Item {
                                    kind: types::ItemKind::Function(body),
                                    visibility: declared_item.visibility,
                                },
                            );
                        }
                        types::CallableKind::Associated { .. } => todo!(),
                    }
                }
                DeclaredItemKind::Struct(DeclaredStruct {
                    name: struct_name,
                    fields,
                }) => {
                    module_items.insert(
                        struct_name.clone(),
                        types::Item {
                            kind: types::ItemKind::Struct(types::Struct {
                                name: struct_name.clone(),
                                fields: fields
                                    .iter()
                                    .map(|(field_name, declaration)| types::StructField {
                                        name: field_name.clone(),
                                        type_: declaration.type_.clone(),
                                        static_: declaration.static_,
                                    })
                                    .collect(),
                                impls: impls
                                    .iter()
                                    .filter(|f| &f.0.struct_ == struct_name)
                                    .map(|(k, v)| (k.clone(), v.clone()))
                                    .collect(),
                            }),
                            visibility: declared_item.visibility,
                        },
                    );
                }
                DeclaredItemKind::Import(DeclaredImport {
                    imported_item,
                    position,
                }) => {
                    let imported_item = declared_modules.find_struct(imported_item).unwrap();
                    match &imported_item.kind {
                        DeclaredItemKind::Struct(DeclaredStruct {
                            name: item_name, ..
                        })
                        | DeclaredItemKind::Function(DeclaredFunction {
                            kind: types::CallableKind::Free { name: item_name },
                            ..
                        })
                        | DeclaredItemKind::Checked(types::Item {
                            kind:
                                types::ItemKind::Function(types::Function {
                                    kind: types::CallableKind::Free { name: item_name },
                                    ..
                                })
                                | types::ItemKind::Struct(types::Struct {
                                    name: item_name, ..
                                }),
                            ..
                        }) => module_items.insert(
                            item_path.clone(),
                            types::Item {
                                kind: types::ItemKind::Import(Import {
                                    imported_item: item_name.clone(),
                                    position: *position,
                                }),
                                visibility: Visibility::Internal, // TODO can imports be reexported?
                            },
                        ),
                        DeclaredItemKind::Function(DeclaredFunction {
                            kind: types::CallableKind::Associated { .. },
                            ..
                        }) => todo!(),
                        DeclaredItemKind::Import(_) => todo!(),
                        DeclaredItemKind::Checked(types::Item {
                            kind: types::ItemKind::Import(_),
                            ..
                        }) => todo!(),
                        DeclaredItemKind::Checked(types::Item {
                            kind:
                                types::ItemKind::Function(types::Function {
                                    kind: types::CallableKind::Associated { .. },
                                    ..
                                }),
                            ..
                        }) => todo!(),
                    };
                }
                DeclaredItemKind::Checked(_) => todo!(),
            }
        }
    }

    let mut items_by_module: HashMap<ModulePath, HashMap<ItemPath, _>> = HashMap::new();

    for (path, item) in module_items {
        items_by_module
            .entry(path.module.clone())
            .or_default()
            .insert(path, item);
    }

    Ok(types::Program {
        modules: items_by_module
            .into_iter()
            .map(|(module_path, items)| (module_path, types::Module { items }))
            .collect(),
    })
}

fn type_check_function(
    declared_function: &DeclaredFunction,
    globals: &HashMap<ItemPath, types::Type>, // TODO this should have Identifier as the key, as
                                              // all locals are in the current module anyway
) -> Result<types::Function, TypeCheckError> {
    let mut locals: HashMap<Identifier, types::Type> = HashMap::new();
    for argument in &declared_function.arguments {
        locals.insert(argument.name.clone(), argument.type_.clone());
    }

    // TODO the item_path should really be multi-variant, so we know whether it's inside a struct,
    // or inside a specific function
    let item_path = match &declared_function.kind {
        types::CallableKind::Free { name } => name.clone(),
        types::CallableKind::Associated { self_type: _, name } => name.struct_.clone(),
    };

    let body = match &declared_function.ast.body {
        ast::FunctionBody::Statements(body_statements, _) => {
            let mut checked_statements = vec![];

            for statement in body_statements {
                let checked_statement = match statement {
                    ast::Statement::Expression(expression, _) => types::Statement::Expression(
                        type_check_expression(expression, &locals, globals, &item_path)?,
                    ),

                    ast::Statement::Let(name, type_, expression) => {
                        let checked_expression =
                            type_check_expression(expression, &locals, globals, &item_path)?;
                        let type_ = convert_type(&item_path.module, type_);

                        if checked_expression.type_ != type_ {
                            return Err(TypeCheckErrorDescription::MismatchedAssignmentType {
                                target_variable: types::Identifier::parse(name),
                                variable_type: type_,
                                assigned_type: checked_expression.type_,
                            }
                            .at(item_path, expression.position));
                        }

                        locals.insert(types::Identifier::parse(name), type_);

                        types::Statement::Let(types::LetStatement {
                            binding: Identifier::parse(name),
                            value: checked_expression,
                        })
                    }
                    ast::Statement::Return(expression, _) => {
                        // TODO verify that all paths return a value
                        let checked_expression =
                            type_check_expression(expression, &locals, globals, &item_path)?;

                        if checked_expression.type_ != declared_function.return_type {
                            return Err(TypeCheckErrorDescription::MismatchedReturnType {
                                actual: checked_expression.type_,
                                expected: declared_function.return_type.clone(),
                            }
                            .at(item_path, expression.position));
                        }

                        types::Statement::Return(checked_expression)
                    }
                };

                checked_statements.push(checked_statement);
            }

            types::FunctionBody::Statements(checked_statements)
        }
        ast::FunctionBody::Extern(_) => types::FunctionBody::Extern,
    };

    Ok(types::Function {
        kind: declared_function.kind.clone(),
        arguments: declared_function
            .arguments
            .iter()
            .map(|argument| types::Argument {
                name: argument.name.clone(),
                type_: argument.type_.clone(),
                position: argument.position,
            })
            .collect(),
        return_type: declared_function.return_type.clone(),
        body,
        position: declared_function.ast.position,
    })
}

fn type_check_function_declaration(
    function: &ast::Function,
    self_type: Option<ItemPath>,
    module_path: &ModulePath,
) -> Result<DeclaredFunction, TypeCheckError> {
    let function_name = types::Identifier::parse(&function.name);
    // TODO this should take both paths with and wihout a struct_name into
    // consideration!
    let function_path = types::ItemPath::new(module_path.clone(), function_name.clone());

    let mut arguments = vec![];

    for arg in &function.arguments {
        let type_ = convert_type(module_path, &arg.type_);

        if type_ == types::Type::Void {
            return Err(TypeCheckErrorDescription::FunctionArgumentCannotBeVoid {
                function_path: function_path.clone(),
                argument_name: types::Identifier::parse(&arg.name),
            }
            .at(function_path, arg.position));
        }

        arguments.push(DeclaredArgument {
            name: types::Identifier::parse(&arg.name),
            type_: convert_type(module_path, &arg.type_),
            position: arg.position,
        });
    }

    Ok(DeclaredFunction {
        kind: self_type.map_or_else(
            || types::CallableKind::Free {
                name: function_path,
            },
            |item_name| types::CallableKind::Associated {
                self_type: item_name.clone(),
                name: types::FieldPath::new(item_name, function_name),
            },
        ),
        arguments,
        return_type: convert_type(module_path, &function.return_type),
        ast: function.clone(),
    })
}

// TODO split it into smaller functions
#[allow(clippy::too_many_lines)]
fn type_check_expression(
    expression: &ast::Expression,
    locals: &HashMap<Identifier, types::Type>,
    globals: &HashMap<ItemPath, types::Type>,
    item_path: &ItemPath,
) -> Result<types::Expression, TypeCheckError> {
    let position = expression.position;

    match &expression.kind {
        ast::ExpressionKind::FunctionCall {
            target,
            arguments: passed_arguments,
        } => {
            let checked_target = type_check_expression(target, locals, globals, item_path)?;

            let types::Type::Callable {
                ref kind,
                arguments: ref callable_arguments,
                ref return_type,
            } = checked_target.type_
            else {
                return Err(TypeCheckErrorDescription::CallingNotCallableItem(
                    checked_target.type_,
                )
                .at(item_path.clone(), position));
            };
            let mut callable_arguments = callable_arguments.clone();

            let self_adjustment = match &kind {
                types::CallableKind::Free { .. } => 0,
                types::CallableKind::Associated { .. } => 1,
            };

            if passed_arguments.len() + self_adjustment != callable_arguments.len() {
                return Err(TypeCheckErrorDescription::IncorrectNumberOfArgumentsPassed(
                    checked_target.type_.clone(),
                )
                .at(item_path.clone(), position));
            }

            let mut checked_arguments = vec![];
            if let types::CallableKind::Associated {
                self_type: self_type_name,
                name: _,
            } = kind
            {
                if callable_arguments[0].name == Identifier::parse("self") {
                    let mut callable_arguments_iter = callable_arguments.into_iter();
                    let self_argument = callable_arguments_iter.next().unwrap();

                    callable_arguments = callable_arguments_iter.collect();

                    let self_type = globals.get(self_type_name);
                    let Some(full_self_type @ types::Type::StructDescriptor(target_struct_name, _)) =
                        self_type
                    else {
                        todo!("{self_type:?}");
                    };

                    // TODO the struct descriptor should have a method that returns its instance
                    // type, so that this code does not rot
                    // TODO this may have funny consequences if there's a shadowing type with the
                    // same name as a builtin, we should probably simply disallow that
                    // TODO we should be using convert_type here
                    let expected_type = match target_struct_name.item.raw() {
                        "u64" => types::Type::U64,
                        "string" => types::Type::Object(types::ItemPath::new(
                            types::ModulePath::parse("std"),
                            types::Identifier::parse("string"),
                        )),
                        _ => types::Type::Object(target_struct_name.clone()),
                    };

                    if self_argument.type_ != expected_type {
                        return Err(
                            TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                                target: *target.clone(),
                                argument_name: self_argument.name,
                                expected_type,
                                actual_type: self_argument.type_,
                            }
                            .at(item_path.clone(), position),
                        );
                    }

                    checked_arguments.push(types::Expression {
                        position: self_argument.position,
                        type_: full_self_type.clone(),
                        kind: types::ExpressionKind::SelfAccess,
                    });
                }
            }

            for (argument, called_function_argument) in
                passed_arguments.iter().zip(callable_arguments)
            {
                let checked_argument = type_check_expression(argument, locals, globals, item_path)?;
                let expected_type = &called_function_argument.type_;

                if &checked_argument.type_ != expected_type {
                    return Err(
                        TypeCheckErrorDescription::UnexpectedArgumentTypeInFunctionCall {
                            target: *target.clone(),
                            argument_name: called_function_argument.name,
                            expected_type: expected_type.clone(),
                            actual_type: checked_argument.type_,
                        }
                        .at(item_path.clone(), argument.position),
                    );
                }

                checked_arguments.push(checked_argument);
            }

            Ok(types::Expression {
                position,
                type_: *return_type.clone(),
                kind: types::ExpressionKind::FunctionCall {
                    target: Box::new(checked_target),
                    arguments: checked_arguments,
                },
            })
        }
        ast::ExpressionKind::Literal(literal) => match literal {
            ast::Literal::String(value, _) => Ok(types::Expression {
                position,
                type_: types::Type::Object(types::ItemPath::new(
                    types::ModulePath::parse("std"),
                    types::Identifier::parse("string"),
                )),
                kind: types::ExpressionKind::Literal(types::Literal::String(value.clone())),
            }),
            ast::Literal::UnsignedInteger(value) => Ok(types::Expression {
                position,
                type_: types::Type::U64,
                kind: types::ExpressionKind::Literal(types::Literal::UnsignedInteger(*value)),
            }),
        },
        ast::ExpressionKind::VariableReference(name) => {
            let id = Identifier::parse(name);

            let value_type = locals
                .get(&id)
                .or_else(|| globals.get(&ItemPath::new(item_path.module.clone(), id.clone())))
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(item_path.clone(), position)
                })
                .cloned()?;

            Ok(types::Expression {
                position,
                type_: value_type,
                kind: types::ExpressionKind::VariableAccess(id),
            })
        }
        ast::ExpressionKind::StructConstructor(struct_name) => {
            let id = Identifier::parse(struct_name);

            // TODO make (locals, globals) and object, so that we can have this retrieval logic
            // centralized
            let types::Type::StructDescriptor(name, _) = locals
                .get(&id)
                .or_else(|| globals.get(&ItemPath::new(item_path.module.clone(), id.clone())))
                .ok_or_else(|| {
                    TypeCheckErrorDescription::UndeclaredVariable(id.clone())
                        .at(item_path.clone(), position)
                })
                .cloned()
                .unwrap()
            else {
                panic!();
            };

            Ok(types::Expression {
                position,
                type_: types::Type::Object(name),
                kind: types::ExpressionKind::StructConstructor(id),
            })
        }
        ast::ExpressionKind::FieldAccess { target, field_name } => {
            let target = type_check_expression(target, locals, globals, item_path)?;

            let type_name = match &target.type_ {
                types::Type::Void => todo!(),
                types::Type::Object(identifier) => identifier,
                types::Type::Array(_) => todo!(),
                types::Type::StructDescriptor(_, _) => todo!(),
                types::Type::Callable { .. } => todo!(),
                types::Type::U64 => {
                    &types::ItemPath::new(types::ModulePath::parse("std"), Identifier::parse("u64"))
                }
                types::Type::Pointer(_) => todo!(),
                types::Type::U8 => todo!(),
            };

            let target_type = globals.get(type_name).unwrap();
            let types::Type::StructDescriptor(_, fields) = target_type else {
                todo!("{target_type}");
            };

            let field_name = types::Identifier::parse(field_name);
            let field_path = types::FieldPath::new(type_name.clone(), field_name.clone());

            // TODO make fields a HashMap so we don't have to .find?
            let field_type = fields
                .iter()
                .find(|x| x.name == field_path)
                .unwrap()
                .type_
                .clone();

            Ok(types::Expression {
                position,
                type_: field_type,
                kind: types::ExpressionKind::FieldAccess {
                    target: Box::new(target),
                    field: field_name,
                },
            })
        }
    }
}
