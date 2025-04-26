mod context;
mod module;
mod rc_builder;
mod scope;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use context::{Builtins, CompilerContext};
use inkwell::{
    AddressSpace,
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicType, StructType},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
};
use module::CompiledModule;
use rc_builder::RcValue;
use scope::{GlobalScope, Scope};

use crate::{
    ast::SourceRange,
    name_mangler::{MangledIdentifier, mangle_field, mangle_item, mangle_module},
    runtime::register_mappings,
    types::{self, Identifier, ModulePath},
};

pub fn compile(
    program: types::Program,
    register_global_mappings: RegisterGlobalMappings,
) -> Result<(), CompileError> {
    let context = Context::create();
    let compiler = Compiler::new(&context);

    compiler.compile(&program, register_global_mappings)
}

type RegisterGlobalMappings = Option<Box<dyn FnOnce(&ExecutionEngine, &Module)>>;

struct Compiler<'ctx> {
    context: CompilerContext<'ctx>,
}

#[derive(Debug)]
pub enum ErrorLocation {
    Position(ModulePath, SourceRange),
    Module(ModulePath),
    Indeterminate,
}

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLocation::Position(module_path, source_range) => {
                write!(f, "{source_range} in module {module_path}")
            }
            ErrorLocation::Module(name) => write!(f, "module {name}"),
            ErrorLocation::Indeterminate => write!(f, "indeterminate"),
        }
    }
}

#[derive(Debug)]
pub struct CompileError {
    description: CompileErrorDescription,
    location: ErrorLocation,
}

#[derive(Debug)]
pub enum CompileErrorDescription {
    ModuleNotFound(ModulePath),
    FunctionNotFound {
        module_name: ModulePath,
        function_name: Identifier,
    },
    InternalError(String),
    StructNotFound {
        module_name: ModulePath,
        struct_name: types::Identifier,
    },
}

impl CompileErrorDescription {
    // TODO clean up all the unwraps so that this actually will be used :)
    #[allow(unused)]
    fn at(self, module_path: ModulePath, position: SourceRange) -> CompileError {
        CompileError {
            description: self,
            location: ErrorLocation::Position(module_path, position),
        }
    }

    fn in_module(self, name: ModulePath) -> CompileError {
        CompileError {
            description: self,
            location: ErrorLocation::Module(name),
        }
    }

    fn at_indeterminate(self) -> CompileError {
        CompileError {
            description: self,
            location: ErrorLocation::Indeterminate,
        }
    }
}

impl Display for CompileErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileErrorDescription::ModuleNotFound(name) => write!(f, "Module {name} not found"),
            CompileErrorDescription::FunctionNotFound {
                module_name,
                function_name,
            } => write!(
                f,
                "Function {function_name} was not found in module {module_name}"
            ),
            CompileErrorDescription::InternalError(message) => {
                write!(f, "Internal error: {message}")
            }
            CompileErrorDescription::StructNotFound {
                module_name,
                struct_name: function_name,
            } => write!(f, "Struct {function_name} not found in {module_name}"),
        }
    }
}

impl Error for CompileError {}
impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "compile error: {} at {}",
            self.description, self.location
        )
    }
}

impl From<BuilderError> for CompileErrorDescription {
    fn from(value: BuilderError) -> Self {
        Self::InternalError(format!("{}", value))
    }
}

trait IntoCompileError {
    fn into_compile_error_at(self, module_path: ModulePath, position: SourceRange) -> CompileError;
}

impl IntoCompileError for BuilderError {
    fn into_compile_error_at(self, module_path: ModulePath, position: SourceRange) -> CompileError {
        CompileError {
            description: self.into(),
            location: ErrorLocation::Position(module_path, position),
        }
    }
}

struct CompiledFunction<'ctx> {
    handle: FunctionHandle,

    entry: BasicBlock<'ctx>,
    end: BasicBlock<'ctx>,
    scope: Rc<Scope<'ctx>>,
    rcs: Vec<RcValue<'ctx>>,
    return_value: Option<Value<'ctx>>,
}

impl<'ctx> CompiledFunction<'ctx> {
    fn build_return(
        &self,
        return_value: Option<&dyn BasicValue<'ctx>>,
        context: &CompilerContext<'ctx>,
        module_path: ModulePath,
    ) -> Result<(), CompileError> {
        context
            .builder
            .build_return(return_value)
            .map_err(|e| e.into_compile_error_at(module_path.clone(), self.handle.location))?;

        Ok(())
    }

    fn set_return_value(&mut self, value: Value<'ctx>) {
        self.return_value = Some(value);
    }
}

#[derive(Debug, Clone)]
pub struct FunctionHandle {
    pub name: MangledIdentifier,
    pub location: SourceRange,
    pub arguments: Vec<types::Argument>,
    pub return_type: types::Type,
    pub export: bool,
}

impl<'ctx> FunctionHandle {
    // TODO move the declare_function* methods from Compiler into CompiledModule and use them here
    fn get_or_create_in_module(
        &self,
        module: &CompiledModule<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> FunctionValue<'ctx> {
        module
            .llvm_module
            .get_function(self.name.as_str())
            .unwrap_or_else(|| {
                module.declare_function(
                    self.export,
                    self.name.clone(),
                    &self.arguments,
                    &self.return_type,
                    context,
                )
            })
    }
}

#[derive(Debug, Clone)]
pub struct StructHandle<'ctx> {
    description: types::Struct,
    // TODO static_fileds should be in types::Struct really
    static_fields: HashMap<types::Identifier, Value<'ctx>>,
    llvm_type: StructType<'ctx>,
}

// TODO The *Handle structs should be lightweight handles, and not copied with the vecs and all
// that
#[derive(Debug, Clone)]
enum Value<'ctx> {
    Primitive(StructHandle<'ctx>, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    Function(FunctionHandle),
    Struct(StructHandle<'ctx>),
}
impl<'ctx> Value<'ctx> {
    fn to_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
        }
    }

    fn as_struct(&self) -> Option<StructHandle<'ctx>> {
        if let Value::Struct(handle) = self {
            Some(handle.clone())
        } else {
            None
        }
    }

    fn as_function(&self) -> Option<FunctionHandle> {
        if let Value::Function(handle) = self {
            Some(handle.clone())
        } else {
            None
        }
    }
}

impl<'src, 'ctx> Compiler<'ctx>
where
    'src: 'ctx,
{
    pub fn new(context: &'ctx Context) -> Self {
        let rc_type = context.struct_type(
            &[
                context.i64_type().as_basic_type_enum(), // refcount
                context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(), // target object
            ],
            false,
        );

        let string_type = context.struct_type(
            &[
                context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(), //characters
            ],
            false,
        );

        let builtins = Builtins {
            // TODO we skip the field so it is not accessible, in the future we should somehow add
            // impls from stdlib which will be methods of the strings
            // TODO StructHandle should have a ::new() to ensure description.impls are in sync
            // with static_fields
            string_handle: StructHandle {
                description: types::Struct {
                    name: types::Identifier::parse("string"),
                    mangled_name: mangle_item(
                        ModulePath::parse("std"),
                        Identifier::parse("string"),
                    ),
                    fields: vec![],
                    impls: HashMap::new(),
                },
                llvm_type: string_type,
                static_fields: HashMap::new(),
            },
            // TODO do we need the handle here? this is not exposed to userland anyway
            rc_handle: StructHandle {
                description: types::Struct {
                    name: types::Identifier::parse("rc"),
                    mangled_name: mangle_item(ModulePath::parse("std"), Identifier::parse("rc")),
                    fields: vec![],
                    impls: HashMap::new(),
                },
                llvm_type: rc_type,
                static_fields: HashMap::new(),
            },
        };

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                builtins,
            },
        }
    }

    // TODO instead of executing the code, this should return some object that exposes the
    // executable code with a safe interface
    pub fn compile(
        self,
        program: &'src types::Program,
        register_global_mappings: RegisterGlobalMappings,
    ) -> Result<(), CompileError> {
        let mut global_scope = GlobalScope::new();

        global_scope.register(
            Identifier::parse("string"),
            Value::Struct(self.context.builtins.string_handle.clone()),
        );

        for (path, program_module) in &program.modules {
            let module = self
                .context
                .llvm_context
                .create_module(mangle_module(path.clone()).as_str());
            let created_module = global_scope.create_module(path.clone(), module);

            for declaration in program_module.items.values() {
                match declaration {
                    types::Item::Function(function) => {
                        let function_handle = FunctionHandle {
                            export: function.is_exported(),
                            name: function.mangled_name.clone(),
                            return_type: function.return_type.clone(),
                            arguments: function.arguments.clone(),
                            location: function.location,
                        };

                        created_module
                            .set_variable(function.name.clone(), Value::Function(function_handle));
                    }
                    types::Item::Struct(struct_) => {
                        let field_types = struct_
                            .fields
                            .iter()
                            .map(|f| self.context.type_to_llvm(&f.type_).as_basic_type_enum())
                            .collect::<Vec<_>>();
                        let llvm_type = self
                            .context
                            .llvm_context
                            .struct_type(&field_types[..], false);

                        let static_fields = struct_
                            .impls
                            .iter()
                            .map(|(name, impl_)| {
                                let handle = FunctionHandle {
                                    export: impl_.export,
                                    name: impl_.mangled_name.clone(),
                                    return_type: impl_.return_type.clone(),
                                    arguments: impl_.arguments.clone(),
                                    location: impl_.location,
                                };
                                (name.clone(), Value::Function(handle))
                            })
                            .collect();

                        created_module.set_variable(
                            struct_.name.clone(),
                            Value::Struct(StructHandle {
                                description: struct_.clone(),
                                llvm_type,
                                static_fields,
                            }),
                        );
                    }
                    types::Item::Import(_) => {}
                };
            }
        }

        for (module_path, file) in &program.modules {
            let Some(module) = global_scope.get_module(module_path) else {
                return Err(
                    CompileErrorDescription::ModuleNotFound(module_path.clone()).at_indeterminate()
                );
            };

            for item in file.items.values() {
                // TODO support importing structs, and possibly other types
                let types::Item::Import(import) = item else {
                    continue;
                };

                let value = global_scope.get_value(&import.path, &import.item).unwrap();
                match value {
                    Value::Primitive(_, _) => todo!(),
                    Value::Reference(_) => todo!(),
                    Value::Function(function) => {
                        let mangled_name = mangle_item(import.path.clone(), import.item.clone());

                        module.import_function(&function, mangled_name.clone(), &self.context);

                        let Value::Function(f) =
                            global_scope.get_value(&import.path, &import.item).unwrap()
                        else {
                            todo!();
                        };

                        module.set_variable(
                            import.item.clone(),
                            Value::Function(FunctionHandle {
                                export: function.export,
                                name: mangled_name,
                                return_type: f.return_type,
                                location: import.location,
                                arguments: function.arguments.clone(),
                            }),
                        );
                    }
                    Value::Struct(ref struct_) => {
                        for (field_name, impl_) in &struct_.static_fields {
                            match impl_ {
                                Value::Primitive(_, _) => todo!(),
                                Value::Reference(_) => todo!(),
                                Value::Function(function_handle) => module.import_function(
                                    function_handle,
                                    mangle_field(
                                        import.path.clone(),
                                        import.item.clone(),
                                        field_name.clone(),
                                    ),
                                    &self.context,
                                ),
                                Value::Struct(_) => todo!(),
                            };
                        }

                        module.set_variable(import.item.clone(), value.clone());
                    }
                };
            }

            for item in file.items.values() {
                match item {
                    types::Item::Function(function) => {
                        self.compile_function(module_path, module, function, None)?;
                    }
                    types::Item::Import(_) => {}
                    types::Item::Struct(struct_) => {
                        for impl_ in struct_.impls.values() {
                            // TODO definitely mangle the name
                            self.compile_function(
                                module_path,
                                module,
                                impl_,
                                Some(struct_.name.clone()),
                            )?;
                        }
                    }
                }
            }
        }

        let root_module = self.context.llvm_context.create_module("root");
        for module in global_scope.into_modules() {
            println!("{}", module.path);

            println!(
                "{}",
                module
                    .llvm_module
                    .print_to_string()
                    .to_string()
                    .replace("\\n", "\n")
            );

            module.llvm_module.verify().unwrap();

            root_module
                .link_in_module(module.llvm_module)
                .map_err(|e| {
                    CompileErrorDescription::InternalError(e.to_string()).in_module(module.path)
                })?;
        }

        root_module.verify().unwrap();
        println!(
            "{}",
            root_module
                .print_to_string()
                .to_string()
                .replace("\\n", "\n")
        );

        let execution_engine = root_module
            .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
            .unwrap();

        (register_global_mappings.unwrap_or(Box::new(register_mappings)))(
            &execution_engine,
            &root_module,
        );

        unsafe {
            let main = execution_engine
                // TODO we should allow for main to be in any module, not just "main"
                .get_function::<unsafe extern "C" fn()>(
                    mangle_item(ModulePath::parse("main"), Identifier::parse("main")).as_str(),
                )
                .unwrap();
            main.call();
        }

        Ok(())
    }

    fn compile_function(
        &self,
        module_path: &ModulePath,
        module: &module::CompiledModule<'ctx>,
        function: &types::Function,
        struct_: Option<Identifier>,
    ) -> Result<(), CompileError> {
        let types::FunctionBody::Statements(statements) = &function.body else {
            return Ok(());
        };
        let mut compiled_function =
            module.begin_compile_function(function, &self.context, struct_)?;
        for statement in statements {
            let self_value = function.has_self().then(|| {
                compiled_function
                    .scope
                    .get_variable(&Identifier::parse("self"))
                    .unwrap()
            });

            match statement {
                types::Statement::Expression(expression) => {
                    self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
                        module_path.clone(),
                        module,
                    )?;
                }
                types::Statement::Let(let_) => {
                    let value = self.compile_expression(
                        &let_.value,
                        self_value,
                        &mut compiled_function,
                        module_path.clone(),
                        module,
                    )?;

                    compiled_function
                        .scope
                        .register(let_.binding.clone(), value.1);
                }
                types::Statement::Return(expression) => {
                    let value = self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
                        module_path.clone(),
                        module,
                    )?;

                    compiled_function.set_return_value(value.1);
                }
            }
        }
        let cleanup_label = rc_builder::build_cleanup(
            &self.context,
            &compiled_function
                .rcs
                .iter()
                .filter(|x| {
                    let Some(ref return_) = compiled_function.return_value else {
                        return true;
                    };

                    match return_ {
                        Value::Primitive(_, _) => true,
                        Value::Reference(rc_value) => rc_value.as_ptr() != x.as_ptr(),
                        Value::Function(_) => true,
                        Value::Struct(_) => true,
                    }
                })
                .cloned()
                .collect::<Vec<_>>(),
            compiled_function.end,
        )?;
        self.context
            .builder
            .position_at_end(compiled_function.entry);
        self.context
            .builder
            .build_unconditional_branch(cleanup_label)
            .unwrap();
        self.context.builder.position_at_end(compiled_function.end);
        // TODO can this match be somehow removed or simplified?
        if let Some(ref r) = compiled_function.return_value {
            match &r.to_basic_value() {
                BasicValueEnum::ArrayValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
                BasicValueEnum::IntValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
                BasicValueEnum::FloatValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
                BasicValueEnum::PointerValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
                BasicValueEnum::StructValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
                BasicValueEnum::VectorValue(v) => {
                    compiled_function.build_return(Some(v), &self.context, module_path.clone())?;
                }
            }
        } else {
            // TODO we should really just return void here, but type_to_llvm can't declare void
            // correctly (yet)
            compiled_function.build_return(
                Some(&self.context.llvm_context.i8_type().const_zero()),
                &self.context,
                module_path.clone(),
            )?;
        }
        Ok(())
    }

    fn compile_expression(
        &self,
        expression: &types::Expression,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: ModulePath,
        module: &CompiledModule<'ctx>,
    ) -> Result<(Option<Value<'ctx>>, Value<'ctx>), CompileError> {
        let position = expression.position;

        match &expression.kind {
            // TODO this should not be named FunctionCall, but just Call
            types::ExpressionKind::FunctionCall { target, arguments } => {
                let compiled_target = self.compile_expression(
                    target,
                    self_.clone(),
                    compiled_function,
                    module_path.clone(),
                    module,
                )?;
                let (self_value, Value::Function(function)) = compiled_target else {
                    todo!();
                };

                let mut compiled_arguments_iter = arguments
                    .iter()
                    .map(|a| {
                        self.compile_expression(
                            a,
                            self_value.clone(),
                            compiled_function,
                            module_path.clone(),
                            module,
                        )
                        .map(|x| x.1)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let call_arguments = compiled_arguments_iter
                    .iter_mut()
                    .map(|a| match a {
                        Value::Primitive(_, v) => v.as_basic_value_enum().into(),
                        Value::Reference(rc_value) => {
                            rc_value.as_ptr().as_basic_value_enum().into()
                        }
                        Value::Function(_) => todo!("implement passing callables as arguments"),
                        Value::Struct(_) => {
                            todo!("implement passing struct definitions as arguments")
                        }
                    })
                    .collect::<Vec<BasicMetadataValueEnum>>();

                // TODO support functions that return things that aren't ints!
                let call_result = self
                    .context
                    .builder
                    .build_call(
                        function.get_or_create_in_module(module, &self.context),
                        &call_arguments,
                        &format!(
                            "call_result_{}_{}_{}_{}",
                            position.0.0, position.0.1, position.1.0, position.1.1
                        ),
                    )
                    .map_err(|e| e.into_compile_error_at(module_path.clone(), position))?;

                let call_result = call_result.as_any_value_enum().try_into().unwrap_or(
                    self.context
                        .llvm_context
                        .i64_type()
                        .const_zero()
                        .as_basic_value_enum(),
                );

                // TODO the call result may not be void
                Ok((
                    None,
                    Value::Primitive(
                        compiled_function
                            .scope
                            .get_variable(&Identifier::parse("u64"))
                            .unwrap()
                            .as_struct()
                            .unwrap(),
                        call_result,
                    ),
                ))
            }
            types::ExpressionKind::Literal(literal) => match literal {
                types::Literal::String(s) => {
                    let name = format!(
                        "literal_{}_{}_{}_{}",
                        position.0.0, position.0.1, position.1.0, position.1.1
                    );
                    let characters_value = self
                        .context
                        .builder
                        .build_global_string_ptr(s, &(name.clone() + "_global"))
                        .unwrap();

                    let literal_value = self
                        .context
                        .builder
                        .build_malloc(
                            self.context.builtins.string_handle.llvm_type,
                            &(name.clone() + "_value"),
                        )
                        .unwrap();
                    let literal_value_characters = unsafe {
                        self.context
                            .builder
                            .build_gep(
                                self.context.builtins.string_handle.llvm_type,
                                literal_value,
                                &[self.context.llvm_context.i64_type().const_int(0, false)],
                                &(name.clone() + "_value_characters"),
                            )
                            .unwrap()
                    };
                    self.context
                        .builder
                        .build_store(literal_value_characters, characters_value)
                        .unwrap();

                    let rc = RcValue::build_init(
                        mangle_item(module_path.clone(), Identifier::parse(&name)),
                        literal_value,
                        self.context.builtins.string_handle.clone(),
                        &self.context,
                    )?;
                    compiled_function.rcs.push(rc.clone());

                    Ok((None, Value::Reference(rc)))
                }
                types::Literal::UnsignedInteger(value) => Ok((
                    None,
                    Value::Primitive(
                        // TODO should this be in builtins or something?
                        compiled_function
                            .scope
                            .get_variable(&Identifier::parse("u64"))
                            .unwrap()
                            .as_struct()
                            .unwrap(),
                        self.context
                            .llvm_context
                            .i64_type()
                            .const_int(*value, false)
                            .as_basic_value_enum(),
                    ),
                )),
            },
            types::ExpressionKind::VariableAccess(name) => {
                Ok((None, compiled_function.scope.get_variable(name).unwrap()))
            }
            types::ExpressionKind::StructConstructor(name) => {
                let s = compiled_function
                    .scope
                    .get_variable(name)
                    .unwrap()
                    .as_struct()
                    .unwrap();

                let value = self
                    .context
                    .builder
                    .build_malloc(
                        s.llvm_type,
                        &format!(
                            "{}_{}_{}_{}_{}",
                            name, position.0.0, position.0.1, position.1.0, position.1.1
                        ),
                    )
                    .unwrap();

                let rc = RcValue::build_init(
                    mangle_item(module_path.clone(), name.clone()),
                    value,
                    s.clone(),
                    &self.context,
                )?;
                compiled_function.rcs.push(rc.clone());

                Ok((None, Value::Reference(rc)))
            }
            types::ExpressionKind::FieldAccess {
                target,
                field: field_name,
            } => {
                let (_, target_value) = self.compile_expression(
                    target,
                    self_,
                    compiled_function,
                    module_path.clone(),
                    module,
                )?;

                let access_result = match &target_value {
                    Value::Primitive(handle, _) => {
                        // TODO this is duplicated between primitive and reference, the access
                        // should probably be handled by hnandle.description
                        if handle
                            .description
                            .fields
                            .iter()
                            .filter(|f| !f.static_)
                            .enumerate()
                            .any(|x| &x.1.name == field_name)
                        {
                            todo!()
                        };

                        handle.static_fields.get(field_name).unwrap().clone()
                    }
                    Value::Reference(ref_) => {
                        if ref_
                            .type_()
                            .description
                            .fields
                            .iter()
                            .filter(|f| !f.static_)
                            .enumerate()
                            .any(|x| &x.1.name == field_name)
                        {
                            todo!()
                        };

                        ref_.type_().static_fields.get(field_name).unwrap().clone()
                    }
                    Value::Function(_) => todo!(),
                    Value::Struct(_) => todo!(),
                };

                Ok((Some(target_value), access_result))
            }
            types::ExpressionKind::SelfAccess => Ok((None, self_.unwrap())),
        }
    }
}
