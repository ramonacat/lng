mod context;
mod module;
mod rc_builder;
mod scope;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use context::{Builtins, CompilerContext};
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, StructType},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, FunctionValue},
    AddressSpace,
};
use module::GlobalScope;
use rc_builder::RcValue;
use scope::Scope;

use crate::{
    ast::{Expression, FunctionBody, SourceRange, Statement},
    runtime::register_mappings,
    types::{self, Identifier, ModulePath},
};

pub fn compile(program: &types::Program) -> Result<(), CompileError> {
    let context = Context::create();

    let compiler = Compiler::new(&context);

    compiler.compile(program)
}

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
    handle: FunctionHandle<'ctx>,

    entry: BasicBlock<'ctx>,
    end: BasicBlock<'ctx>,
    scope: Rc<Scope<'ctx>>,
    rcs: Vec<RcValue<'ctx>>,
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
}

#[derive(Clone)]
pub struct FunctionHandle<'ctx> {
    pub self_: Option<Identifier>,
    pub location: SourceRange,
    pub arguments: Vec<types::Argument>,
    pub llvm_function: FunctionValue<'ctx>,
}

#[derive(Clone)]
#[allow(unused)]
pub struct StructHandle<'ctx> {
    description: types::Struct,
    static_fields: HashMap<types::Identifier, Value<'ctx>>,
    llvm_type: StructType<'ctx>,
}

#[derive(Clone)]
enum Value<'ctx> {
    Primitive(BasicMetadataValueEnum<'ctx>),
    // TODO the StructHandle should probably be inside RcValue
    Reference(StructHandle<'ctx>, RcValue<'ctx>),
    Function(FunctionHandle<'ctx>),
    Struct(StructHandle<'ctx>),
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
                    name: types::Identifier("string".to_string()),
                    fields: vec![],
                    impls: HashMap::new(),
                },
                llvm_type: string_type,
                static_fields: HashMap::new(),
            },
            rc_handle: StructHandle {
                description: types::Struct {
                    name: types::Identifier("rc".to_string()),
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

    // TODO implement name mangling to avoid collisions between functions from different modules!!!
    fn declare_function_inner(
        &self,
        name: Identifier,
        arguments: &[types::Argument],
        llvm_module: &Module<'ctx>,
        linkage: Linkage,
    ) -> FunctionValue<'ctx> {
        let arguments = arguments
            .iter()
            .map(|arg| self.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        let function_type = self
            .context
            .llvm_context
            .void_type()
            .fn_type(&arguments[..], false);

        // TODO name mangling!!!
        llvm_module.add_function(&name.0, function_type, Some(linkage))
    }

    fn declare_function(
        &self,
        function: &types::Function,
        module: &Module<'ctx>,
    ) -> FunctionValue<'ctx> {
        let linkage = if function.export
            || function.name == types::Identifier("main".to_string())
            || matches!(function.body, FunctionBody::Extern(_))
        {
            Linkage::External
        } else {
            Linkage::Internal
        };

        self.declare_function_inner(function.name.clone(), &function.arguments, module, linkage)
    }

    fn import_function(
        &self,
        function: &FunctionHandle<'ctx>,
        module: &Module<'ctx>,
        // TODO support importing as an alias
        name: Identifier,
    ) -> FunctionValue<'ctx> {
        self.declare_function_inner(name, &function.arguments, module, Linkage::External)
    }

    // TODO instead of executing the code, this should return some object that exposes the
    // executable code with a safe interface
    pub fn compile(self, program: &'src types::Program) -> Result<(), CompileError> {
        let mut global_scope = GlobalScope::new();

        for (path, program_module) in &program.modules {
            let module = self.context.llvm_context.create_module(path.as_str());
            let created_module = global_scope.create_module(path.clone(), module);

            for declaration in program_module.items.values() {
                match declaration {
                    types::Item::Function(function) => {
                        let llvm_function =
                            self.declare_function(function, &created_module.llvm_module);

                        let function_handle = FunctionHandle {
                            self_: None,
                            arguments: function.arguments.clone(),
                            llvm_function,
                            location: function.location,
                        };

                        created_module
                            .set_variable(function.name.clone(), Value::Function(function_handle));
                    }
                    types::Item::Struct(struct_) => {
                        let field_types = struct_
                            .fields
                            .iter()
                            .map(|f| self.type_to_llvm(&f.type_).as_basic_type_enum())
                            .collect::<Vec<_>>();
                        let llvm_type = self
                            .context
                            .llvm_context
                            .struct_type(&field_types[..], false);

                        let static_fields = struct_
                            .impls
                            .iter()
                            .map(|(name, impl_)| {
                                let llvm_function =
                                    self.declare_function(impl_, &created_module.llvm_module);
                                let handle = FunctionHandle {
                                    self_: Some(struct_.name.clone()),
                                    arguments: impl_.arguments.clone(),
                                    llvm_function,
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
                    types::Item::ImportFunction(_) => {}
                };
            }
        }

        for (module_path, file) in &program.modules {
            let Some(module) = global_scope.get(module_path) else {
                return Err(
                    CompileErrorDescription::ModuleNotFound(module_path.clone()).at_indeterminate()
                );
            };

            for item in file.items.values() {
                // TODO support importing structs, and possibly other types
                let types::Item::ImportFunction(import) = item else {
                    continue;
                };

                let function =
                    global_scope.resolve_function(&import.path, &import.item, import.location)?;

                let imported_function =
                    self.import_function(&function, &module.llvm_module, import.item.clone());

                module.set_variable(
                    import.item.clone(),
                    Value::Function(FunctionHandle {
                        self_: None,
                        location: import.location,
                        arguments: function.arguments.clone(),
                        llvm_function: imported_function,
                    }),
                );
            }

            for item in file.items.values() {
                match item {
                    types::Item::Function(function) => {
                        self.compile_function(module_path, module, function, None)?;
                    }
                    types::Item::ImportFunction(_) => {}
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

        register_mappings(&execution_engine, &root_module);

        unsafe {
            let main = execution_engine
                .get_function::<unsafe extern "C" fn()>("main")
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
        let FunctionBody::Statements(statements, _) = &function.body else {
            return Ok(());
        };
        let mut compiled_function =
            module.begin_compile_function(function, &self.context, struct_)?;
        for statement in statements {
            match statement {
                // TODO get rid of the ast types, and make typecheck create its own
                Statement::Expression(expression, _) => {
                    self.compile_expression(
                        expression,
                        &mut compiled_function,
                        module_path.clone(),
                    )?;
                }
                Statement::Let(name, _, value) => {
                    let value = self.compile_expression(
                        value,
                        &mut compiled_function,
                        module_path.clone(),
                    )?;

                    compiled_function
                        .scope
                        .register(Identifier(name.to_string()), value.1);
                }
            }
        }
        let cleanup_label = rc_builder::build_cleanup(
            &self.context,
            &compiled_function.rcs,
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
        compiled_function.build_return(None, &self.context, module_path.clone())?;
        Ok(())
    }

    fn compile_expression(
        &self,
        expression: &Expression,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: ModulePath,
    ) -> Result<(Option<Value<'ctx>>, Value<'ctx>), CompileError> {
        match expression {
            crate::ast::Expression::FunctionCall {
                target,
                arguments,
                position,
            } => {
                let compiled_target =
                    self.compile_expression(target, compiled_function, module_path.clone())?;
                let (self_value, Value::Function(function)) = compiled_target else {
                    // TODO actual error check!
                    panic!();
                };

                let mut compiled_arguments_iter = arguments
                    .iter()
                    .map(|a| {
                        self.compile_expression(a, compiled_function, module_path.clone())
                            .map(|x| x.1)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if function.self_.is_some() {
                    if let Some(first) = function.arguments.first() {
                        if first.name.0 == "self" {
                            let mut new_compiled_arguments = vec![self_value.unwrap()];
                            new_compiled_arguments.append(&mut compiled_arguments_iter);
                            compiled_arguments_iter = new_compiled_arguments;
                        }
                    }
                }

                let call_arguments = compiled_arguments_iter
                    .iter_mut()
                    .map(|a| match a {
                        Value::Primitive(v) => *v,
                        Value::Reference(_, rc_value) => {
                            rc_value.as_ptr().as_basic_value_enum().into()
                        }
                        Value::Function(_) => todo!("implement passing callables as arguments"),
                        Value::Struct(_) => {
                            todo!("implement passing struct definitions as arguments")
                        }
                    })
                    .collect::<Vec<_>>();

                // todo functions can return a Reference too, we need to consider that and return
                // the right expression result!
                let call_result = self
                    .context
                    .builder
                    .build_call(
                        function.llvm_function,
                        &call_arguments,
                        &format!(
                            "call_result_{}_{}_{}_{}",
                            position.0 .0, position.0 .1, position.1 .0, position.1 .1
                        ),
                    )
                    .map_err(|e| e.into_compile_error_at(module_path.clone(), *position))?;

                let call_result = call_result.as_any_value_enum().try_into().unwrap_or(
                    self.context
                        .llvm_context
                        .i8_type()
                        .const_zero()
                        .as_basic_value_enum()
                        .into(),
                );

                Ok((None, Value::Primitive(call_result)))
            }
            crate::ast::Expression::Literal(literal, _) => match literal {
                crate::ast::Literal::String(s, location) => {
                    let name = format!(
                        "literal_{}_{}_{}_{}",
                        location.0 .0, location.0 .1, location.1 .0, location.1 .1
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

                    let rc = RcValue::build_init(&name, literal_value, &self.context)?;
                    compiled_function.rcs.push(rc);

                    Ok((
                        None,
                        Value::Reference(self.context.builtins.string_handle.clone(), rc),
                    ))
                }
            },
            Expression::VariableReference(name, _) => Ok((
                None,
                compiled_function
                    .scope
                    .get_variable(&Identifier(name.clone()))
                    .unwrap(),
            )),
            Expression::StructConstructor(name, location) => {
                let s = compiled_function
                    .scope
                    .get_struct(&Identifier(name.clone()), module_path, *location)
                    .unwrap();

                let value = self
                    .context
                    .builder
                    .build_malloc(
                        s.llvm_type,
                        &format!(
                            "{}_{}_{}_{}_{}",
                            name, location.0 .0, location.0 .1, location.1 .0, location.1 .1
                        ),
                    )
                    .unwrap();

                let rc = RcValue::build_init(name, value, &self.context)?;
                compiled_function.rcs.push(rc);

                Ok((None, Value::Reference(s, rc)))
            }
            Expression::FieldAccess {
                target,
                field_name,
                position: _,
            } => {
                let value =
                    self.compile_expression(target, compiled_function, module_path.clone())?;

                let (_, Value::Reference(ref reference_type, _)) = value else {
                    todo!();
                };

                if reference_type
                    .description
                    .fields
                    .iter()
                    .filter(|f| !f.static_)
                    .enumerate()
                    .any(|x| x.1.name == Identifier(field_name.clone()))
                {
                    todo!()
                };

                Ok((
                    Some(value.1.clone()),
                    reference_type
                        .static_fields
                        .get(&Identifier(field_name.clone()))
                        .unwrap()
                        .clone(),
                ))
            }
        }
    }

    fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            types::Type::Void => panic!("Cannot pass void arguments!"),
            // TODO arrays should be structs that have bounds, not just ptrs
            types::Type::Array(_) => {
                Box::new(self.context.llvm_context.ptr_type(AddressSpace::default()))
            }
            types::Type::Object(_) => {
                Box::new(self.context.llvm_context.ptr_type(AddressSpace::default()))
            }
            types::Type::StructDescriptor(_, _) => todo!(),
            types::Type::Callable { .. } => {
                Box::new(self.context.llvm_context.ptr_type(AddressSpace::default()))
            }
        }
    }
}
