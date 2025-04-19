mod rc_builder;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, StructType},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, FunctionValue},
    AddressSpace,
};
use rc_builder::build_rc;

use crate::{
    ast::{Expression, FunctionBody, SourceRange, Statement},
    runtime::register_mappings,
    types::{self, Identifier, ModulePath},
};

struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    declared_modules: HashMap<types::ModulePath, Module<'ctx>>,
}

struct Builtins<'ctx> {
    rc_type: StructType<'ctx>,
    string_type: StructType<'ctx>,
}

struct Scope<'ctx> {
    builtins: Rc<Builtins<'ctx>>,
}

pub fn compile(program: &types::Program) -> Result<(), CompileError> {
    let context = Context::create();
    let compiler = Compiler::new(&context);

    compiler.compile(program)
}

#[derive(Debug)]
pub enum ErrorLocation {
    // TODO this should also have the module name
    Position(SourceRange),
    Module(ModulePath),
    Indeterminate,
}

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLocation::Position(source_range) => write!(f, "{source_range}"),
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
}

impl CompileErrorDescription {
    fn at(self, position: SourceRange) -> CompileError {
        CompileError {
            description: self,
            location: ErrorLocation::Position(position),
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
    fn into_compile_error_at(self, position: SourceRange) -> CompileError;
}

impl IntoCompileError for BuilderError {
    fn into_compile_error_at(self, position: SourceRange) -> CompileError {
        CompileError {
            description: self.into(),
            location: ErrorLocation::Position(position),
        }
    }
}

struct CompiledFunction<'ctx, 'src> {
    context: &'src Context,
    definition: &'src types::Function,
    builder: &'src Builder<'ctx>,
    llvm_function: FunctionValue<'ctx>,
    exits: Vec<Box<dyn Fn(&'src Builder<'ctx>, &'src Context) + 'src>>,
}

impl<'ctx, 'src> CompiledFunction<'ctx, 'src> {
    fn build_return(
        &self,
        return_value: Option<&dyn BasicValue<'ctx>>,
    ) -> Result<(), CompileError> {
        for exit in &self.exits {
            exit(self.builder, self.context);
        }

        self.builder
            .build_return(return_value)
            .map_err(|e| e.into_compile_error_at(self.definition.location))?;

        Ok(())
    }

    fn register_exit(&mut self, exit_function: impl Fn(&Builder<'ctx>, &Context) + 'src) {
        self.exits.push(Box::new(exit_function));
    }
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let declared_modules = HashMap::new();

        Self {
            context,
            builder: context.create_builder(),
            declared_modules,
        }
    }

    fn declare_function_inner(
        &self,
        function: &types::Function,
        llvm_module: &Module<'ctx>,
        linkage: Linkage,
    ) {
        let arguments = function
            .arguments
            .iter()
            .map(|arg| self.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        let function_type = self.context.void_type().fn_type(&arguments[..], false);

        llvm_module.add_function(&function.name.0, function_type, Some(linkage));
    }

    fn declare_function(&self, function: &types::Function, module: &Module<'ctx>) {
        let linkage = if function.export || function.name == types::Identifier("main".to_string()) {
            Linkage::External
        } else {
            Linkage::Internal
        };

        self.declare_function_inner(function, module, linkage);
    }

    fn import_function(&self, function: &types::Function, module: &Module<'ctx>) {
        self.declare_function_inner(function, module, Linkage::External);
    }

    // TODO instead of executing the code, this should return some object that exposes the
    // executable code with a safe interface
    pub fn compile(mut self, program: &types::Program) -> Result<(), CompileError> {
        let mut function_declarations: HashMap<
            types::ModulePath,
            HashMap<types::Identifier, types::Function>,
        > = HashMap::new();
        let mut struct_declarations: HashMap<
            types::ModulePath,
            HashMap<types::Identifier, (types::Struct, StructType<'ctx>)>,
        > = HashMap::new();

        for (path, program_module) in &program.modules {
            let module = self.context.create_module(path.as_str());

            let mut function_declarations_for_module = HashMap::new();
            let mut struct_declarations_for_module: HashMap<
                Identifier,
                (types::Struct, StructType<'ctx>),
            > = HashMap::new();

            for declaration in program_module.items.values() {
                match declaration {
                    types::Item::Function(function) => {
                        function_declarations_for_module
                            .insert(function.name.clone(), function.clone());

                        self.declare_function(function, &module);
                    }
                    types::Item::Struct(struct_) => {
                        let field_types = struct_
                            .fields
                            .iter()
                            .map(|f| self.type_to_llvm(&f.type_).as_basic_type_enum())
                            .collect::<Vec<_>>();
                        let llvm_type = self.context.struct_type(&field_types[..], false);

                        struct_declarations_for_module
                            .insert(struct_.name.clone(), (struct_.clone(), llvm_type));
                    }
                    types::Item::ImportFunction(_) => {}
                };
            }

            self.declared_modules.insert(path.clone(), module);
            function_declarations.insert(path.clone(), function_declarations_for_module);
            struct_declarations.insert(path.clone(), struct_declarations_for_module);
        }

        let std_module_path = types::ModulePath(types::Identifier("std".to_string()));

        let Some(std_struct_declarations) = struct_declarations.get(&std_module_path) else {
            return Err(CompileErrorDescription::ModuleNotFound(std_module_path).at_indeterminate());
        };

        for (module_path, file) in &program.modules {
            let Some(module) = self.declared_modules.get(module_path) else {
                return Err(
                    CompileErrorDescription::ModuleNotFound(module_path.clone()).at_indeterminate()
                );
            };

            for item in file.items.values() {
                let types::Item::ImportFunction(import) = item else {
                    continue;
                };

                self.import_function(
                    function_declarations
                        .get(&import.path)
                        .ok_or_else(|| {
                            CompileErrorDescription::ModuleNotFound(import.path.clone())
                                .at(import.location)
                        })?
                        .get(&import.item)
                        .ok_or_else(|| {
                            CompileErrorDescription::FunctionNotFound {
                                module_name: import.path.clone(),
                                function_name: import.item.clone(),
                            }
                            .at(import.location)
                        })?,
                    module,
                );
            }

            let rc_type = self.context.struct_type(
                &[
                    self.context.i64_type().as_basic_type_enum(), // refcount
                    self.context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(), // target object
                ],
                false,
            );

            let global_scope = Scope {
                builtins: Rc::new(Builtins {
                    rc_type,
                    string_type: std_struct_declarations
                        .get(&Identifier("string".to_string()))
                        .unwrap()
                        .1,
                }),
            };

            for item in file.items.values() {
                match item {
                    types::Item::Function(function) => {
                        let FunctionBody::Statements(statements, _) = &function.body else {
                            continue;
                        };

                        let Some(llvm_function) = module.get_function(&function.name.0) else {
                            return Err(CompileErrorDescription::FunctionNotFound {
                                module_name: module_path.clone(),
                                function_name: function.name.clone(),
                            }
                            .at(function.location));
                        };

                        let function_block =
                            self.context.append_basic_block(llvm_function, "entry");

                        let mut compiled_function = CompiledFunction {
                            context: self.context,
                            builder: &self.builder,
                            definition: function,
                            llvm_function,
                            exits: vec![],
                        };

                        self.builder.position_at_end(function_block);

                        for statement in statements {
                            match statement {
                                Statement::Expression(expression, _) => {
                                    // TODO each function should have its own scope?
                                    self.compile_expression(
                                        expression,
                                        module,
                                        &global_scope,
                                        &mut compiled_function,
                                    )?;
                                }
                            }
                        }

                        compiled_function.build_return(None)?;
                    }
                    types::Item::ImportFunction(_) => {}
                    types::Item::Struct(_) => {}
                }
            }
        }

        let root_module = self.context.create_module("root");
        for module in self.declared_modules {
            root_module.link_in_module(module.1).map_err(|e| {
                CompileErrorDescription::InternalError(e.to_string()).in_module(module.0)
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

    fn resolve_function(&self, name: &str, module: &Module<'ctx>) -> Option<FunctionValue<'ctx>> {
        module.get_function(name)
    }

    fn compile_expression<'src>(
        &self,
        expression: &Expression,
        module: &Module<'ctx>,
        global_scope: &Scope<'ctx>,
        compiled_function: &mut CompiledFunction<'ctx, 'src>,
    ) -> Result<BasicMetadataValueEnum, CompileError> {
        match expression {
            crate::ast::Expression::FunctionCall {
                name,
                arguments,
                position,
            } => {
                let function = self.resolve_function(name, module).ok_or_else(|| {
                    CompileErrorDescription::FunctionNotFound {
                        module_name: types::ModulePath(types::Identifier(
                            module.get_name().to_string_lossy().to_string(),
                        )),
                        function_name: types::Identifier(name.clone()),
                    }
                    .at(*position)
                })?;

                let call_arguments = arguments
                    .iter()
                    .map(|a| self.compile_expression(a, module, global_scope, compiled_function))
                    .collect::<Result<Vec<_>, _>>()?;

                let call_result = self
                    .builder
                    .build_call(function, &call_arguments, name)
                    .map_err(|e| e.into_compile_error_at(*position))?;

                Ok(call_result.as_any_value_enum().try_into().unwrap_or(
                    self.context
                        .i8_type()
                        .const_zero()
                        .as_basic_value_enum()
                        .into(),
                ))
            }
            crate::ast::Expression::Literal(literal, _) => match literal {
                crate::ast::Literal::String(s, _) => {
                    let characters_value = self
                        .builder
                        .build_global_string_ptr(s, "literal0_global")
                        .unwrap();

                    let literal_value = self
                        .builder
                        .build_malloc(global_scope.builtins.string_type, "literal0_value")
                        .unwrap();
                    let literal_value_characters = unsafe {
                        self.builder
                            .build_gep(
                                global_scope.builtins.string_type,
                                literal_value,
                                &[self.context.i64_type().const_int(0, false)],
                                "literal0_value_characters",
                            )
                            .unwrap()
                    };
                    self.builder
                        .build_store(literal_value_characters, characters_value)
                        .unwrap();

                    build_rc(
                        "literal0",
                        literal_value,
                        global_scope,
                        &self.builder,
                        self.context,
                        compiled_function,
                    )
                }
            },
        }
    }

    fn type_to_llvm(&self, type_: &types::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            types::Type::Void => panic!("Cannot pass void arguments!"),
            // TODO arrays should be structs that have bounds, not just ptrs
            types::Type::Array(_) => Box::new(self.context.ptr_type(AddressSpace::default())),
            types::Type::Object(_) => Box::new(self.context.ptr_type(AddressSpace::default())),
        }
    }
}
