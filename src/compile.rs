use std::{collections::HashMap, error::Error, fmt::Display};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    types::BasicType,
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, FunctionValue},
    AddressSpace,
};

use crate::{
    ast::{Expression, Function, FunctionBody, SourceRange, Statement},
    stdlib::register_mappings,
    type_check::Program,
};

struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    declared_modules: HashMap<String, Module<'ctx>>,
}

pub fn compile(program: &Program) -> Result<(), CompileError> {
    let context = Context::create();
    let compiler = Compiler::new(&context);

    compiler.compile(program)
}

#[derive(Debug)]
pub enum ErrorLocation {
    // TODO this should also have the module name
    Position(SourceRange),
    Module(String),
}

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLocation::Position(source_range) => write!(f, "{source_range}"),
            ErrorLocation::Module(name) => write!(f, "module {name}"),
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
    ModuleNotFound(String),
    FunctionNotFound {
        module_name: String,
        function_name: String,
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

    fn in_module(self, name: String) -> CompileError {
        CompileError {
            description: self,
            location: ErrorLocation::Module(name),
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

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let declared_modules = HashMap::new();

        Self {
            context,
            builder: context.create_builder(),
            declared_modules,
        }
    }

    fn declare_function_inner(&self, function: &Function, module: &Module<'ctx>, linkage: Linkage) {
        let arguments = function
            .arguments
            .iter()
            .map(|arg| self.type_to_llvm(&arg.type_).as_basic_type_enum().into())
            .collect::<Vec<_>>();

        let function_type = self.context.void_type().fn_type(&arguments[..], false);

        module.add_function(&function.name, function_type, Some(linkage));
    }

    fn declare_function(&self, function: &Function, module: &Module<'ctx>) {
        let linkage = if function.export || function.name == "main" {
            Linkage::External
        } else {
            Linkage::Internal
        };

        self.declare_function_inner(function, module, linkage);
    }

    fn import_function(&self, function: &Function, module: &Module<'ctx>) {
        self.declare_function_inner(function, module, Linkage::External);
    }

    // TODO instead of executing the code, this should return some object that exposes the
    // executable code with a safe interface
    pub fn compile(mut self, program: &Program) -> Result<(), CompileError> {
        let mut function_declarations: HashMap<String, HashMap<String, Function>> = HashMap::new();

        for file in &program.0 {
            let module = self.context.create_module(&file.name);
            let mut function_declarations_for_module = HashMap::new();

            for declaration in &file.declarations {
                match declaration {
                    crate::ast::Declaration::Function(function, _) => {
                        function_declarations_for_module
                            .insert(function.name.clone(), function.clone());

                        self.declare_function(function, &module);
                    }
                };
            }

            self.declared_modules.insert(file.name.clone(), module);
            function_declarations.insert(file.name.clone(), function_declarations_for_module);
        }

        for file in &program.0 {
            let Some(module) = self.declared_modules.get(&file.name) else {
                return Err(
                    CompileErrorDescription::ModuleNotFound(file.name.clone()).at(file.position)
                );
            };

            for import in &file.imports {
                // TODO support multi-level module hierarchies
                let (import_module, import_function) =
                    (import.path[0].clone(), import.path[1].clone());

                self.import_function(
                    function_declarations
                        .get(&import_module)
                        .ok_or_else(|| {
                            CompileErrorDescription::ModuleNotFound(import_module.clone())
                                .at(import.position)
                        })?
                        .get(&import_function)
                        .ok_or_else(|| {
                            CompileErrorDescription::FunctionNotFound {
                                module_name: import_module,
                                function_name: import_function,
                            }
                            .at(import.position)
                        })?,
                    module,
                );
            }

            for declaration in &file.declarations {
                match declaration {
                    crate::ast::Declaration::Function(function, position) => {
                        let FunctionBody::Statements(statements, _) = &function.body else {
                            continue;
                        };

                        let Some(function) = module.get_function(&function.name) else {
                            return Err(CompileErrorDescription::FunctionNotFound {
                                module_name: file.name.clone(),
                                function_name: function.name.clone(),
                            }
                            .at(function.position));
                        };
                        let function_block = self.context.append_basic_block(function, "entry");

                        self.builder.position_at_end(function_block);

                        for statement in statements {
                            match statement {
                                Statement::Expression(expression, _) => {
                                    self.compile_expression(expression, module)?;
                                }
                            }
                        }

                        self.builder
                            .build_return(None)
                            .map_err(|e| e.into_compile_error_at(*position))?;
                    }
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

    fn compile_expression(
        &self,
        expression: &Expression,
        module: &Module<'ctx>,
    ) -> Result<BasicMetadataValueEnum, CompileError> {
        match expression {
            crate::ast::Expression::FunctionCall {
                name,
                arguments,
                position,
            } => {
                let function = self.resolve_function(name, module).ok_or_else(|| {
                    CompileErrorDescription::FunctionNotFound {
                        module_name: module.get_name().to_string_lossy().to_string(),
                        function_name: name.clone(),
                    }
                    .at(*position)
                })?;

                let call_arguments = arguments
                    .iter()
                    .map(|a| self.compile_expression(a, module))
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
            crate::ast::Expression::Literal(literal, _) => {
                match literal {
                    crate::ast::Literal::String(s, _) => {
                        let string_bytes = s.as_bytes().to_vec();

                        let string_type = self
                            .context
                            .i8_type()
                            .array_type(string_bytes.len() as u32 + 1);

                        // FIXME use a globally unique name for the global
                        let global = module.add_global(string_type, None, "str0");
                        global.set_initializer(&self.context.const_string(&string_bytes[..], true));
                        global.set_constant(true);
                        global.set_visibility(inkwell::GlobalVisibility::Hidden);

                        Ok(global.as_pointer_value().as_basic_value_enum().into())
                    }
                }
            }
        }
    }

    fn type_to_llvm(&self, type_: &crate::ast::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match type_ {
            crate::ast::Type::Void => panic!("Cannot pass void arguments!"),
            // todo arrays should be structs that have bounds, not just ptrs
            crate::ast::Type::Array(_) => Box::new(self.context.ptr_type(AddressSpace::default())),
            crate::ast::Type::Named(n) => match n.as_str() {
                // todo string should be some form of a struct
                "string" => Box::new(self.context.ptr_type(AddressSpace::default())),
                _ => todo!(),
            },
        }
    }
}
