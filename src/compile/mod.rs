mod context;
mod rc_builder;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc, sync::RwLock};

use context::{Builtins, CompilerContext};
use inkwell::{
    builder::BuilderError,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, StructType},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace,
};
use rc_builder::RcValue;

use crate::{
    ast::{Expression, FunctionBody, SourceRange, Statement},
    runtime::register_mappings,
    types::{self, Identifier, ModulePath},
};

struct Compiler<'ctx> {
    context: CompilerContext<'ctx>,
    declared_modules: HashMap<types::ModulePath, Module<'ctx>>,
}

struct Scope<'ctx> {
    locals: RwLock<HashMap<Identifier, BasicValueEnum<'ctx>>>,
    parent: Option<Rc<Scope<'ctx>>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn root() -> Rc<Self> {
        Rc::new(Self {
            locals: RwLock::new(HashMap::new()),
            parent: None,
        })
    }

    pub fn child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            locals: RwLock::new(HashMap::new()),
            parent: Some(self.clone()),
        })
    }

    pub fn register(&self, name: Identifier, value: BasicValueEnum<'ctx>) {
        self.locals.write().unwrap().insert(name, value);
    }

    pub fn get_variable(&self, name: &Identifier) -> Option<BasicValueEnum<'ctx>> {
        if let Some(variable) = self.locals.read().unwrap().get(name) {
            return Some(*variable);
        }

        if let Some(parent) = &self.parent {
            return parent.get_variable(name);
        }

        None
    }
}

pub fn compile(program: &types::Program) -> Result<(), CompileError> {
    let context = Context::create();

    let compiler = Compiler::new(&context);

    compiler.compile(program)
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

type OnFunctionExit<'ctx> = dyn Fn(&CompilerContext<'ctx>) + 'ctx;

struct CompiledFunction<'ctx, 'src> {
    module: &'src types::Module,
    definition: &'src types::Function,

    exits: Vec<Box<OnFunctionExit<'ctx>>>,
    scope: Rc<Scope<'ctx>>,

    llvm_function: FunctionValue<'ctx>,
}

impl<'ctx, 'src> CompiledFunction<'ctx, 'src>
where
    'src: 'ctx,
{
    fn build_return(
        &self,
        return_value: Option<&dyn BasicValue<'ctx>>,
        context: &CompilerContext<'ctx>,
    ) -> Result<(), CompileError> {
        for exit in &self.exits {
            exit(context);
        }

        context.builder.build_return(return_value).map_err(|e| {
            e.into_compile_error_at(self.module.path.clone(), self.definition.location)
        })?;

        Ok(())
    }

    fn register_exit(&mut self, exit_function: impl Fn(&CompilerContext<'ctx>) + 'ctx) {
        self.exits.push(Box::new(exit_function));
    }
}

#[derive(Clone, Copy)]
enum ValueType<'ctx> {
    Value(BasicMetadataValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
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
            rc_type,
            string_type,
        };

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                builtins,
            },
            declared_modules: HashMap::new(),
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

        let function_type = self
            .context
            .llvm_context
            .void_type()
            .fn_type(&arguments[..], false);

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
    pub fn compile(mut self, program: &'src types::Program) -> Result<(), CompileError> {
        let mut function_declarations: HashMap<
            types::ModulePath,
            HashMap<types::Identifier, types::Function>,
        > = HashMap::new();

        let mut struct_declarations: HashMap<
            types::ModulePath,
            HashMap<types::Identifier, (types::Struct, StructType<'ctx>)>,
        > = HashMap::new();

        for (path, program_module) in &program.modules {
            let module = self.context.llvm_context.create_module(path.as_str());

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
                        let llvm_type = self
                            .context
                            .llvm_context
                            .struct_type(&field_types[..], false);

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

        let global_scope = Scope::root();

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
                                .at(module_path.clone(), import.location)
                        })?
                        .get(&import.item)
                        .ok_or_else(|| {
                            CompileErrorDescription::FunctionNotFound {
                                module_name: import.path.clone(),
                                function_name: import.item.clone(),
                            }
                            .at(module_path.clone(), import.location)
                        })?,
                    module,
                );
            }

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
                            .at(module_path.clone(), function.location));
                        };

                        let scope = global_scope.child();

                        for (argument, argument_value) in
                            function.arguments.iter().zip(llvm_function.get_params())
                        {
                            scope.register(argument.name.clone(), argument_value);
                        }

                        let function_block = self
                            .context
                            .llvm_context
                            .append_basic_block(llvm_function, "entry");

                        let mut compiled_function = CompiledFunction {
                            module: file,
                            definition: function,
                            llvm_function,
                            exits: vec![],
                            scope,
                        };

                        self.context.builder.position_at_end(function_block);

                        for statement in statements {
                            match statement {
                                Statement::Expression(expression, _) => {
                                    self.compile_expression(
                                        expression,
                                        &mut compiled_function,
                                        &self.context.builtins,
                                        module,
                                    )?;
                                }
                            }
                        }

                        compiled_function.build_return(None, &self.context)?;
                    }
                    types::Item::ImportFunction(_) => {}
                    types::Item::Struct(_) => {}
                }
            }
        }

        let root_module = self.context.llvm_context.create_module("root");
        for module in self.declared_modules {
            println!("{}", module.0);

            println!(
                "{}",
                module.1.print_to_string().to_string().replace("\\n", "\n")
            );

            module.1.verify().unwrap();

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

    fn compile_expression(
        &self,
        expression: &Expression,
        compiled_function: &mut CompiledFunction<'ctx, 'src>,
        builtins: &Builtins<'ctx>,
        module: &Module<'ctx>, // TODO this should be Scope instead, and functions should be values
                               // therein
    ) -> Result<ValueType<'ctx>, CompileError> {
        match expression {
            crate::ast::Expression::FunctionCall {
                name,
                arguments,
                position,
            } => {
                let function = module.get_function(name).unwrap();

                let call_arguments = arguments
                    .iter()
                    .map(|a| self.compile_expression(a, compiled_function, builtins, module))
                    .collect::<Result<Vec<_>, _>>()?
                    .iter_mut()
                    .map(|a| match a {
                        ValueType::Value(v) => *v,
                        ValueType::Reference(rc_value) => {
                            // TODO in this case we need to codegen refcount increment on enter and
                            // decrement on exit!
                            rc_value.as_ptr().as_basic_value_enum().into()
                        }
                    })
                    .collect::<Vec<_>>();

                // todo functions can return a Reference too, we need to consider that and return
                // the right expression result!
                let call_result = self
                    .context
                    .builder
                    .build_call(function, &call_arguments, name)
                    .map_err(|e| {
                        e.into_compile_error_at(compiled_function.module.path.clone(), *position)
                    })?;

                let call_result = call_result.as_any_value_enum().try_into().unwrap_or(
                    self.context
                        .llvm_context
                        .i8_type()
                        .const_zero()
                        .as_basic_value_enum()
                        .into(),
                );

                Ok(ValueType::Value(call_result))
            }
            crate::ast::Expression::Literal(literal, _) => match literal {
                crate::ast::Literal::String(s, _) => {
                    let characters_value = self
                        .context
                        .builder
                        .build_global_string_ptr(s, "literal0_global")
                        .unwrap();

                    let literal_value = self
                        .context
                        .builder
                        .build_malloc(builtins.string_type, "literal0_value")
                        .unwrap();
                    let literal_value_characters = unsafe {
                        self.context
                            .builder
                            .build_gep(
                                builtins.string_type,
                                literal_value,
                                &[self.context.llvm_context.i64_type().const_int(0, false)],
                                "literal0_value_characters",
                            )
                            .unwrap()
                    };
                    self.context
                        .builder
                        .build_store(literal_value_characters, characters_value)
                        .unwrap();

                    let rc = RcValue::build_init(
                        "literal0",
                        literal_value,
                        &self.context,
                        compiled_function,
                        &self.context.builtins,
                    )?;

                    Ok(ValueType::Reference(rc))
                }
            },
            Expression::VariableReference(name, _) => {
                // TODO determine based on the signature if this should be a Value or Reference
                Ok(ValueType::Value(
                    compiled_function
                        .scope
                        .get_variable(&Identifier(name.clone()))
                        .unwrap()
                        .into(),
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
        }
    }
}
