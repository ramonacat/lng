mod context;
mod module;
mod rc_builder;
pub(crate) mod scope;
mod string_builder;
mod value;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use context::{Builtins, CompilerContext};
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use module::CompiledModule;
use rc_builder::RcValue;
use scope::{GlobalScope, Scope};
use string_builder::StringValue;
use value::{FunctionHandle, StructHandle, Value};

use crate::{
    ast::SourceRange,
    runtime::register_mappings,
    std::compile_std,
    types::{self, FQName, Identifier, Visibility},
};

// TODO instead of executing the code, this should return some object that exposes the
// executable code with a safe interface
pub fn compile(
    program: &types::Module,
    std_program: &types::Module,
    register_global_mappings: RegisterGlobalMappings,
) -> Result<(), CompileError> {
    let context = Context::create();
    let std = compile_std(std_program, &context)?;

    let compiler = Compiler::new(&context, Some(std));

    let global_scope = compiler.compile(program)?;
    let root_module = context.create_module("root");

    for module in global_scope.into_modules() {
        println!("{}", module.to_ir());

        let module_path = module.path();
        let finalized = module.finalize();

        root_module.link_in_module(finalized).map_err(|e| {
            CompileErrorDescription::InternalError(e.to_string()).in_module(module_path)
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

    (register_global_mappings.unwrap_or_else(|| Box::new(register_mappings)))(
        &execution_engine,
        &root_module,
    );

    unsafe {
        let main = execution_engine
            // TODO we should allow for main to be in any module, not just "main"
            .get_function::<unsafe extern "C" fn()>(
                types::FQName::parse("main.main").into_mangled().as_str(),
            )
            .unwrap();
        main.call();
    }

    Ok(())
}

type RegisterGlobalMappings = Option<Box<dyn FnOnce(&ExecutionEngine, &Module)>>;

pub(crate) struct Compiler<'ctx> {
    context: CompilerContext<'ctx>,
}

#[derive(Debug, Clone)]
pub enum ErrorLocation {
    Position(FQName, SourceRange),
    Indeterminate,
    ItemOnly(FQName),
}

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Position(fqname, source_range) => {
                write!(f, "{source_range} in module {fqname}")
            }
            Self::Indeterminate => write!(f, "indeterminate"),
            Self::ItemOnly(fqname) => write!(f, "{fqname}"),
        }
    }
}

#[derive(Debug)]
pub struct CompileError {
    description: CompileErrorDescription,
    position: Box<ErrorLocation>,
}

#[derive(Debug)]
pub enum CompileErrorDescription {
    ModuleNotFound(FQName),
    FunctionNotFound {
        module_name: FQName,
        function_name: Identifier,
    },
    InternalError(String),
    StructNotFound {
        module_name: FQName,
        struct_name: types::Identifier,
    },
}

impl CompileErrorDescription {
    // TODO clean up all the unwraps so that this actually will be used :)
    #[allow(unused)]
    fn at(self, module_path: FQName, position: SourceRange) -> CompileError {
        CompileError {
            description: self,
            position: Box::new(ErrorLocation::Position(module_path, position)),
        }
    }

    fn in_module(self, name: FQName) -> CompileError {
        CompileError {
            description: self,
            position: Box::new(ErrorLocation::ItemOnly(name)),
        }
    }

    fn at_indeterminate(self) -> CompileError {
        CompileError {
            description: self,
            position: Box::new(ErrorLocation::Indeterminate),
        }
    }
}

impl Display for CompileErrorDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ModuleNotFound(name) => write!(f, "Module {name} not found"),
            Self::FunctionNotFound {
                module_name,
                function_name,
            } => write!(
                f,
                "Function {function_name} was not found in module {module_name}"
            ),
            Self::InternalError(message) => {
                write!(f, "Internal error: {message}")
            }
            Self::StructNotFound {
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
            self.description, self.position
        )
    }
}

impl From<BuilderError> for CompileErrorDescription {
    fn from(value: BuilderError) -> Self {
        Self::InternalError(format!("{value}"))
    }
}

trait IntoCompileError {
    fn into_compile_error_at(self, module_path: FQName, position: SourceRange) -> CompileError;
}

impl IntoCompileError for BuilderError {
    fn into_compile_error_at(self, module_path: FQName, position: SourceRange) -> CompileError {
        CompileError {
            description: self.into(),
            position: Box::new(ErrorLocation::Position(module_path, position)),
        }
    }
}

pub(crate) struct CompiledFunction<'ctx> {
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
    ) -> Result<(), CompileError> {
        context
            .builder
            .build_return(return_value)
            .map_err(|e| e.into_compile_error_at(self.handle.fqname, self.handle.position))?;

        Ok(())
    }

    fn set_return_value(&mut self, value: Value<'ctx>) {
        self.return_value = Some(value);
    }
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, std: Option<GlobalScope<'ctx>>) -> Self {
        let builtins = Builtins {
            string_handle: string_builder::describe_structure(),
            rc_handle: rc_builder::describe_structure(),
        };

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                global_scope: std.unwrap_or_else(|| {
                    let mut scope = GlobalScope::default();
                    scope
                        .create_module(FQName::parse("std"), context.create_module("std"))
                        .set_variable(
                            Identifier::parse("string"),
                            Value::Struct(builtins.string_handle.clone()),
                        );
                    scope
                }),
                builtins,
            },
        }
    }

    // TODO split into smaller functions and remove the allow
    #[allow(clippy::too_many_lines)]
    pub fn compile(mut self, program: &types::Module) -> Result<GlobalScope<'ctx>, CompileError> {
        for (path, declaration) in &program.all(None) {
            let module_path = path.without_last();
            let created_module = if self.context.global_scope.get_module(module_path).is_none() {
                let llvm_module = self
                    .context
                    .llvm_context
                    .create_module((*path).into_mangled().as_str());

                self.context
                    .global_scope
                    .create_module(module_path, llvm_module)
            } else {
                self.context
                    .global_scope
                    .get_module_mut(module_path)
                    .unwrap()
            };

            {
                match &declaration.kind {
                    types::ItemKind::Function(function) => {
                        let function_handle = FunctionHandle {
                            // TODO main should be detected during typecheck (with signature
                            // verification, checks that only one exists in the program, etc.)
                            visibility: if function.name.last().raw() == "main"
                                || matches!(
                                    function.definition.body,
                                    types::FunctionBody::Extern(_)
                                ) {
                                Visibility::Export
                            } else {
                                declaration.visibility
                            },
                            name: function.mangled_name(),
                            fqname: function.name,
                            return_type: function.definition.return_type.clone(),
                            arguments: function.definition.arguments.clone(),
                            position: function.definition.position,
                        };

                        created_module
                            .set_variable(function.name.last(), Value::Function(function_handle));
                    }
                    types::ItemKind::Struct(struct_) => {
                        let static_fields = struct_
                            .impls
                            .iter()
                            .map(|(name, impl_)| {
                                let handle = FunctionHandle {
                                    fqname: struct_.name.with_part(*name),
                                    visibility: declaration.visibility,
                                    name: impl_.mangled_name(),
                                    return_type: impl_.definition.return_type.clone(),
                                    arguments: impl_.definition.arguments.clone(),
                                    position: impl_.definition.position,
                                };
                                (*name, Value::Function(handle))
                            })
                            .collect();

                        created_module.set_variable(
                            struct_.name.last(),
                            Value::Struct(StructHandle::new_with_statics(
                                struct_.clone(),
                                static_fields,
                            )),
                        );
                    }
                    types::ItemKind::Import(_) => {}
                    types::ItemKind::Module(_) => todo!(),
                }
            }
        }

        for (name, item) in &program.all(None) {
            let module_path = name.without_last();
            let Some(module) = self.context.global_scope.get_module(module_path) else {
                return Err(CompileErrorDescription::ModuleNotFound(module_path).at_indeterminate());
            };

            let types::ItemKind::Import(import) = &item.kind else {
                continue;
            };

            let value = self
                .context
                .global_scope
                .get_value(import.imported_item)
                .unwrap();

            match value {
                Value::Primitive(_, _) => todo!(),
                Value::Reference(_) => todo!(),
                Value::Function(function) => {
                    module.import_function(&function, &self.context);

                    let function_value = Value::Function(FunctionHandle {
                        fqname: function.fqname,
                        visibility: function.visibility,
                        name: function.name.clone(),
                        return_type: function.return_type,
                        position: import.position,
                        arguments: function.arguments.clone(),
                    });

                    module.set_variable(name.last(), function_value);
                }
                Value::Struct(ref struct_) => {
                    struct_.import(module, &self.context);
                }
                Value::Empty => todo!(),
            }
        }
        for (item_name, item) in &program.all(None) {
            let module_path = item_name.without_last();
            let Some(module) = self.context.global_scope.get_module(module_path) else {
                return Err(CompileErrorDescription::ModuleNotFound(module_path).at_indeterminate());
            };

            {
                match &item.kind {
                    types::ItemKind::Function(function) => {
                        self.compile_function(
                            module
                                .get_variable(item_name.last())
                                .unwrap()
                                .as_function()
                                .unwrap(),
                            module,
                            &function.definition,
                        )?;
                    }
                    types::ItemKind::Import(_) => {}
                    types::ItemKind::Struct(struct_) => {
                        for (impl_name, impl_) in &struct_.impls {
                            self.compile_function(
                                module
                                    .get_variable(item_name.last())
                                    .unwrap()
                                    .as_struct()
                                    .unwrap()
                                    .read_field_value(Value::Empty, *impl_name)
                                    .unwrap()
                                    .as_function()
                                    .unwrap(),
                                module,
                                &impl_.definition,
                            )?;
                        }
                    }
                    types::ItemKind::Module(_) => todo!(),
                }
            }
        }

        Ok(self.context.global_scope)
    }

    // TODO split into smaller functions, remove the allow
    #[allow(clippy::too_many_lines)]
    fn compile_function(
        &self,
        handle: FunctionHandle,
        // TODO remove this argument, get this from module when needed
        module: &module::CompiledModule<'ctx>,
        function: &types::FunctionDefinition,
    ) -> Result<(), CompileError> {
        let types::FunctionBody::Statements(statements) = &function.body else {
            return Ok(());
        };
        let mut compiled_function = module.begin_compile_function(handle, function, &self.context);

        for statement in statements {
            let self_value = compiled_function.scope.get_value(Identifier::parse("self"));

            match statement {
                types::Statement::Expression(expression) => {
                    self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
                        module,
                    )?;
                }
                types::Statement::Let(let_) => {
                    let value = self.compile_expression(
                        &let_.value,
                        self_value,
                        &mut compiled_function,
                        module,
                    )?;

                    compiled_function.scope.set_value(let_.binding, value.1);
                }
                types::Statement::Return(expression) => {
                    let value = self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
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
                        Value::Reference(rc_value) => rc_value.as_ptr() != x.as_ptr(),
                        Value::Primitive(_, _)
                        | Value::Function(_)
                        | Value::Struct(_)
                        | Value::Empty => true,
                    }
                })
                .cloned()
                .collect::<Vec<_>>(),
            compiled_function.end,
        );
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
            match &r.as_basic_value() {
                BasicValueEnum::ArrayValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
                BasicValueEnum::IntValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
                BasicValueEnum::FloatValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
                BasicValueEnum::PointerValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
                BasicValueEnum::StructValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
                BasicValueEnum::VectorValue(v) => {
                    compiled_function.build_return(Some(v), &self.context)?;
                }
            }
        } else {
            compiled_function.build_return(None, &self.context)?;
        }
        Ok(())
    }

    // TODO: split into smaller functions and remove the allow
    #[allow(clippy::too_many_lines)]
    fn compile_expression(
        &self,
        expression: &types::Expression,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module: &CompiledModule<'ctx>,
    ) -> Result<(Option<Value<'ctx>>, Value<'ctx>), CompileError> {
        let position = expression.position;

        match &expression.kind {
            types::ExpressionKind::Call { target, arguments } => {
                let compiled_target =
                    self.compile_expression(target, self_.clone(), compiled_function, module)?;
                let (self_value, Value::Function(function)) = compiled_target else {
                    todo!();
                };

                let mut compiled_arguments_iter = arguments
                    .iter()
                    .map(|a| {
                        self.compile_expression(a, self_value.clone(), compiled_function, module)
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
                        Value::Empty => todo!(),
                    })
                    .collect::<Vec<BasicMetadataValueEnum>>();

                let call_result = self
                    .context
                    .builder
                    .build_call(
                        function.get_or_create_in_module(module, &self.context),
                        &call_arguments,
                        &format!("call_result_{}", position.as_id()),
                    )
                    .map_err(|e| {
                        e.into_compile_error_at(compiled_function.handle.fqname, position)
                    })?;

                let call_result = call_result.as_any_value_enum();

                let value = match function.return_type {
                    types::Type::Unit => Value::Empty,
                    types::Type::Object(item_path) => Value::Reference(RcValue::from_pointer(
                        call_result.into_pointer_value(),
                        self.context
                            .global_scope
                            .get_value(item_path)
                            .unwrap()
                            .as_struct()
                            .unwrap(),
                    )),
                    types::Type::Array(_) => todo!(),
                    types::Type::StructDescriptor(_) => todo!(),
                    types::Type::Callable { .. } => todo!(),
                    types::Type::U64 => todo!(),
                    types::Type::U8 => todo!(),
                    types::Type::Pointer(_) => todo!(),
                };

                Ok((None, value))
            }
            types::ExpressionKind::Literal(literal) => match literal {
                types::Literal::String(s) => {
                    let value = StringValue::new_literal(s.clone());
                    let name = format!("literal_{}", position.as_id());
                    let rc = value.build_instance(&name, &self.context);
                    compiled_function.rcs.push(rc.clone());

                    Ok((None, Value::Reference(rc)))
                }
                types::Literal::UnsignedInteger(value) => Ok((
                    None,
                    Value::Primitive(
                        self.context.get_std_type("u64").unwrap(),
                        self.context
                            .llvm_context
                            .i64_type()
                            .const_int(*value, false)
                            .as_basic_value_enum(),
                    ),
                )),
            },
            types::ExpressionKind::VariableAccess(name) => {
                Ok((None, compiled_function.scope.get_value(*name).unwrap()))
            }
            types::ExpressionKind::StructConstructor(name) => {
                let s = compiled_function
                    .scope
                    .get_value(*name)
                    .unwrap()
                    .as_struct()
                    .unwrap();

                let field_values = HashMap::new();
                // TODO actually set the field values!

                let value = s.build_heap_instance(
                    &self.context,
                    &format!("{}_{}", name, position.as_id()),
                    field_values,
                );

                let rc = RcValue::build_init(
                    &format!("{}_rc_{}", name, position.as_id()),
                    value,
                    s.clone(),
                    &self.context,
                );
                compiled_function.rcs.push(rc.clone());

                Ok((None, Value::Reference(rc)))
            }
            types::ExpressionKind::FieldAccess {
                target,
                field: field_name,
            } => {
                let (_, target_value) =
                    self.compile_expression(target, self_, compiled_function, module)?;

                let access_result = target_value.read_field_value(*field_name).unwrap();

                Ok((Some(target_value), access_result))
            }
            types::ExpressionKind::SelfAccess => Ok((None, self_.unwrap())),
        }
    }
}
