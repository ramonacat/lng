mod builtins;
mod context;
mod module;
pub(crate) mod scope;
mod value;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use builtins::{
    rc::{self, RcValue},
    string::{self, StringValue},
};
use context::{Builtins, CompilerContext};
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use module::CompiledModule;
use rand::Rng;
use scope::{GlobalScope, Scope};
use value::{FunctionHandle, StructHandle, StructInstance, Value};

use crate::{
    ast::SourceRange,
    errors::ErrorLocation,
    std::{compile_std, runtime::register_mappings},
    types::{self, FQName, Identifier, TypeArguments},
};

fn unique_name(parts: &[&str]) -> String {
    format!(
        "{}_{}",
        parts.join("_"),
        rand::rng()
            .sample_iter(rand::distr::Alphanumeric)
            .take(16)
            .map(char::from)
            .collect::<String>()
    )
}

// TODO instead of executing the code, this should return some object that exposes the
// executable code with a safe interface
// TODO support also generating binaries instead of only JITting
pub fn compile(
    program: &types::RootModule,
    std_program: &types::RootModule,
    register_global_mappings: RegisterGlobalMappings,
) -> Result<(), CompileError> {
    let context = Context::create();
    let std = compile_std(std_program, &context)?;

    let CompiledRootModule::Library { scope: std_scope } = std else {
        todo!();
    };

    let compiler = Compiler::new(&context, Some(std_scope));

    let compiled_root_module = compiler.compile(program)?;
    let root_module = context.create_module("root");

    let CompiledRootModule::App {
        scope: global_scope,
        main,
    } = compiled_root_module
    else {
        dbg!(compiled_root_module);
        todo!();
    };

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
            .get_function::<unsafe extern "C" fn()>(main.into_mangled().as_str())
            .unwrap();
        main.call();
    }

    Ok(())
}

type RegisterGlobalMappings = Option<Box<dyn FnOnce(&ExecutionEngine, &Module)>>;

pub(crate) struct Compiler<'ctx> {
    context: CompilerContext<'ctx>,
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
    FieldNotFound(FQName, Identifier),
    MissingGenericArguments(FQName, TypeArguments),
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
            Self::FieldNotFound(struct_name, field_name) => {
                write!(f, "Field {field_name} not found on struct {struct_name}")
            }
            Self::MissingGenericArguments(fqname, type_arguments) => write!(
                f,
                "missing generic arguments {type_arguments} in a call to {fqname}"
            ),
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

#[derive(Debug)]
pub enum CompiledRootModule<'ctx> {
    App {
        scope: GlobalScope<'ctx>,
        main: FQName,
    },
    Library {
        scope: GlobalScope<'ctx>,
    },
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, std: Option<GlobalScope<'ctx>>) -> Self {
        let builtins = Builtins {
            string_handle: string::describe_structure(),
            rc_handle: rc::describe_structure(),
        };

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                global_scope: std.unwrap_or_default(),
                builtins,
            },
        }
    }

    pub fn compile(
        mut self,
        program: &types::RootModule,
    ) -> Result<CompiledRootModule<'ctx>, CompileError> {
        let main = program.main();
        let root_module = program.root_module();

        self.declare_items(root_module, FQName::parse(""));
        self.resolve_imports(root_module, FQName::parse(""))?;

        // self.define_items(root_module, FQName::parse(""))?;

        if let Some(main) = main {
            let handle = self
                .context
                .global_scope
                .get_value(main)
                .unwrap()
                .as_function()
                .unwrap();
            // TODO verify in type_check that main has no generic arguments
            self.compile_function(&handle).unwrap();
            Ok(CompiledRootModule::App {
                scope: self.context.global_scope,
                main,
            })
        } else {
            Ok(CompiledRootModule::Library {
                scope: self.context.global_scope,
            })
        }
    }

    fn resolve_imports(
        &mut self,
        program: &types::Module,
        root_path: FQName,
    ) -> Result<(), CompileError> {
        for (name, item) in program.items() {
            match &item.kind {
                types::ItemKind::Function(_) | types::ItemKind::Struct(_) => {}
                types::ItemKind::Import(import) => {
                    let value = self
                        .context
                        .global_scope
                        .get_value(import.imported_item)
                        .unwrap();

                    let Some(module) = self.context.global_scope.get_module_mut(root_path) else {
                        return Err(
                            CompileErrorDescription::ModuleNotFound(root_path).at_indeterminate()
                        );
                    };

                    match value {
                        Value::Primitive(_, _) => todo!(),
                        Value::Reference(_) => todo!(),
                        Value::Function(function) => {
                            module.import_function(function.clone());

                            let function_value = Value::Function(function.clone());

                            module.set_variable(name, function_value);
                        }
                        Value::Struct(struct_) => {
                            module.set_variable(name, Value::Struct(struct_.clone()));
                        }
                        Value::Empty => todo!(),
                        Value::Callable(_, _) => todo!(),
                    }
                }
                types::ItemKind::Module(module) => {
                    self.resolve_imports(module, root_path.with_part(name))?;
                }
            }
        }

        Ok(())
    }

    fn declare_items(&mut self, program: &types::Module, root_path: FQName) {
        for (path, declaration) in program.items() {
            let module = self.get_or_create_module(root_path);

            match &declaration.kind {
                types::ItemKind::Function(function) => {
                    let function_handle = FunctionHandle {
                        definition: function.definition.clone(),
                        linkage: if declaration.visibility == types::Visibility::Export
                            || matches!(function.definition.body, types::FunctionBody::Extern(_))
                        {
                            Linkage::External
                        } else {
                            Linkage::Internal
                        },
                        name: function.mangled_name(),
                        fqname: function.name,
                        return_type: function.definition.return_type.clone(),
                        arguments: function.definition.arguments.clone(),
                        position: function.definition.position,
                        module_name: function.name.without_last(),
                    };

                    module.set_variable(function.name.last(), Value::Function(function_handle));
                }
                types::ItemKind::Struct(struct_) => {
                    module.set_variable(struct_.name.last(), Value::Struct(struct_.clone()));
                }
                types::ItemKind::Import(_) => {}
                types::ItemKind::Module(module_declaration) => {
                    self.declare_items(module_declaration, root_path.with_part(path));
                }
            }
        }
    }

    fn get_or_create_module(&mut self, module_path: FQName) -> &mut CompiledModule<'ctx> {
        self.context
            .global_scope
            .get_or_create_module(module_path, || {
                self.context
                    .llvm_context
                    .create_module(module_path.into_mangled().as_str())
            })
    }

    fn compile_function(&mut self, handle: &FunctionHandle) -> Result<(), CompileError> {
        let types::FunctionBody::Statements(statements) = &handle.definition.body else {
            return Ok(());
        };

        let module_path = handle.module_name;
        self.get_or_create_module(module_path);
        let module = self.context.global_scope.get_module(module_path).unwrap();

        let mut compiled_function = module.begin_compile_function(handle.clone(), &self.context);

        for statement in statements {
            let self_value = compiled_function.scope.get_value(Identifier::parse("self"));

            match statement {
                types::Statement::Expression(expression) => {
                    self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
                        module_path,
                    )?;
                }
                types::Statement::Let(let_) => {
                    let value = self.compile_expression(
                        &let_.value,
                        self_value,
                        &mut compiled_function,
                        module_path,
                    )?;

                    compiled_function.scope.set_value(let_.binding, value.1);
                }
                types::Statement::Return(expression) => {
                    let value = self.compile_expression(
                        expression,
                        self_value,
                        &mut compiled_function,
                        module_path,
                    )?;

                    compiled_function.set_return_value(value.1);
                }
            }
        }

        let cleanup_label = rc::build_cleanup(
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
                        | Value::Empty
                        | Value::Callable(_, _) => true,
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

    fn compile_expression(
        &mut self,
        expression: &types::Expression,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: FQName,
    ) -> Result<(Option<Value<'ctx>>, Value<'ctx>), CompileError> {
        let position = expression.position;

        match &expression.kind {
            types::ExpressionKind::Call { target, arguments } => {
                let value = self.compile_expression_call(
                    self_.as_ref(),
                    compiled_function,
                    module_path,
                    position,
                    target,
                    arguments,
                )?;

                Ok((None, value))
            }
            types::ExpressionKind::Literal(literal) => match literal {
                types::Literal::String(s) => {
                    let value = StringValue::new_literal(s.clone());
                    let name = &unique_name(&["literal", "string"]);
                    let rc = value.build_instance(name, &self.context);
                    compiled_function.rcs.push(rc.clone());

                    Ok((None, Value::Reference(rc)))
                }
                types::Literal::UnsignedInteger(value) => Ok((
                    None,
                    Value::Primitive(
                        self.context.get_std_type("u64").unwrap(),
                        self.context.const_u64(*value).as_basic_value_enum(),
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
                // TODO get the type argument values from the expression!
                let s = StructHandle::new(s);

                let field_values = HashMap::new();
                // TODO actually set the field values!

                let value = s.build_heap_instance(
                    &self.context,
                    &unique_name(&[&name.raw()]),
                    field_values,
                );

                let rc = RcValue::build_init(
                    &unique_name(&[&name.raw(), "rc"]),
                    &StructInstance::new(value, s.type_()),
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
                    self.compile_expression(target, self_, compiled_function, module_path)?;

                let access_result = target_value
                    .read_field_value(*field_name, &self.context)
                    .unwrap();

                Ok((Some(target_value), access_result))
            }
            types::ExpressionKind::SelfAccess => Ok((None, self_.unwrap())),
        }
    }

    fn compile_expression_call(
        &mut self,
        self_: Option<&Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: FQName,
        position: SourceRange,
        target: &types::Expression,
        arguments: &[types::Expression],
    ) -> Result<Value<'ctx>, CompileError> {
        let compiled_target =
            self.compile_expression(target, self_.cloned(), compiled_function, module_path)?;
        let self_value = compiled_target.0;

        let function = match &compiled_target.1 {
            Value::Callable(function_handle, _) | Value::Function(function_handle) => {
                function_handle
            }
            Value::Empty => todo!(),
            Value::Primitive(_, _) => todo!(),
            Value::Reference(_) => todo!(),
            Value::Struct(_) => todo!(),
        };

        if !self
            .context
            .global_scope
            .get_module(function.module_name)
            .unwrap()
            .has_function(&function.name)
        {
            self.compile_function(function).unwrap();
        }

        let compiled_target_function = self
            .context
            .global_scope
            .get_module(module_path)
            .unwrap()
            .get_or_create_function(function, &self.context);

        self.context
            .builder
            .position_at_end(compiled_function.entry);

        let mut compiled_arguments_iter = arguments
            .iter()
            .map(|a| {
                self.compile_expression(a, self_value.clone(), compiled_function, module_path)
                    .map(|x| x.1)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let call_arguments = compiled_arguments_iter
            .iter_mut()
            .map(|a| match a {
                Value::Primitive(_, v) => v.as_basic_value_enum().into(),
                Value::Reference(rc_value) => rc_value.as_ptr().as_basic_value_enum().into(),
                Value::Function(_) => todo!("implement passing callables as arguments"),
                Value::Struct(_) => {
                    todo!("implement passing struct definitions as arguments")
                }
                Value::Empty => todo!(),
                Value::Callable(_, _) => todo!(),
            })
            .collect::<Vec<BasicMetadataValueEnum>>();

        let call_result = self
            .context
            .builder
            .build_call(
                compiled_target_function,
                &call_arguments,
                &unique_name(&["call_result"]),
            )
            .map_err(|e| e.into_compile_error_at(compiled_function.handle.fqname, position))?;
        let call_result = call_result.as_any_value_enum();
        let value = match function.return_type.kind() {
            types::TypeKind::Unit => Value::Empty,
            types::TypeKind::Object {
                type_name: item_path,
            } => {
                let value_type = self
                    .context
                    .global_scope
                    .get_value(*item_path)
                    .unwrap()
                    .as_struct()
                    .unwrap();

                let value_type = StructHandle::new(value_type);

                Value::Reference(RcValue::from_pointer(
                    call_result.into_pointer_value(),
                    value_type.type_(),
                ))
            }
            types::TypeKind::Array { .. } => todo!(),
            types::TypeKind::StructDescriptor(_) => todo!(),
            types::TypeKind::Callable { .. } => todo!(),
            types::TypeKind::U64 => todo!(),
            types::TypeKind::U8 => todo!(),
            types::TypeKind::Pointer(_) => todo!(),
            types::TypeKind::Generic(_) => todo!(),
        };
        Ok(value)
    }
}
