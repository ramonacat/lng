pub mod builtins;
mod context;
mod module;
pub(crate) mod scope;
mod value;

use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use builtins::{
    array,
    rc::{self, RcValue},
    string::{self, StringValue},
};
use context::{AllItems, Builtins, CompilerContext};
use inkwell::{
    basic_block::BasicBlock,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use module::CompiledModule;
use rand::Rng;
use scope::{GlobalScope, Scope};
use value::{StructInstance, Value};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    std::{TYPE_NAME_STRING, compile_std, runtime::register_mappings},
    types::{
        self, InstantiatedType,
        functions::{InstantiatedFunctionId, InstantiatedStructId},
    },
};

use self::array::TYPE_NAME_ARRAY;

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
    program: &types::modules::RootModule,
    std_program: &types::modules::RootModule,
    register_global_mappings: RegisterGlobalMappings,
) -> Result<(), CompileError> {
    let context = Context::create();
    let std = compile_std(std_program, &context)?;

    let CompiledRootModule::Library { scope: std_scope } = std else {
        todo!();
    };

    let compiler = Compiler::new(&context, Some(std_scope));

    let compiled_root_module = compiler.compile(program);
    let root_module = context.create_module("root");

    let CompiledRootModule::App {
        scope: global_scope,
        main,
    } = compiled_root_module
    else {
        todo!();
    };

    for module in global_scope.into_modules() {
        println!("{}", module.to_ir());

        let finalized = module.finalize();

        root_module
            .link_in_module(finalized)
            .expect("module should be linked");
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
            .get_function::<unsafe extern "C" fn()>(
                main.into_mangled(&types::TypeArgumentValues::new_empty())
                    .as_str(),
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

#[derive(Debug)]
pub struct CompileError {
    description: CompileErrorDescription,
    position: ast::SourceSpan,
}

#[derive(Debug)]
pub enum CompileErrorDescription {
    ModuleNotFound(types::modules::ModuleId),
    FunctionNotFound {
        module_name: FQName,
        function_name: Identifier,
    },
    InternalError(String),
    StructNotFound {
        module_name: FQName,
        struct_name: Identifier,
    },
    FieldNotFound(FQName, Identifier),
    MissingGenericArguments(FQName, types::TypeArguments),
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
    fn at(self, position: ast::SourceSpan) -> CompileError;
}

impl IntoCompileError for BuilderError {
    fn at(self, position: ast::SourceSpan) -> CompileError {
        CompileError {
            description: self.into(),
            position,
        }
    }
}

pub(crate) struct CompiledFunction<'ctx> {
    // TODO can this just be FunctionId?
    handle: types::functions::Function<InstantiatedType>,

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
            .map_err(|e| e.at(self.handle.position))?;

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
        main: types::functions::FunctionId,
    },
    Library {
        scope: GlobalScope<'ctx>,
    },
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, std: Option<GlobalScope<'ctx>>) -> Self {
        let builtins = Builtins {
            rc_handle: rc::describe_structure(),
        };

        let global_scope = std.unwrap_or_else(|| {
            // If there is no std, then we want to get in early and create the builtins, so that
            // the rest of the compilation can just add methods, etc. to them
            let mut scope = GlobalScope::new(HashMap::new(), HashMap::new());

            let std = scope.get_or_create_module(types::modules::ModuleId::parse("std"), || {
                context.create_module("std")
            });

            std.set_variable(
                Identifier::parse("string"),
                Value::Struct(string::describe_structure()),
            );
            std.set_variable(
                Identifier::parse("array"),
                Value::Struct(array::describe_structure()),
            );
            std.set_variable(
                Identifier::parse("rc"),
                Value::Struct(rc::describe_structure()),
            );
            scope
        });

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                global_scope,
                builtins,
            },
        }
    }

    pub fn compile(mut self, program: &types::modules::RootModule) -> CompiledRootModule<'ctx> {
        let main = program.main();
        let mut structs = program.structs().clone();
        let functions = program.functions().clone();

        // TODO this is a bit hacky, we should have an attribute on the structs marking them as
        // predefined, so that the definitions are ignored, but the impls are not
        structs
            .get_mut(&*TYPE_NAME_STRING)
            .unwrap()
            .fields
            .append(&mut string::describe_structure().fields);

        // TODO should the array struct be declared in stdlib, like string?
        structs.insert(*TYPE_NAME_ARRAY, array::describe_structure());

        self.context.global_scope.structs = AllItems::new(structs, functions);

        for module in program.modules().keys() {
            self.get_or_create_module(*module);
        }

        if let Some(main) = main {
            let definition = self
                .context
                .global_scope
                .structs
                .inspect_instantiated_function(
                    &InstantiatedFunctionId::new(main, types::TypeArgumentValues::new_empty()),
                    |function| {
                        let function = function.unwrap();
                        // TODO verify in type_check that main has no generic arguments
                        // TODO can we avoid this clone?
                        function.clone()
                    },
                );

            self.compile_function(&definition).unwrap();

            CompiledRootModule::App {
                scope: self.context.global_scope,
                main,
            }
        } else {
            CompiledRootModule::Library {
                scope: self.context.global_scope,
            }
        }
    }

    fn get_or_create_module(
        &mut self,
        module_path: types::modules::ModuleId,
    ) -> &mut CompiledModule<'ctx> {
        self.context
            .global_scope
            .get_or_create_module(module_path, || {
                self.context
                    .llvm_context
                    .create_module(module_path.into_mangled().as_str())
            })
    }

    fn compile_function(
        &mut self,
        handle: &types::functions::Function<InstantiatedType>,
    ) -> Result<(), CompileError> {
        let types::functions::FunctionBody::Statements(statements) = &handle.body else {
            return Ok(());
        };

        let module_path = handle.module_name;
        self.get_or_create_module(module_path);
        let module = self.context.global_scope.get_module(module_path).unwrap();

        // TODO in reality, at this point we should already have the handle instantiated, do this
        // earlier!
        let mut compiled_function = module.begin_compile_function(handle, &self.context);

        self.compile_statements(statements, module_path, &mut compiled_function)?;

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
                        Value::Primitive(_, _, _)
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
                BasicValueEnum::ScalableVectorValue(_) => todo!(),
            }
        } else {
            compiled_function.build_return(None, &self.context)?;
        }
        Ok(())
    }

    fn compile_statements(
        &mut self,
        statements: &Vec<types::Statement<InstantiatedType>>,
        module_path: types::modules::ModuleId,
        compiled_function: &mut CompiledFunction<'ctx>,
    ) -> Result<(), CompileError> {
        for statement in statements {
            let self_value = compiled_function.scope.get_value(Identifier::parse("self"));

            match statement {
                types::Statement::Expression(expression) => {
                    self.compile_expression(
                        expression,
                        self_value,
                        compiled_function,
                        module_path,
                    )?;
                }
                types::Statement::Let(let_) => {
                    let value = self.compile_expression(
                        &let_.value,
                        self_value,
                        compiled_function,
                        module_path,
                    )?;

                    compiled_function.scope.set_value(let_.binding, value.1);
                }
                types::Statement::Return(expression) => {
                    let value = self.compile_expression(
                        expression,
                        self_value,
                        compiled_function,
                        module_path,
                    )?;

                    compiled_function.set_return_value(value.1);
                }
            }
        }

        Ok(())
    }

    fn compile_expression(
        &mut self,
        expression: &types::Expression<InstantiatedType>,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: types::modules::ModuleId,
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
            types::ExpressionKind::Literal(literal) => {
                Ok(self.compile_literal(compiled_function, literal))
            }
            types::ExpressionKind::LocalVariableAccess(name) => {
                Ok((None, compiled_function.scope.get_value(*name).unwrap()))
            }
            types::ExpressionKind::GlobalVariableAccess(module_id, name) => {
                let value = self
                    .context
                    .global_scope
                    .get_value(*module_id, *name)
                    .or_else(|| {
                        let function_id = types::functions::FunctionId::InModule(*module_id, *name);

                        self.context
                            .global_scope
                            .structs
                            .inspect_instantiated_function(
                                &InstantiatedFunctionId::new(
                                    function_id,
                                    // TODO we need to ensure during typecheck that we won't get
                                    // here without the right TypeArgumentValues, we will need to
                                    // attach TypeArgumentValues to expressions
                                    types::TypeArgumentValues::new_empty(),
                                ),
                                |f_| {
                                    f_.map(|x| {
                                        self.context
                                            .global_scope
                                            .get_module(*module_id)
                                            .unwrap()
                                            .get_or_create_function(x, &self.context);

                                        Value::Function(function_id)
                                    })
                                },
                            )
                    });

                Ok((None, value.unwrap()))
            }
            // TODO instead of the name, we should take an expression here, so that dynamically
            // generated structs can be implemented
            types::ExpressionKind::StructConstructor(name) => {
                // TODO ensure the struct is instantiated in the context
                // TODO get the type argument values from the expression!
                let field_values = HashMap::new();
                // TODO actually set the field values!

                let value = self.context.global_scope.structs.inspect_instantiated(
                    &InstantiatedStructId::new(*name, types::TypeArgumentValues::new_empty()),
                    |s| {
                        s.unwrap().build_heap_instance(
                            &self.context,
                            &unique_name(&[&name.to_string()]),
                            field_values,
                        )
                    },
                );

                let rc = RcValue::build_init(
                    &unique_name(&[&name.to_string(), "rc"]),
                    &StructInstance::new(
                        value,
                        types::InstantiatedType::new(
                            types::InstantiatedTypeKind::Object {
                                type_name: *name,
                                type_argument_values: types::TypeArgumentValues::new_empty(),
                            },
                            types::TypeArgumentValues::new_empty(),
                        ),
                    ),
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

    fn compile_literal(
        &self,
        compiled_function: &mut CompiledFunction<'ctx>,
        literal: &types::Literal,
    ) -> (Option<Value<'ctx>>, Value<'ctx>) {
        match literal {
            types::Literal::String(s) => {
                let value = StringValue::new_literal(s.clone());
                let name = &unique_name(&["literal", "string"]);
                let rc = value.build_instance(name, &self.context);
                compiled_function.rcs.push(rc.clone());

                (None, Value::Reference(rc))
            }
            types::Literal::UnsignedInteger(value) => (
                None,
                Value::Primitive(
                    CompilerContext::get_std_type("u64"),
                    types::TypeArgumentValues::new_empty(),
                    self.context.const_u64(*value).as_basic_value_enum(),
                ),
            ),
        }
    }

    fn compile_expression_call(
        &mut self,
        self_: Option<&Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
        target: &types::Expression<types::InstantiatedType>,
        arguments: &[types::Expression<types::InstantiatedType>],
    ) -> Result<Value<'ctx>, CompileError> {
        let compiled_target =
            self.compile_expression(target, self_.cloned(), compiled_function, module_path)?;
        let self_value = compiled_target.0;

        let function_id = match &compiled_target.1 {
            Value::Function(function_id) => function_id,
            Value::Empty => todo!(),
            Value::Primitive(_, _, _) => todo!(),
            Value::Reference(_) => todo!(),
            Value::Struct(_) => todo!(),
        };

        let instantiated_function_id =
            InstantiatedFunctionId::new(*function_id, types::TypeArgumentValues::new_empty());
        let definition = self
            .context
            .global_scope
            .structs
            .inspect_instantiated_function(&instantiated_function_id, |function| {
                function.unwrap().clone()
            });

        if !self
            .context
            .global_scope
            .get_module(definition.module_name)
            .unwrap()
            // TODO mangle the type arguments as well!
            .has_function(&definition.mangled_id())
        {
            self.compile_function(&definition).unwrap();
        }

        let function_value = self
            .context
            .global_scope
            .get_module(module_path)
            .unwrap()
            .get_or_create_function(&definition, &self.context);

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
                Value::Primitive(_, _, v) => v.as_basic_value_enum().into(),
                Value::Reference(rc_value) => rc_value.as_ptr().as_basic_value_enum().into(),
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
                function_value,
                &call_arguments,
                &unique_name(&["call_result"]),
            )
            .map_err(|e| e.at(position))?;
        let call_result = call_result.as_any_value_enum();
        let value = match definition.return_type.kind() {
            types::InstantiatedTypeKind::Unit => Value::Empty,
            types::InstantiatedTypeKind::Object {
                type_name: id,
                type_argument_values,
            } => Value::Reference(RcValue::from_pointer(
                call_result.into_pointer_value(),
                self.context.global_scope.structs.inspect_instantiated(
                    &InstantiatedStructId::new(*id, type_argument_values.clone()),
                    |x| x.unwrap().definition.type_.clone(),
                ),
            )),
            types::InstantiatedTypeKind::Array { .. } => todo!(),
            types::InstantiatedTypeKind::Callable { .. } => todo!(),
            types::InstantiatedTypeKind::U64 => todo!(),
            types::InstantiatedTypeKind::U8 => todo!(),
            types::InstantiatedTypeKind::Pointer(_) => todo!(),
            types::InstantiatedTypeKind::Struct(_) => todo!(),
            types::InstantiatedTypeKind::Function(_) => todo!(),
        };
        Ok(value)
    }
}
