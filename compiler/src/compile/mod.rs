pub mod builtins;
mod context;
pub(crate) mod mangler;
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
    values::{AnyValue, BasicMetadataValueEnum, BasicValue},
};
use mangler::{MangledIdentifier, mangle_module_id, mangle_type};
use module::CompiledModule;
use rand::Rng;
use scope::{GlobalScope, Scope};
use value::{StructInstance, Value};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    std::{TYPE_NAME_STRING, TYPE_NAME_U64, compile_std, runtime::register_mappings},
    types::{self, structs::InstantiatedStructId},
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

    let CompiledRootModule::Library {
        scope: std_scope,
        modules: std_modules,
    } = std
    else {
        todo!();
    };

    let compiler = Compiler::new(&context, Some(std_scope), Some(std_modules));

    let compiled_root_module = compiler.compile(program);
    let root_module = context.create_module("root");

    let CompiledRootModule::App {
        scope: _,
        main,
        modules,
    } = compiled_root_module
    else {
        todo!();
    };

    for module in modules.into_modules() {
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
            .get_function::<unsafe extern "C" fn()>(main.as_str())
            .unwrap();
        main.call();
    }

    Ok(())
}

type RegisterGlobalMappings = Option<Box<dyn FnOnce(&ExecutionEngine, &Module)>>;

#[derive(Debug)]
pub struct Modules<'ctx> {
    modules: HashMap<types::modules::ModuleId, CompiledModule<'ctx>>,
}

impl<'ctx> Modules<'ctx> {
    pub(super) fn get_or_create(
        &mut self,
        path: types::modules::ModuleId,
        scope: &Rc<Scope<'ctx>>,
        create_llvm_module: impl FnOnce() -> Module<'ctx>,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .entry(path)
            .or_insert_with(|| CompiledModule::new(scope.child(), create_llvm_module()))
    }

    #[must_use]
    pub fn get(&self, path: types::modules::ModuleId) -> Option<&CompiledModule<'ctx>> {
        self.modules.get(&path)
    }

    fn get_mut(
        &mut self,
        module_path: types::modules::ModuleId,
    ) -> Option<&mut CompiledModule<'ctx>> {
        self.modules.get_mut(&module_path)
    }

    pub fn into_modules(self) -> impl Iterator<Item = CompiledModule<'ctx>> {
        self.modules.into_values()
    }

    #[must_use]
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
}

pub(crate) struct Compiler<'ctx> {
    context: CompilerContext<'ctx>,
    global_scope: GlobalScope<'ctx>,
    modules: Modules<'ctx>,
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
    entry: BasicBlock<'ctx>,
    end: BasicBlock<'ctx>,
    scope: Rc<Scope<'ctx>>,
    rcs: Vec<RcValue<'ctx>>,
    return_value: Option<Value<'ctx>>,
}

impl<'ctx> CompiledFunction<'ctx> {
    fn set_return_value(&mut self, value: Value<'ctx>) {
        self.return_value = Some(value);
    }
}

#[derive(Debug)]
pub enum CompiledRootModule<'ctx> {
    App {
        scope: GlobalScope<'ctx>,
        modules: Modules<'ctx>,
        main: MangledIdentifier,
    },
    Library {
        scope: GlobalScope<'ctx>,
        modules: Modules<'ctx>,
    },
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(
        context: &'ctx Context,
        std: Option<GlobalScope<'ctx>>,
        modules: Option<Modules<'ctx>>,
    ) -> Self {
        let builtins = Builtins {
            rc_handle: rc::describe_structure(),
        };

        let global_scope = std.unwrap_or_else(|| {
            // If there is no std, then we want to get in early and create the builtins, so that
            // the rest of the compilation can just add methods, etc. to them

            let mut structs = HashMap::new();

            structs.insert(*TYPE_NAME_STRING, string::describe_structure());
            structs.insert(*TYPE_NAME_ARRAY, array::describe_structure());

            GlobalScope::new(structs, HashMap::new())
        });

        let modules = modules.unwrap_or_else(Modules::new);

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                builtins,
            },
            global_scope,
            modules,
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

        let rc_struct_id = types::structs::StructId::InModule(
            types::modules::ModuleId::parse("std"),
            Identifier::parse("rc"),
        );
        structs.insert(rc_struct_id, rc::describe_structure());

        self.global_scope.structs = AllItems::new(structs, functions);
        self.global_scope
            .structs
            .get_or_instantiate_struct(&InstantiatedStructId::new(
                *TYPE_NAME_U64,
                types::TypeArgumentValues::new_empty(),
            ));

        for module in program.modules().keys() {
            self.get_or_create_module(*module);
        }

        if let Some(main) = main {
            let main = self
                .global_scope
                .structs
                .get_or_instantiate_function(&types::functions::InstantiatedFunctionId::new(
                    main,
                    types::TypeArgumentValues::new_empty(),
                ))
                .unwrap()
                .clone();
            self.compile_function(&main).unwrap();
            let mangled_main = mangle_type(&main.type_);

            CompiledRootModule::App {
                scope: self.global_scope,
                modules: self.modules,
                main: mangled_main,
            }
        } else {
            CompiledRootModule::Library {
                modules: self.modules,
                scope: self.global_scope,
            }
        }
    }

    fn get_or_create_module(
        &mut self,
        module_path: types::modules::ModuleId,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .get_or_create(module_path, self.global_scope.scope(), || {
                self.context
                    .llvm_context
                    .create_module(mangle_module_id(module_path).as_str())
            })
    }

    fn compile_function(
        &mut self,
        handle: &types::functions::Function<types::InstantiatedType>,
    ) -> Result<(), CompileError> {
        let types::functions::FunctionBody::Statements(statements) = &handle.body else {
            return Ok(());
        };

        let module_path = handle.module_name;

        self.get_or_create_module(module_path);

        let module = self.modules.get_mut(module_path).unwrap();

        let mut compiled_function =
            module.begin_compile_function(handle, &self.context, &mut self.global_scope);

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
                        Value::Primitive(_, _)
                        | Value::Function(_)
                        | Value::InstantiatedStruct(_)
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

        if let Some(return_value) = compiled_function
            .return_value
            .as_ref()
            .map(Value::as_basic_value)
        {
            self.context
                .builder
                .build_return(Some(&return_value))
                .unwrap();
        } else {
            self.context.builder.build_return(None).unwrap();
        }

        Ok(())
    }

    fn compile_statements(
        &mut self,
        statements: &Vec<types::Statement<types::InstantiatedType>>,
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
        expression: &types::Expression<types::InstantiatedType>,
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
                let value = self.compile_expression_global_variable_access(*module_id, *name);

                Ok((None, value.unwrap()))
            }
            types::ExpressionKind::StructConstructor(target, field_values) => {
                let target =
                    self.compile_expression(target, self_.clone(), compiled_function, module_path)?;

                let (name, target_tav) = match &target.1 {
                    Value::Empty => todo!(),
                    Value::Primitive(_, _) => todo!(),
                    Value::Reference(_) => todo!(),
                    Value::Function(_) => todo!(),
                    Value::InstantiatedStruct(struct_) => {
                        (struct_.id(), struct_.argument_values().clone())
                    }
                };
                // TODO ensure the struct is instantiated in the context
                // TODO get the type argument values from the expression!
                let field_values = field_values
                    .iter()
                    .map(|x| {
                        Ok((
                            x.name,
                            self.compile_expression(
                                &x.value,
                                self_.clone(),
                                compiled_function,
                                module_path,
                            )
                            .map(|y| y.1.as_basic_value())?,
                        ))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                let value = self
                    .global_scope
                    .structs
                    .get_or_instantiate_struct(&types::structs::InstantiatedStructId::new(
                        name,
                        target_tav.clone(),
                    ))
                    .unwrap()
                    .build_heap_instance(
                        &self.context,
                        &unique_name(&[&name.to_string()]),
                        field_values,
                    );

                let rc = RcValue::build_init(
                    &unique_name(&[&name.to_string(), "rc"]),
                    &StructInstance::new(
                        value,
                        types::InstantiatedType::new(types::InstantiatedTypeKind::Object {
                            type_name: name,
                            type_argument_values: target_tav,
                        }),
                    ),
                    &self.context,
                    &mut self.global_scope,
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
                    .read_field_value(*field_name, &self.context, &self.global_scope)
                    .unwrap();

                Ok((Some(target_value), access_result))
            }
            // TODO do we still need this?
            types::ExpressionKind::SelfAccess => Ok((None, self_.unwrap())),
        }
    }

    fn compile_expression_global_variable_access(
        &mut self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> Option<Value<'ctx>> {
        // TODO there should be something on the global_scope to receive a value by FQN
        let value = {
            let this = &self.modules;
            this.modules
                .get(&module_id)
                .and_then(|x| x.get_variable(name))
        }
        .or_else(|| {
            let function_id = types::functions::FunctionId::InModule(module_id, name);

            let instantiated_function_id = types::functions::InstantiatedFunctionId::new(
                function_id,
                // TODO we need to ensure during typecheck that we won't get
                // here without the right TypeArgumentValues
                types::TypeArgumentValues::new_empty(),
            );
            self.global_scope
                .structs
                .get_or_instantiate_function(&instantiated_function_id)
                .map(|x| {
                    self.modules
                        .get_mut(module_id)
                        .unwrap()
                        .get_or_create_function(x, &self.context);

                    Value::Function(function_id)
                })
        })
        .or_else(|| {
            let struct_id = types::structs::StructId::InModule(module_id, name);

            self.global_scope
                .structs
                .get_or_instantiate_struct(&types::structs::InstantiatedStructId::new(
                    struct_id,
                    types::TypeArgumentValues::new_empty(),
                ))
                .map(|x| {
                    let types::InstantiatedTypeKind::Struct(x) = x.definition.type_.kind() else {
                        todo!();
                    };
                    let instantiated_struct_id = types::structs::InstantiatedStructId::new(
                        struct_id,
                        x.argument_values().clone(),
                    );

                    Value::InstantiatedStruct(instantiated_struct_id)
                })
        });
        value
    }

    fn compile_literal(
        &mut self,
        compiled_function: &mut CompiledFunction<'ctx>,
        literal: &types::Literal,
    ) -> (Option<Value<'ctx>>, Value<'ctx>) {
        match literal {
            types::Literal::String(s) => {
                let value = StringValue::new_literal(s.clone());
                let name = &unique_name(&["literal", "string"]);
                let rc = value.build_instance(name, &self.context, &mut self.global_scope);
                compiled_function.rcs.push(rc.clone());

                (None, Value::Reference(rc))
            }
            types::Literal::UnsignedInteger(value) => (
                None,
                Value::Primitive(
                    types::structs::InstantiatedStructId::new(
                        CompilerContext::get_std_type("u64"),
                        types::TypeArgumentValues::new_empty(),
                    ),
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
            Value::Primitive(_, _) => todo!(),
            Value::Reference(_) => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        };

        let instantiated_function_id = types::functions::InstantiatedFunctionId::new(
            *function_id,
            types::TypeArgumentValues::new_empty(),
        );
        let definition = self
            .global_scope
            .structs
            .get_or_instantiate_function(&instantiated_function_id)
            .unwrap()
            .clone();

        if !self
            .modules
            .get(definition.module_name)
            .unwrap()
            .has_function(&definition.type_)
        {
            self.compile_function(&definition).unwrap();
        }

        let function_value = self
            .modules
            .get_mut(module_path)
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
                Value::Primitive(_, v) => v.as_basic_value_enum().into(),
                Value::Reference(rc_value) => rc_value.as_ptr().as_basic_value_enum().into(),
                Value::Function(_) => todo!("implement passing callables as arguments"),
                Value::Empty => todo!(),
                Value::InstantiatedStruct(_) => todo!(),
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
                self.global_scope
                    .structs
                    .get_or_instantiate_struct(&types::structs::InstantiatedStructId::new(
                        *id,
                        type_argument_values.clone(),
                    ))
                    .unwrap()
                    .definition
                    .type_
                    .clone(),
            )),
            types::InstantiatedTypeKind::Array { .. } => todo!(),
            types::InstantiatedTypeKind::Callable { .. } => todo!(),
            types::InstantiatedTypeKind::U64 => todo!(),
            types::InstantiatedTypeKind::U8 => todo!(),
            types::InstantiatedTypeKind::Pointer(_) => todo!(),
            types::InstantiatedTypeKind::Struct(_) => todo!(),
            types::InstantiatedTypeKind::Function(_) => todo!(),
            types::InstantiatedTypeKind::IndirectCallable(_, _) => todo!(),
            types::InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
        };
        Ok(value)
    }
}
