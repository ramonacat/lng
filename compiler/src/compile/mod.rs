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
use context::{AllStructs, Builtins, CompilerContext};
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
use value::{FunctionHandle, StructInstance, Value};

use crate::{
    ast,
    std::{TYPE_NAME_STRING, compile_std, runtime::register_mappings},
    types,
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
    position: ast::SourceSpan,
}

#[derive(Debug)]
pub enum CompileErrorDescription {
    ModuleNotFound(types::FQName),
    FunctionNotFound {
        module_name: types::FQName,
        function_name: types::Identifier,
    },
    InternalError(String),
    StructNotFound {
        module_name: types::FQName,
        struct_name: types::Identifier,
    },
    FieldNotFound(types::FQName, types::Identifier),
    MissingGenericArguments(types::FQName, types::TypeArguments),
}

impl CompileErrorDescription {
    #[must_use]
    const fn at(self, position: ast::SourceSpan) -> CompileError {
        CompileError {
            description: self,
            position,
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

            let std = scope
                .get_or_create_module(types::FQName::parse("std"), || context.create_module("std"));

            std.set_variable(
                types::Identifier::parse("string"),
                Value::Struct(string::describe_structure()),
            );
            std.set_variable(
                types::Identifier::parse("array"),
                Value::Struct(array::describe_structure()),
            );
            std.set_variable(
                types::Identifier::parse("rc"),
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

    pub fn compile(
        mut self,
        program: &types::RootModule,
    ) -> Result<CompiledRootModule<'ctx>, CompileError> {
        let main = program.main();
        let root_module = program.root_module();
        let mut structs = program.structs().clone();
        let functions = program.functions().clone();

        // TODO this is a bit hacky, we should have an attribute on the structs marking them as
        // predefined, so that the definitions are ignored, but the impls are not
        structs
            .get_mut(&types::structs::StructId::FQName(*TYPE_NAME_STRING))
            .unwrap()
            .fields
            .append(&mut string::describe_structure().fields);

        // TODO should the array struct be declared in stdlib, like string?
        structs.insert(
            types::structs::StructId::FQName(*TYPE_NAME_ARRAY),
            array::describe_structure(),
        );

        self.context.global_scope.structs = AllStructs::new(structs);
        self.context.global_scope.functions = functions;

        self.declare_items(root_module, types::FQName::parse(""));
        self.resolve_imports(root_module, types::FQName::parse(""))?;

        // self.define_items(root_module, FQName::parse(""))?;

        if let Some(main) = main {
            let handle = self
                .context
                .global_scope
                // TODO remove this match, treat the id as opaque
                .get_value(match main {
                    types::functions::FunctionId::FQName(fqname) => fqname,
                    types::functions::FunctionId::Extern(identifier) => {
                        types::FQName::parse(&identifier.raw())
                    }
                })
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
        root_path: types::FQName,
    ) -> Result<(), CompileError> {
        for (name, item) in program.items() {
            match &item.kind {
                types::ItemKind::Import(import) => {
                    let value = self
                        .context
                        .global_scope
                        .get_value(import.imported_item)
                        .unwrap();

                    let Some(module) = self.context.global_scope.get_module_mut(root_path) else {
                        return Err(
                            CompileErrorDescription::ModuleNotFound(root_path).at(import.position)
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
                    }
                }
                types::ItemKind::Module(module) => {
                    self.resolve_imports(module, root_path.with_part(name))?;
                }
                types::ItemKind::Function(_) | types::ItemKind::Struct(_) => {}
            }
        }

        Ok(())
    }

    fn declare_items(&mut self, program: &types::Module, root_path: types::FQName) {
        for (path, declaration) in program.items() {
            self.get_or_create_module(root_path);

            match &declaration.kind {
                types::ItemKind::Function(function_id) => {
                    let function_handle = {
                        let function = self
                            .context
                            .global_scope
                            .functions
                            .get(function_id)
                            .unwrap();

                        FunctionHandle {
                            id: *function_id,
                            module_name: root_path,
                            linkage: if declaration.visibility == types::Visibility::Export
                                || matches!(
                                    function.body,
                                    types::functions::FunctionBody::Extern(_)
                                ) {
                                Linkage::External
                            } else {
                                Linkage::Internal
                            },
                            position: function.position,
                            arguments: function.arguments.clone(),
                            return_type: function.return_type.clone(),
                            body: function.body.clone(),
                        }
                    };

                    // TODO remove this match, treat function.id as opaque
                    self.get_or_create_module(root_path).set_variable(
                        match function_id {
                            types::functions::FunctionId::FQName(fqname) => fqname.last(),
                            types::functions::FunctionId::Extern(identifier) => *identifier,
                        },
                        Value::Function(function_handle),
                    );
                }
                types::ItemKind::Module(module_declaration) => {
                    self.declare_items(module_declaration, root_path.with_part(path));
                }
                types::ItemKind::Import(_) | types::ItemKind::Struct(_) => {}
            }
        }
    }

    fn get_or_create_module(&mut self, module_path: types::FQName) -> &mut CompiledModule<'ctx> {
        self.context
            .global_scope
            .get_or_create_module(module_path, || {
                self.context
                    .llvm_context
                    .create_module(module_path.into_mangled().as_str())
            })
    }

    fn compile_function(&mut self, handle: &FunctionHandle) -> Result<(), CompileError> {
        let types::functions::FunctionBody::Statements(statements) = &handle.body else {
            return Ok(());
        };

        let module_path = handle.module_name;
        self.get_or_create_module(module_path);
        let module = self.context.global_scope.get_module(module_path).unwrap();

        // TODO in reality, at this point we should already have the handle instantiated, do this
        // earlier!
        let mut compiled_function = module.begin_compile_function(
            handle.instantiate(&types::TypeArgumentValues::new_empty()),
            &self.context,
        );

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
        statements: &Vec<types::Statement>,
        module_path: types::FQName,
        compiled_function: &mut CompiledFunction<'ctx>,
    ) -> Result<(), CompileError> {
        for statement in statements {
            let self_value = compiled_function
                .scope
                .get_value(types::Identifier::parse("self"));

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
        expression: &types::Expression,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: types::FQName,
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
                        CompilerContext::get_std_type("u64"),
                        self.context.const_u64(*value).as_basic_value_enum(),
                    ),
                )),
            },
            types::ExpressionKind::VariableAccess(name) => {
                Ok((None, compiled_function.scope.get_value(*name).unwrap()))
            }
            types::ExpressionKind::StructConstructor(name) => {
                // TODO ensure the struct is instantiated in the context
                // TODO get the type argument values from the expression!
                let field_values = HashMap::new();
                // TODO actually set the field values!

                let value = self
                    .context
                    .global_scope
                    .structs
                    .inspect_instantiated(name, |s| {
                        s.unwrap().build_heap_instance(
                            &self.context,
                            &unique_name(&[&name.to_string()]),
                            field_values,
                        )
                    });

                let rc = RcValue::build_init(
                    &unique_name(&[&name.to_string(), "rc"]),
                    &StructInstance::new(value, name.clone()),
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
        module_path: types::FQName,
        position: ast::SourceSpan,
        target: &types::Expression,
        arguments: &[types::Expression],
    ) -> Result<Value<'ctx>, CompileError> {
        let compiled_target =
            self.compile_expression(target, self_.cloned(), compiled_function, module_path)?;
        let self_value = compiled_target.0;

        let function = match &compiled_target.1 {
            Value::Function(function_handle) => function_handle,
            Value::Empty => todo!(),
            Value::Primitive(_, _) => todo!(),
            Value::Reference(_) => todo!(),
            Value::Struct(_) => todo!(),
        };

        // TODO the instantiation should be done earlier
        let function = function.instantiate(&types::TypeArgumentValues::new_empty());

        if !self
            .context
            .global_scope
            .get_module(function.module_name)
            .unwrap()
            .has_function(&function.id.into_mangled())
        {
            self.compile_function(&function).unwrap();
        }

        let compiled_target_function = self
            .context
            .global_scope
            .get_module(module_path)
            .unwrap()
            .get_or_create_function(&function, &self.context);

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
            // TODO treat FunctionId as opaque, remove this match
            .map_err(|e| e.at(position))?;
        let call_result = call_result.as_any_value_enum();
        let value = match function.return_type.kind() {
            types::TypeKind::Unit => Value::Empty,
            types::TypeKind::Object { type_name: id } => Value::Reference(RcValue::from_pointer(
                call_result.into_pointer_value(),
                id.clone(),
            )),
            types::TypeKind::Array { .. } => todo!(),
            types::TypeKind::Callable { .. } => todo!(),
            types::TypeKind::U64 => todo!(),
            types::TypeKind::U8 => todo!(),
            types::TypeKind::Pointer(_) => todo!(),
            types::TypeKind::Generic(_) => todo!(),
        };
        Ok(value)
    }
}
