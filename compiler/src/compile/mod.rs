pub mod builtins;
mod compilers;
mod context;
pub(crate) mod errors;
pub(crate) mod mangler;
mod module;
pub(crate) mod scope;
mod value;

use std::{collections::HashMap, rc::Rc};

use builtins::{
    array,
    rc::{self, RcValue},
    string,
};
use compilers::expression::ExpressionCompiler;
use context::{AllItems, Builtins, CompilerContext};
use errors::CompileError;
use inkwell::{
    basic_block::BasicBlock, context::Context, execution_engine::ExecutionEngine, module::Module,
};
use mangler::{MangledIdentifier, mangle_module_id, mangle_type};
use module::CompiledModule;
use rand::Rng;
use scope::{GlobalScope, Scope};
use value::Value;

use crate::{
    identifier::Identifier,
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
        items: std_items,
    } = std
    else {
        todo!();
    };

    let compiler = Compiler::new(
        &context,
        Some(std_scope),
        Some(std_modules),
        Some(std_items),
    );

    let compiled_root_module = compiler.compile(program);
    let root_module = context.create_module("root");

    let CompiledRootModule::App {
        scope: _,
        main,
        modules,
        items: _,
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
        scope: Rc<Scope<'ctx>>,
        create_llvm_module: impl FnOnce() -> Module<'ctx>,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .entry(path)
            .or_insert_with(|| CompiledModule::new(scope, create_llvm_module()))
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
    items: AllItems<'ctx>,
    modules: Modules<'ctx>,
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
        items: AllItems<'ctx>,
        main: MangledIdentifier,
    },
    Library {
        scope: GlobalScope<'ctx>,
        items: AllItems<'ctx>,
        modules: Modules<'ctx>,
    },
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(
        context: &'ctx Context,
        std_scope: Option<GlobalScope<'ctx>>,
        std_modules: Option<Modules<'ctx>>,
        std_items: Option<AllItems<'ctx>>,
    ) -> Self {
        let builtins = Builtins {
            rc_handle: rc::describe_structure(),
        };

        let global_scope = std_scope.unwrap_or_else(GlobalScope::new);
        let modules = std_modules.unwrap_or_else(Modules::new);
        let items = std_items.unwrap_or_else(|| {
            // If there is no std, then we want to get in early and create the builtins, so that
            // the rest of the compilation can just add methods, etc. to them

            let mut structs = HashMap::new();

            structs.insert(*TYPE_NAME_STRING, string::describe_structure());
            structs.insert(*TYPE_NAME_ARRAY, array::describe_structure());

            AllItems::new(structs, HashMap::new())
        });

        Self {
            context: CompilerContext {
                llvm_context: context,
                builder: context.create_builder(),
                builtins,
            },
            global_scope,
            modules,
            items,
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

        self.items = AllItems::new(structs, functions);
        self.items
            .get_or_instantiate_struct(&InstantiatedStructId::new(
                *TYPE_NAME_U64,
                types::TypeArgumentValues::new_empty(),
            ));

        for module in program.modules().keys() {
            self.get_or_create_module(*module);
        }

        if let Some(main) = main {
            let main = self
                .items
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
                items: self.items,
                main: mangled_main,
            }
        } else {
            CompiledRootModule::Library {
                modules: self.modules,
                scope: self.global_scope,
                items: self.items,
            }
        }
    }

    fn get_or_create_module(
        &mut self,
        module_path: types::modules::ModuleId,
    ) -> &mut CompiledModule<'ctx> {
        self.modules
            .get_or_create(module_path, self.global_scope.child(), || {
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
            module.begin_compile_function(handle, &self.context, &mut self.items);

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

            let mut expression_compiler = ExpressionCompiler::new(self);

            match statement {
                types::Statement::Expression(expression) => {
                    expression_compiler.compile_expression(
                        expression,
                        self_value,
                        compiled_function,
                        module_path,
                    )?;
                }
                types::Statement::Let(let_) => {
                    let value = expression_compiler.compile_expression(
                        &let_.value,
                        self_value,
                        compiled_function,
                        module_path,
                    )?;

                    compiled_function.scope.set_value(let_.binding, value.1);
                }
                types::Statement::Return(expression) => {
                    let value = expression_compiler.compile_expression(
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
}
