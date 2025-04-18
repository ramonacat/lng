use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::BasicType,
    values::{AnyValue, BasicMetadataValueEnum, BasicValue, FunctionValue},
    AddressSpace,
};

use crate::{
    ast::{Expression, Function, FunctionBody, Statement},
    stdlib::register_mappings,
    type_check::Program,
};

struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    declared_modules: HashMap<String, Module<'ctx>>,
}

pub fn compile(program: &Program) {
    let context = Context::create();
    let compiler = Compiler::new(&context);

    compiler.compile(program);
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

    pub fn compile(mut self, program: &Program) {
        let mut function_declarations: HashMap<String, HashMap<String, Function>> = HashMap::new();

        for file in &program.0 {
            let module = self.context.create_module(&file.name);
            function_declarations.insert(file.name.clone(), HashMap::new());

            for declaration in &file.declarations {
                match declaration {
                    crate::ast::Declaration::Function(function) => {
                        function_declarations
                            .get_mut(&file.name)
                            .unwrap()
                            .insert(function.name.clone(), function.clone());

                        self.declare_function(function, &module);
                    }
                };
            }

            self.declared_modules.insert(file.name.clone(), module);
        }

        for file in &program.0 {
            let module = self.declared_modules.get(&file.name).unwrap();

            for import in &file.imports {
                // TODO support multi-level module hierarchies
                let (import_module, import_function) =
                    (import.path[0].clone(), import.path[1].clone());

                self.import_function(
                    function_declarations
                        .get(&import_module)
                        .unwrap()
                        .get(&import_function)
                        .unwrap(),
                    module,
                );
            }

            for declaration in &file.declarations {
                match declaration {
                    crate::ast::Declaration::Function(function) => {
                        let FunctionBody::Statements(statements) = &function.body else {
                            continue;
                        };

                        let function = module.get_function(&function.name).unwrap();
                        let function_block = self.context.append_basic_block(function, "entry");

                        self.builder.position_at_end(function_block);

                        for statement in statements {
                            match statement {
                                Statement::Expression(expression) => {
                                    self.compile_expression(expression, module);
                                }
                            }
                        }

                        self.builder.build_return(None).unwrap();
                    }
                }
            }
        }

        let root_module = self.context.create_module("root");
        for module in self.declared_modules {
            root_module.link_in_module(module.1).unwrap();
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
    }

    fn resolve_function(&self, name: &str, module: &Module<'ctx>) -> Option<FunctionValue<'ctx>> {
        module.get_function(name)
    }

    fn compile_expression(
        &self,
        expression: &Expression,
        module: &Module<'ctx>,
    ) -> BasicMetadataValueEnum {
        match expression {
            crate::ast::Expression::FunctionCall { name, arguments } => {
                let function = self.resolve_function(name, module).unwrap();

                let call_arguments = arguments
                    .iter()
                    .map(|a| self.compile_expression(a, module))
                    .collect::<Vec<_>>();
                let call_result = self
                    .builder
                    .build_call(function, &call_arguments, name)
                    .unwrap();

                call_result.as_any_value_enum().try_into().unwrap_or(
                    self.context
                        .i8_type()
                        .const_zero()
                        .as_basic_value_enum()
                        .into(),
                )
            }
            crate::ast::Expression::Literal(literal) => {
                match literal {
                    crate::ast::Literal::String(s) => {
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

                        global.as_pointer_value().as_basic_value_enum().into()
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
