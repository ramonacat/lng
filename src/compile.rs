use std::{
    collections::HashMap,
    ffi::{c_char, CStr},
    rc::Rc,
};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicType,
    values::{AnyValue, BasicMetadataValueEnum, BasicValue},
    AddressSpace,
};

use crate::{
    ast::{Expression, FunctionBody, Statement},
    type_check::Program,
};

#[no_mangle]
pub extern "C" fn lng_println(arg: *const c_char) {
    let arg = unsafe { CStr::from_ptr(arg) };
    println!("PRINTLN CALLED: {arg:?}");
}

pub fn compile(program: &Program) {
    let context = Context::create();
    let builder = context.create_builder();

    let mut declared_modules: HashMap<String, Rc<Module>> = HashMap::new();

    for (i, file) in program.0.iter().enumerate() {
        let module_name = format!("mod{i}");
        let module = Rc::new(context.create_module(&module_name)); // todo use the filename or smth
        declared_modules.insert(module_name, module.clone());

        for declared_function in &file.functions {
            let arguments = declared_function
                .arguments
                .iter()
                .map(|arg| {
                    type_to_llvm(&arg.type_, &context)
                        .as_basic_type_enum()
                        .into()
                })
                .collect::<Vec<_>>();

            let function_type = context.void_type().fn_type(&arguments[..], false);
            module.add_function(&declared_function.name, function_type, None);
        }
    }

    for (i, file) in program.0.iter().enumerate() {
        let module_name = format!("mod{i}");
        let module = declared_modules.get(&module_name).unwrap();

        for declared_function in &file.functions {
            let FunctionBody::Statements(statements) = &declared_function.body else {
                continue;
            };

            let function = module.get_function(&declared_function.name).unwrap();
            let function_block = context.append_basic_block(function, "entry");

            builder.position_at_end(function_block);

            for statement in statements {
                match statement {
                    Statement::Expression(expression) => {
                        compile_expression(expression, &context, module, &builder);
                    }
                }
            }

            builder.build_return(None).unwrap();
        }
    }

    let module = declared_modules.get("mod0").unwrap();
    module.verify().unwrap();

    let execution_engine = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
        .unwrap();

    execution_engine.add_global_mapping(
        &module.get_function("lng_println").unwrap(),
        (lng_println as *const extern "C" fn()) as usize,
    );

    unsafe {
        let main = execution_engine
            .get_function::<unsafe extern "C" fn()>("main")
            .unwrap();
        main.call();
    }
}

fn type_to_llvm<'a>(type_: &crate::ast::Type, context: &'a Context) -> Box<dyn BasicType<'a> + 'a> {
    match type_ {
        crate::ast::Type::Void => panic!("Cannot pass void arguments!"),
        // todo arrays should be structs that have bounds, not just ptrs
        crate::ast::Type::Array(_) => Box::new(context.ptr_type(AddressSpace::default())),
        crate::ast::Type::Named(n) => match n.as_str() {
            // todo string should be some form of a struct
            "string" => Box::new(context.ptr_type(AddressSpace::default())),
            _ => todo!(),
        },
    }
}

fn compile_expression<'a>(
    expression: &Expression,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> BasicMetadataValueEnum<'a> {
    match expression {
        crate::ast::Expression::FunctionCall { name, arguments } => {
            let function = module.get_function(name).unwrap();

            let call_arguments = arguments
                .iter()
                .map(|a| compile_expression(a, context, module, builder))
                .collect::<Vec<_>>();
            let call_result = builder.build_call(function, &call_arguments, name).unwrap();

            (call_result)
                .as_any_value_enum()
                .try_into()
                .unwrap_or(context.i8_type().const_zero().as_basic_value_enum().into())
        }
        crate::ast::Expression::Literal(literal) => {
            match literal {
                crate::ast::Literal::String(s) => {
                    let mut string_bytes = s.as_bytes().to_vec();
                    string_bytes.push(0);

                    let string_type = context.i8_type().array_type(string_bytes.len() as u32 + 1);

                    // FIXME use a globally unique name for the global
                    let global = module.add_global(string_type, None, "str0");
                    global.set_initializer(&context.const_string(&string_bytes[..], true));

                    (global).as_pointer_value().as_basic_value_enum().into()
                }
            }
        }
    }
}
