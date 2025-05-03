use std::{cell::RefCell, collections::HashMap};

use compiler::{
    compile::compile,
    parse::parse_file,
    std::{
        runtime::{LngRc, LngString},
        type_check_std,
    },
    type_check::type_check,
};
use inkwell::{execution_engine::ExecutionEngine, module::Module};

thread_local! {
    static TEST_RESUTLS: RefCell<String> = const { RefCell::new(String::new()) };
}

#[unsafe(no_mangle)]
extern "C" fn test_println(arg: *const LngRc<LngString>) {
    let arg = unsafe { &*(*arg).pointee }.to_string();

    TEST_RESUTLS.with_borrow_mut(|results| {
        results.push_str(&arg);
        results.push('\n');
    })
}

fn register_test_mappings(engine: &ExecutionEngine, module: &Module) {
    compiler::std::runtime::register_mappings(engine, module);

    if let Some(println_handle) = module.get_function("println_impl") {
        engine.add_global_mapping(
            &println_handle,
            test_println as *const extern "C" fn() as usize,
        );
    }
}

pub fn run(program: HashMap<&str, &str>) -> String {
    let asts = program
        .into_iter()
        .map(|(name, contents)| parse_file(name, contents).unwrap())
        .collect::<Vec<_>>();

    let std_program = type_check_std().unwrap();
    let type_check_result = type_check(&asts, Some(&std_program)).unwrap();

    TEST_RESUTLS.with_borrow_mut(|results| results.clear());

    compile(
        &type_check_result,
        &std_program,
        Some(Box::new(register_test_mappings)),
    )
    .unwrap();

    let mut results = String::new();

    TEST_RESUTLS.with_borrow_mut(|x| std::mem::swap(&mut results, x));

    results
}
