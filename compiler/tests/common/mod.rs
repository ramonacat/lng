use std::{
    collections::HashMap,
    ffi::CStr,
    sync::{Mutex, OnceLock},
};

use compiler::{
    compile::compile,
    parse::parse_file,
    runtime::{LngRc, LngString},
    type_check::{Program, type_check},
};
use inkwell::{execution_engine::ExecutionEngine, module::Module};

static TEST_RESUTLS: OnceLock<Mutex<String>> = OnceLock::new();

#[unsafe(no_mangle)]
extern "C" fn test_println(arg: *const LngRc<LngString>) {
    let arg = unsafe { CStr::from_ptr((*(*arg).pointee).contents) }
        .to_str()
        .unwrap();

    let mut results = TEST_RESUTLS
        .get_or_init(|| Mutex::new(String::new()))
        .lock()
        .unwrap();

    results.push_str(arg);
    results.push('\n');
}

fn register_test_mappings(engine: &ExecutionEngine, module: &Module) {
    compiler::runtime::register_mappings(engine, module);

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

    let program = Program(asts);
    let type_check_result = type_check(program).unwrap();

    TEST_RESUTLS
        .get_or_init(|| Mutex::new(String::new()))
        .lock()
        .unwrap()
        .clear();

    compile(type_check_result, Some(Box::new(register_test_mappings))).unwrap();

    TEST_RESUTLS
        .get_or_init(|| Mutex::new(String::new()))
        .lock()
        .unwrap()
        .clone()
}
