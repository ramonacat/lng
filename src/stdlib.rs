use std::ffi::{c_char, CStr};

use inkwell::{execution_engine::ExecutionEngine, module::Module};

#[no_mangle]
pub extern "C" fn println(arg: *const c_char) {
    let arg = unsafe { CStr::from_ptr(arg) }.to_str().unwrap();
    println!("{}", arg);
}

pub fn register_mappings(execution_engine: &ExecutionEngine, module: &Module) {
    if let Some(println_handle) = module.get_function("println") {
        execution_engine
            .add_global_mapping(&println_handle, println as *const extern "C" fn() as usize);
    }
}
