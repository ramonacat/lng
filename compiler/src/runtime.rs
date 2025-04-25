use std::{
    ffi::{CStr, c_char},
    fmt::Debug,
};

use inkwell::{execution_engine::ExecutionEngine, module::Module};

// keep in sync with stdlib `string`!!!
#[repr(C)]
#[derive(Debug)]
pub struct LngString {
    pub contents: *const c_char,
}

// keep in sync with the definiton in compiler
#[repr(C)]
#[derive(Debug)]
pub struct LngRc<T: Debug> {
    pub refcount: u64,
    pub pointee: *const T,
}

#[unsafe(no_mangle)]
extern "C" fn println(arg: *const LngRc<LngString>) {
    let arg = unsafe { CStr::from_ptr((*(*arg).pointee).contents) }
        .to_str()
        .unwrap();

    println!("{}", arg);
}

pub fn register_mappings(execution_engine: &ExecutionEngine, module: &Module) {
    if let Some(println_handle) = module.get_function("println_impl") {
        execution_engine
            .add_global_mapping(&println_handle, println as *const extern "C" fn() as usize);
    }
}
