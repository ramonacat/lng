use std::{
    ffi::{c_char, CStr},
    fmt::Debug,
};

use inkwell::{execution_engine::ExecutionEngine, module::Module};

// keep in sync with stdlib `string`!!!
#[repr(C)]
#[derive(Debug)]
pub struct LngString {
    contents: *const c_char,
}

// keep in sync with the definiton in compiler
#[repr(C)]
#[derive(Debug)]
pub struct LngRc<T: Debug> {
    refcount: u64,
    pointee: *const T,
}

#[no_mangle]
pub extern "C" fn println(arg: *const LngRc<LngString>) {
    eprintln!("{:?}", unsafe { &*arg });

    let arg = unsafe { CStr::from_ptr((*(*arg).pointee).contents) }
        .to_str()
        .unwrap();
    println!("{}", arg);
}

pub fn register_mappings(execution_engine: &ExecutionEngine, module: &Module) {
    if let Some(println_handle) = module.get_function("println") {
        execution_engine
            .add_global_mapping(&println_handle, println as *const extern "C" fn() as usize);
    }
}
