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

// TODO this is a bit cursed, as the allocator here might be different from the one that's used by
// the jitted code, so when it's freed in the jitted code, it is UB. We should find a way to move
// the allocations to the other side! Maybe just implement the function natively? :)
extern "C" fn u64_to_string(arg: u64) -> *const LngRc<LngString> {
    let mut bytes = arg.to_string().into_bytes();
    bytes.push(0);
    let characters = Box::leak(Box::new(bytes)).as_ptr();
    let lng_string = LngString {
        contents: characters as *const i8,
    };
    let lng_rc = LngRc {
        refcount: 1,
        pointee: Box::leak(Box::new(lng_string)),
    };

    Box::leak(Box::new(lng_rc))
}

pub fn register_mappings(execution_engine: &ExecutionEngine, module: &Module) {
    if let Some(println_handle) = module.get_function("println_impl") {
        execution_engine
            .add_global_mapping(&println_handle, println as *const extern "C" fn() as usize);
    }

    if let Some(u64_to_string_handle) = module.get_function("u64_to_string_impl") {
        execution_engine.add_global_mapping(
            &u64_to_string_handle,
            u64_to_string as *const extern "C" fn() as usize,
        );
    }
}
