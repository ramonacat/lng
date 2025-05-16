use inkwell::{execution_engine::ExecutionEngine, module::Module};

use crate::compile::builtins::{rc::BuiltinRc, string::BuiltinString};

impl std::fmt::Display for BuiltinString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let slice = unsafe {
            std::slice::from_raw_parts(
                self.characters.cast(),
                usize::try_from(self.length).unwrap(),
            )
        };

        let string = String::from_utf8(slice.to_vec()).unwrap();
        write!(f, "{string}")
    }
}

#[unsafe(no_mangle)]
extern "C" fn println(arg: *const BuiltinRc<BuiltinString>) {
    let pointee = unsafe { &*(*arg).pointee };

    println!("{pointee}");
}

// TODO this is a bit cursed, as the allocator here might be different from the one that's used by
// the jitted code, so when it's freed in the jitted code, it is UB. We should find a way to move
// the allocations to the other side! Maybe just implement the function natively? :)
extern "C" fn u64_to_string(arg: u64) -> *const BuiltinRc<BuiltinString> {
    let bytes = arg.to_string().into_bytes();
    let length = bytes.len();

    let characters = Box::leak(Box::new(bytes)).as_ptr();
    let lng_string = BuiltinString {
        characters: characters.cast(),
        length: length as u64,
    };
    let lng_rc = BuiltinRc {
        refcount: 1,
        pointee: Box::leak(Box::new(lng_string)),
        // TODO we should point at the actual descriptor!
        type_descriptor: std::ptr::null(),
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
