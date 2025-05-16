use compiler_derive::BuiltinStruct;

#[derive(BuiltinStruct)]
#[module_id("std")]
#[struct_name("vtable_entry")]
#[generic(TPointee)]
#[repr(C)]
pub struct VTableEntry {
    type_store_id: u64,
    type_id: u64,

    callback: *const fn(),
}
