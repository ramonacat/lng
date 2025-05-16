use compiler_derive::BuiltinStruct;

use super::vtable_entry::VTableEntry;

#[derive(BuiltinStruct)]
#[module_id("std")]
#[struct_name("type_descriptor")]
#[generic(TPointee)]
#[repr(C)]
pub struct BuiltinTypeDescriptor {
    #[array]
    pub vtable: *const VTableEntry,
    pub vtable_size: u64,
}
