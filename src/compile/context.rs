use inkwell::{builder::Builder, context::Context, types::StructType};

use super::StructHandle;

// TODO remove _type, because it's already contained in the handle
pub struct Builtins<'ctx> {
    pub rc_type: StructType<'ctx>,
    pub rc_handle: StructHandle<'ctx>,
    pub string_type: StructType<'ctx>,
    pub string_handle: StructHandle<'ctx>,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins<'ctx>,
}
