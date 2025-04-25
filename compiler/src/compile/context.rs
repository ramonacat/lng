use inkwell::{builder::Builder, context::Context};

use super::StructHandle;

pub struct Builtins<'ctx> {
    pub rc_handle: StructHandle<'ctx>,
    pub string_handle: StructHandle<'ctx>,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins<'ctx>,
}
