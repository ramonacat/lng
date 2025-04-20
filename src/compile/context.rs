use inkwell::{builder::Builder, context::Context, types::StructType};

pub struct Builtins<'ctx> {
    pub rc_type: StructType<'ctx>,
    pub string_type: StructType<'ctx>,
}

pub struct CompilerContext<'ctx> {
    pub llvm_context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub builtins: Builtins<'ctx>,
}
