use std::sync::LazyLock;

use inkwell::context::Context;

use crate::{
    compile::{CompileError, Compiler, scope::GlobalScope},
    parse::parse_file,
    type_check::{TypeCheckError, type_check},
    types,
};

pub(crate) static TYPE_NAME_U64: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.u64"));
pub(crate) static TYPE_NAME_STRING: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.string"));

pub fn type_check_std() -> Result<types::Module, TypeCheckError> {
    let asts = vec![parse_file("std", include_str!("../../stdlib/std.lng")).unwrap()];

    type_check(&asts, None)
}

pub fn compile_std<'ctx>(
    program: &types::Module,
    context: &'ctx Context,
) -> Result<GlobalScope<'ctx>, CompileError> {
    let compiler = Compiler::new(context, None);

    compiler.compile(program)
}
