pub mod runtime;

use std::sync::LazyLock;

use inkwell::context::Context;

use crate::{
    compile::{CompileError, CompiledRootModule, Compiler},
    parse::parse_file,
    type_check::{errors::TypeCheckError, type_check},
    types,
};

pub(crate) static TYPE_NAME_U64: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.u64"));
pub(crate) static TYPE_NAME_UNIT: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.unit"));
pub(crate) static TYPE_NAME_STRING: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.string"));

pub fn type_check_std() -> Result<types::RootModule, TypeCheckError> {
    let asts = vec![parse_file("std", include_str!("../../stdlib/std.lng")).unwrap()];

    type_check(&asts, None)
}

pub fn compile_std<'ctx>(
    program: &types::RootModule,
    context: &'ctx Context,
) -> Result<CompiledRootModule<'ctx>, CompileError> {
    let compiler = Compiler::new(context, None);

    compiler.compile(program)
}
