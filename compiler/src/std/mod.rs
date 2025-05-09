pub mod runtime;

use std::sync::LazyLock;

use inkwell::context::Context;

use crate::{
    ast,
    compile::{CompileError, CompiledRootModule, Compiler},
    identifier::FQName,
    type_check::{errors::TypeCheckError, type_check},
    types,
};

pub(crate) static TYPE_NAME_U64: LazyLock<FQName> = LazyLock::new(|| FQName::parse("std.u64"));
pub(crate) static TYPE_NAME_STRING: LazyLock<FQName> =
    LazyLock::new(|| FQName::parse("std.string"));

pub fn type_check_std() -> Result<types::RootModule, TypeCheckError> {
    let asts = vec![crate::parser::parse_file(
        ast::SourceFileName::new("std".to_string()),
        include_str!("../../stdlib/std.lng"),
    )];

    type_check(&asts, None)
}

pub fn compile_std<'ctx>(
    program: &types::RootModule,
    context: &'ctx Context,
) -> Result<CompiledRootModule<'ctx>, CompileError> {
    let compiler = Compiler::new(context, None);

    compiler.compile(program)
}
