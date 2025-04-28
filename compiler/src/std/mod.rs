use std::sync::LazyLock;

use inkwell::context::Context;

use crate::{
    compile::{CompileError, Compiler, scope::GlobalScope},
    parse::parse_file,
    type_check::{self, TypeCheckError, type_check},
    types,
};

pub(crate) static MODULE_PATH_STD: LazyLock<types::ModulePath> =
    LazyLock::new(|| types::ModulePath::parse("std"));
pub(crate) static TYPE_NAME_U64: LazyLock<types::Identifier> =
    LazyLock::new(|| types::Identifier::parse("u64"));
pub(crate) static TYPE_NAME_STRING: LazyLock<types::Identifier> =
    LazyLock::new(|| types::Identifier::parse("string"));

pub fn type_check_std() -> Result<types::Program, TypeCheckError> {
    let asts = vec![parse_file("std", include_str!("../../stdlib/std.lng")).unwrap()];

    type_check(&type_check::Program(asts), None)
}

pub fn compile_std<'ctx>(
    program: &types::Program,
    context: &'ctx Context,
) -> Result<GlobalScope<'ctx>, CompileError> {
    let compiler = Compiler::new(context, None);

    compiler.compile(program)
}
