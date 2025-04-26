use inkwell::context::Context;

use crate::{
    compile::{CompileError, Compiler, scope::GlobalScope},
    parse::parse_file,
    type_check::{self, TypeCheckError, type_check},
    types,
};

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
