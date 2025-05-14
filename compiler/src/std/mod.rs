pub mod runtime;

use std::sync::LazyLock;

use inkwell::context::Context;

use crate::{
    ast,
    compile::{CompiledRootModule, Compiler, errors::CompileError},
    identifier::Identifier,
    type_check::{errors::TypeCheckError, type_check},
    types,
};

pub(crate) static TYPE_NAME_U64: LazyLock<types::structs::StructId> = LazyLock::new(|| {
    types::structs::StructId::InModule(
        types::modules::ModuleId::parse("std"),
        Identifier::parse("u64"),
    )
});

pub(crate) static TYPE_NAME_STRING: LazyLock<types::structs::StructId> = LazyLock::new(|| {
    types::structs::StructId::InModule(
        types::modules::ModuleId::parse("std"),
        Identifier::parse("string"),
    )
});

pub fn type_check_std() -> Result<types::modules::RootModule, TypeCheckError> {
    let asts = vec![crate::parser::parse_file(
        ast::SourceFileName::new("std".to_string()),
        include_str!("../../stdlib/std.lng"),
    )];

    type_check(&asts, None)
}

pub fn compile_std<'ctx>(
    program: &types::modules::RootModule,
    context: &'ctx Context,
) -> Result<CompiledRootModule<'ctx>, CompileError> {
    let compiler = Compiler::new(context, None, None, None);

    Ok(compiler.compile(program))
}
