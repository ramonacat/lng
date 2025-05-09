mod declaration_checker;
mod declarations;
mod definition_checker;
pub mod errors;

use declaration_checker::DeclarationChecker;
use declarations::{DeclaredFunction, DeclaredRootModule, DeclaredStructField};
use errors::TypeCheckError;

use crate::{ast, types};

pub fn type_check(
    program: &[ast::SourceFile],
    std: Option<&types::modules::RootModule>,
) -> Result<types::modules::RootModule, TypeCheckError> {
    let root_module_declaration = std.map_or_else(DeclaredRootModule::new, |std| {
        DeclaredRootModule::from_predeclared(
            std.modules(),
            std.structs().clone(),
            std.functions().clone(),
        )
    });

    let type_checker = DeclarationChecker::new(root_module_declaration);

    let definition_checker = type_checker.check(program)?;

    definition_checker.check()
}
