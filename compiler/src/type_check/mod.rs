mod declaration_checker;
mod declarations;
mod definition_checker;
pub mod errors;

use declaration_checker::DeclarationChecker;
use declarations::{
    DeclaredFunction, DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule,
    DeclaredRootModule, DeclaredStructField,
};
use errors::{TypeCheckError, TypeCheckErrorDescription};

use crate::{
    ast,
    types::{self, structs::InstantiatedStructId},
};

impl types::Item {
    fn type_(
        &self,
        root_module: &DeclaredModule,
        current_module: types::FQName,
        error_location: ast::SourceSpan,
    ) -> Result<types::Type, TypeCheckError> {
        // TODO handle possible generics here
        match &self.kind {
            types::ItemKind::Function(function_id) => Ok(types::Type::new_not_generic(
                types::TypeKind::Callable(*function_id),
            )),
            types::ItemKind::Struct(struct_) => {
                Ok(types::Type::new_not_generic(types::TypeKind::Object {
                    type_name: InstantiatedStructId(
                        *struct_,
                        types::TypeArgumentValues::new_empty(),
                    ),
                }))
            }
            types::ItemKind::Import(import) => root_module
                .get_item(import.imported_item)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::ItemDoesNotExist(import.imported_item)
                        .at(error_location)
                })?
                .type_(root_module, current_module, error_location),
            types::ItemKind::Module(_) => todo!(),
        }
    }
}

pub fn type_check(
    program: &[ast::SourceFile],
    std: Option<&types::RootModule>,
) -> Result<types::RootModule, TypeCheckError> {
    let root_module_declaration = std.map_or_else(DeclaredRootModule::new, |std| {
        DeclaredRootModule::from_predeclared(
            std.root_module(),
            std.structs().clone(),
            std.functions().clone(),
        )
    });

    let type_checker = DeclarationChecker::new(root_module_declaration);

    let definition_checker = type_checker.check(program)?;
    definition_checker.check()
}
