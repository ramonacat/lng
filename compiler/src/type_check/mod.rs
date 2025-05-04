mod declaration_checker;
mod declarations;
mod definition_checker;
pub mod errors;

use declaration_checker::DeclarationChecker;
use declarations::{
    DeclaredArgument, DeclaredAssociatedFunction, DeclaredFunction, DeclaredFunctionDefinition,
    DeclaredImport, DeclaredItem, DeclaredItemKind, DeclaredModule, DeclaredStruct,
    DeclaredStructField,
};
use errors::{TypeCheckError, TypeCheckErrorDescription};

use crate::{ast, errors::ErrorLocation, types};

impl types::Item {
    fn type_(
        &self,
        root_module: &DeclaredModule,
        error_location: ErrorLocation,
    ) -> Result<types::Type, TypeCheckError> {
        // TODO handle possible generics here
        match &self.kind {
            types::ItemKind::Function(function) => Ok(function.type_()),
            types::ItemKind::Struct(struct_) => Ok(types::Type::new_not_generic(
                types::TypeKind::StructDescriptor(types::StructDescriptorType {
                    name: struct_.name,
                    fields: struct_.fields.clone(),
                }),
            )),
            types::ItemKind::Import(import) => root_module
                .get_item(import.imported_item)
                .ok_or_else(|| {
                    TypeCheckErrorDescription::ItemDoesNotExist(import.imported_item)
                        .at(error_location)
                })?
                .type_(root_module, error_location),
            types::ItemKind::Module(_) => todo!(),
        }
    }
}

pub fn type_check(
    program: &[ast::SourceFile],
    std: Option<&types::RootModule>,
) -> Result<types::RootModule, TypeCheckError> {
    let mut root_module_declaration = DeclaredModule::new();

    if let Some(std) = std.map(types::RootModule::root_module) {
        root_module_declaration.import_predeclared(std);
    }

    let type_checker = DeclarationChecker::new(root_module_declaration);

    let definition_checker = type_checker.check(program)?;
    definition_checker.check()
}
