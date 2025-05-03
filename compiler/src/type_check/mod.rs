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
use errors::TypeCheckError;

use crate::{ast, types};

impl types::Item {
    fn type_(&self, root_module: &DeclaredModule) -> types::Type {
        match &self.kind {
            types::ItemKind::Function(function) => function.type_(),
            types::ItemKind::Struct(struct_) => {
                types::Type::StructDescriptor(types::StructDescriptorType {
                    name: struct_.name,
                    fields: struct_.fields.clone(),
                })
            }
            types::ItemKind::Import(import) => root_module
                .get_item(import.imported_item)
                .unwrap()
                .type_(root_module),
            types::ItemKind::Module(_) => todo!(),
        }
    }
}

pub fn type_check(
    program: &[ast::SourceFile],
    std: Option<&types::RootModule>,
) -> Result<types::RootModule, TypeCheckError> {
    let mut root_module_declaration = DeclaredModule::new();

    if let Some(std) = std {
        match std {
            types::RootModule::App { .. } => todo!(),
            types::RootModule::Library { module } => {
                root_module_declaration.import_predeclared(module);
            }
        }
    }

    let type_checker = DeclarationChecker::new(root_module_declaration);

    let definition_checker = type_checker.check(program)?;
    definition_checker.check()
}
