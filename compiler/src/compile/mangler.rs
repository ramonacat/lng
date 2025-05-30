use crate::identifier::Identifier;
use crate::types;
use std::fmt::Write;

pub(super) fn mangle_type(type_: &types::Type) -> MangledIdentifier {
    match type_.kind() {
        types::TypeKind::Unit => todo!(),
        types::TypeKind::Object { .. } => todo!(),
        types::TypeKind::Array { .. } => todo!(),
        types::TypeKind::Callable(function_id) => match function_id {
            types::functions::FunctionId::InModule(module_id, identifier) => mangle_item_name(
                *module_id,
                *identifier,
                &types::generics::TypeArguments::new_empty(),
                IdentifierKind::Function(*function_id),
            ),
            types::functions::FunctionId::InStruct(struct_id, identifier) => {
                mangle_struct_item_name(
                    *struct_id,
                    *identifier,
                    &types::generics::TypeArguments::new_empty(),
                )
            }
        },
        types::TypeKind::U64 => todo!(),
        types::TypeKind::U8 => todo!(),
        types::TypeKind::Pointer(_) => todo!(),
        types::TypeKind::Struct(_) => todo!(),
        types::TypeKind::IndirectCallable(_, _) => todo!(),
        types::TypeKind::InterfaceObject { .. } => todo!(),
        types::TypeKind::Generic(_) => todo!(),
        types::TypeKind::Interface(_) => todo!(),
    }
}

pub(super) fn mangle_module_id(id: types::modules::ModuleId) -> MangledIdentifier {
    MangledIdentifier {
        mangled: id.to_string().replace('.', "$"),
        source: IdentifierKind::ModuleId(id),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    ModuleId(types::modules::ModuleId),
    Identifier(Identifier),
    StructField(types::structs::StructId, Identifier),
    Function(types::functions::FunctionId),
    Struct(types::structs::StructId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MangledIdentifier {
    mangled: String,
    source: IdentifierKind,
}

impl MangledIdentifier {
    pub(crate) const fn as_str(&self) -> &str {
        self.mangled.as_str()
    }

    pub(crate) fn new_raw(identifier: Identifier) -> Self {
        Self {
            mangled: identifier.raw(),
            source: IdentifierKind::Identifier(identifier),
        }
    }
}

fn mangle_type_argument_values(tav: &types::generics::TypeArguments) -> String {
    let mut result = String::new();

    for argument in tav.arguments() {
        write!(
            result,
            "{}$_${}",
            argument.name(),
            argument.value().unwrap()
        )
        .unwrap();
    }

    result
}

fn mangle_item_name(
    module: types::modules::ModuleId,
    item: Identifier,
    tav: &types::generics::TypeArguments,
    source: IdentifierKind,
) -> MangledIdentifier {
    MangledIdentifier {
        mangled: mangle_module_id(module).mangled
            + "$$"
            + &item.raw()
            + "$$$"
            + &mangle_type_argument_values(tav),
        source,
    }
}

fn mangle_struct_item_name(
    struct_id: types::structs::StructId,
    item: Identifier,
    tav: &types::generics::TypeArguments,
) -> MangledIdentifier {
    MangledIdentifier {
        mangled: mangle_struct_id(struct_id, tav).mangled + "$$" + &item.raw() + "$$$",
        source: IdentifierKind::StructField(struct_id, item),
    }
}

fn mangle_struct_id(
    struct_id: types::structs::StructId,
    tav: &types::generics::TypeArguments,
) -> MangledIdentifier {
    match struct_id {
        types::structs::StructId::InModule(module_id, identifier) => mangle_item_name(
            module_id,
            identifier,
            tav,
            IdentifierKind::Struct(struct_id),
        ),
    }
}
