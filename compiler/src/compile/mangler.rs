use super::scope::GlobalScope;
use crate::types;
use crate::{
    identifier::Identifier,
    types::{InstantiatedType, TypeArgumentValues, modules::ModuleId, structs::StructId},
};
use std::fmt::Write;

pub(super) fn mangle_type(
    type_: &types::InstantiatedType,
    global_scope: &GlobalScope,
) -> MangledIdentifier {
    match type_.kind() {
        types::InstantiatedTypeKind::Unit => todo!(),
        types::InstantiatedTypeKind::Object { .. } => todo!(),
        types::InstantiatedTypeKind::Array { .. } => todo!(),
        types::InstantiatedTypeKind::Callable(_) => todo!(),
        types::InstantiatedTypeKind::U64 => todo!(),
        types::InstantiatedTypeKind::U8 => todo!(),
        types::InstantiatedTypeKind::Pointer(_) => todo!(),
        types::InstantiatedTypeKind::Struct(_) => todo!(),
        // TODO can we stop accessing global_scope here somehow? maybe let the compiler decide
        // whether or not to mangle?
        types::InstantiatedTypeKind::Function(instantiated_function_id) => global_scope
            .structs
            .get_function(instantiated_function_id.id())
            .map(|function| match function.body {
                types::functions::FunctionBody::Extern(identifier) => {
                    nomangle_identifier(identifier)
                }
                types::functions::FunctionBody::Statements(_) => match function.id {
                    types::functions::FunctionId::InModule(module_id, fqname) => mangle_item_name(
                        module_id,
                        fqname,
                        instantiated_function_id.argument_values(),
                        IdentifierKind::Function(function.id),
                    ),
                    types::functions::FunctionId::InStruct(struct_id, identifier) => {
                        mangle_struct_item_name(
                            struct_id,
                            identifier,
                            instantiated_function_id.argument_values(),
                        )
                    }
                },
            })
            .unwrap(),
        types::InstantiatedTypeKind::IndirectCallable(_, _) => todo!(),
        types::InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
    }
}

pub(super) fn mangle_module_id(id: ModuleId) -> MangledIdentifier {
    MangledIdentifier {
        mangled: id.to_string().replace('.', "$"),
        source: IdentifierKind::ModuleId(id),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IdentifierKind {
    ModuleId(ModuleId),
    Identifier(Identifier),
    StructField(types::structs::StructId, Identifier),
    Function(types::functions::FunctionId),
    Struct(StructId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MangledIdentifier {
    mangled: String,
    source: IdentifierKind,
}

impl MangledIdentifier {
    pub(crate) fn as_str(&self) -> &str {
        self.mangled.as_str()
    }
}

fn mangle_type_argument_values(tav: &TypeArgumentValues<InstantiatedType>) -> String {
    let mut result = String::new();

    for (id, value) in &tav.0 {
        write!(result, "{id}$_${value}").unwrap();
    }

    result
}

fn mangle_item_name(
    module: ModuleId,
    item: Identifier,
    tav: &TypeArgumentValues<InstantiatedType>,
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
    struct_id: StructId,
    item: Identifier,
    tav: &TypeArgumentValues<InstantiatedType>,
) -> MangledIdentifier {
    MangledIdentifier {
        mangled: mangle_struct_id(struct_id, tav).mangled + "$$" + &item.raw() + "$$$",
        source: IdentifierKind::StructField(struct_id, item),
    }
}

fn mangle_struct_id(
    struct_id: StructId,
    tav: &TypeArgumentValues<InstantiatedType>,
) -> MangledIdentifier {
    match struct_id {
        StructId::InModule(module_id, identifier) => mangle_item_name(
            module_id,
            identifier,
            tav,
            IdentifierKind::Struct(struct_id),
        ),
    }
}

fn nomangle_identifier(identifier: Identifier) -> MangledIdentifier {
    MangledIdentifier {
        mangled: identifier.raw(),
        source: IdentifierKind::Identifier(identifier),
    }
}
