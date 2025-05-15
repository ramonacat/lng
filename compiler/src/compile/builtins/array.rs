use std::{collections::HashMap, sync::LazyLock};

use compiler_derive::BuiltinStruct;
use inkwell::values::BasicValue;

use crate::{
    compile::{
        context::{AllItems, CompilerContext},
        unique_name,
        value::StructInstance,
    },
    identifier::Identifier,
    types::{self, generics::TypeArgument},
};

use super::rc::RcValue;

pub static TYPE_NAME_ARRAY: LazyLock<types::structs::StructId> = LazyLock::new(|| {
    types::structs::StructId::InModule(
        types::modules::ModuleId::parse("std"),
        Identifier::parse("array"),
    )
});
static ITEMS_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("items"));
static LENGTH_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("length"));
static CAPACITY_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("capacity"));

#[derive(BuiltinStruct)]
#[module_id("std")]
#[struct_name("array")]
#[generic(TItem)]
#[repr(C)]
pub struct BuiltinArray<TItem> {
    #[array]
    pub items: *const TItem,
    pub length: u64,
    pub capacity: u64,
}

pub struct ArrayValue {}

impl ArrayValue {
    pub fn build_instance<'ctx>(
        item_type: &types::Type,
        context: &CompilerContext<'ctx>,
        structs: &mut AllItems<'ctx>,
        types: &mut dyn types::store::TypeStore,
    ) -> RcValue<'ctx> {
        let items_type = context.make_object_type(item_type);
        // TODO add freeing of this array once destructors are in place
        let items = context
            .builder
            .build_array_malloc(
                items_type,
                context.const_u64(1),
                &unique_name(&["string_items"]),
            )
            .unwrap();
        let mut field_values = HashMap::new();
        field_values.insert(*ITEMS_FIELD, items.as_basic_value_enum());

        field_values.insert(*LENGTH_FIELD, context.const_u64(0).as_basic_value_enum());
        field_values.insert(*CAPACITY_FIELD, context.const_u64(1).as_basic_value_enum());

        let id = *TYPE_NAME_ARRAY;
        let item_type_id = types.add(item_type.clone());

        let array_value = structs
            .get_or_instantiate_struct(&types::structs::InstantiatedStructId::new(
                id,
                types::generics::TypeArguments::new(vec![TypeArgument::new_value(
                    Identifier::parse("TPointee"),
                    item_type_id,
                )]),
            ))
            .unwrap()
            .build_heap_instance(context, &unique_name(&["string"]), field_values, types);

        RcValue::build_init(
            &unique_name(&["rc_array"]),
            &StructInstance::new(
                array_value,
                types::Type::new(types::TypeKind::Struct(
                    types::structs::InstantiatedStructId::new(
                        id,
                        types::generics::TypeArguments::new(vec![TypeArgument::new_value(
                            Identifier::parse("TPointee"),
                            item_type_id,
                        )]),
                    ),
                )),
            ),
            context,
            structs,
            types,
        )
    }
}
