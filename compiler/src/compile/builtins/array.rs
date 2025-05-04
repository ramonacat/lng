use std::{collections::HashMap, sync::LazyLock};

use inkwell::values::BasicValue;

use crate::{
    compile::{context::CompilerContext, unique_name, value::StructInstance},
    types,
};

use super::rc::RcValue;

pub static TYPE_NAME_ARRAY: LazyLock<types::FQName> =
    LazyLock::new(|| types::FQName::parse("std.array"));
static ITEMS_FIELD: LazyLock<types::Identifier> =
    LazyLock::new(|| types::Identifier::parse("items"));
static LENGTH_FIELD: LazyLock<types::Identifier> =
    LazyLock::new(|| types::Identifier::parse("length"));
static CAPACITY_FIELD: LazyLock<types::Identifier> =
    LazyLock::new(|| types::Identifier::parse("capacity"));

// TODO A macro that generates both the struct on rust side and the StructHandle from a single
// definition
pub fn describe_structure() -> types::Struct {
    let type_argument_name = types::TypeArgument::new(types::Identifier::parse("TItem"));
    let generic_argument_type = types::Type::new_generic(
        types::TypeKind::Generic(type_argument_name),
        vec![type_argument_name],
    );
    let struct_id = types::StructId::FQName(*TYPE_NAME_ARRAY);

    types::Struct {
        name: *TYPE_NAME_ARRAY,
        type_arguments: types::TypeArguments::new(vec![type_argument_name]),
        fields: vec![
            types::StructField {
                struct_id,
                name: *ITEMS_FIELD,
                type_: types::Type::new_generic(
                    types::TypeKind::Pointer(Box::new(generic_argument_type)),
                    vec![type_argument_name],
                ),
                static_: false,
            },
            types::StructField {
                struct_id,
                name: *LENGTH_FIELD,
                type_: types::Type::new_not_generic(types::TypeKind::U64),
                static_: false,
            },
            types::StructField {
                struct_id,
                name: *CAPACITY_FIELD,
                type_: types::Type::new_not_generic(types::TypeKind::U64),
                static_: false,
            },
        ],
        impls: HashMap::new(),
    }
}

pub struct ArrayValue {}

impl ArrayValue {
    pub fn build_instance<'ctx>(
        item_type: &types::Type,
        context: &CompilerContext<'ctx>,
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

        let mut tav = HashMap::new();
        tav.insert(
            types::TypeArgument::new(types::Identifier::parse("TItem")),
            item_type.clone(),
        );
        let tav = types::TypeArgumentValues::new(tav);
        let id = context.instantiate_struct(types::StructId::FQName(*TYPE_NAME_ARRAY), &tav);

        let array_value = context
            .global_scope
            .structs
            .inspect_instantiated_struct(&id, |a| {
                a.unwrap()
                    .build_heap_instance(context, &unique_name(&["string"]), field_values)
            });

        RcValue::build_init(
            &unique_name(&["rc_array"]),
            &StructInstance::new(array_value, id),
            context,
        )
    }
}
