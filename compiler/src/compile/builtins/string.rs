use std::collections::HashMap;

use inkwell::values::BasicValue as _;

use crate::{
    compile::{context::CompilerContext, value::InstantiatedStructHandle},
    std::TYPE_NAME_STRING,
    types::{self, Identifier, TypeArguments},
};

use super::rc::RcValue;

pub struct StringValue {
    value: String,
}

impl StringValue {
    pub const fn new_literal(value: String) -> Self {
        Self { value }
    }

    pub fn build_instance<'ctx>(
        &self,
        name: &str,
        context: &CompilerContext<'ctx>,
    ) -> RcValue<'ctx> {
        let characters_value = context
            .builder
            .build_global_string_ptr(&self.value, &(name.to_string() + "_global"))
            .unwrap();

        let mut field_values = HashMap::new();
        field_values.insert(
            types::Identifier::parse("characters"),
            characters_value.as_basic_value_enum(),
        );
        field_values.insert(
            types::Identifier::parse("length"),
            context
                .const_u64(self.value.len() as u64)
                .as_basic_value_enum(),
        );

        let string_handle = InstantiatedStructHandle::new(
            context.builtins.string_handle.clone(),
            types::TypeArgumentValues::new_empty(),
        );
        let literal_value = string_handle.build_heap_instance(
            context,
            &(name.to_string() + "_value"),
            field_values,
        );

        RcValue::build_init(name, literal_value, string_handle, context)
    }
}

pub fn describe_structure() -> types::Struct {
    types::Struct {
        name: *TYPE_NAME_STRING,
        type_arguments: TypeArguments::new_empty(),
        fields: vec![
            types::StructField {
                struct_name: *TYPE_NAME_STRING,
                name: Identifier::parse("characters"),
                type_: types::Type::Pointer(Box::new(types::Type::U8)),
                static_: false,
            },
            types::StructField {
                struct_name: *TYPE_NAME_STRING,
                name: Identifier::parse("length"),
                type_: types::Type::U64,
                static_: false,
            },
        ],
        impls: HashMap::new(),
    }
}
