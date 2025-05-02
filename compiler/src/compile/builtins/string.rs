use std::collections::HashMap;

use inkwell::values::BasicValue as _;

use crate::{
    compile::{context::CompilerContext, value::StructHandle},
    std::TYPE_NAME_STRING,
    types::{self, Identifier},
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

        let literal_value = context.builtins.string_handle.build_heap_instance(
            context,
            &(name.to_string() + "_value"),
            field_values,
        );

        RcValue::build_init(
            name,
            literal_value,
            context.builtins.string_handle.clone(),
            context,
        )
    }
}

pub fn describe_structure<'ctx>() -> StructHandle<'ctx> {
    StructHandle::new(types::Struct {
        name: *TYPE_NAME_STRING,
        // TODO also store the length, in order to avoid using 0-termination
        fields: vec![types::StructField {
            struct_name: *TYPE_NAME_STRING,
            name: Identifier::parse("characters"),
            type_: types::Type::Pointer(Box::new(types::Type::U8)),
            static_: false,
        }],
        impls: HashMap::new(),
    })
}
