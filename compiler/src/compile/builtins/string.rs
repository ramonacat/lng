use std::collections::HashMap;

use compiler_derive::BuiltinStruct;
use inkwell::values::BasicValue as _;

use crate::{
    compile::{context::CompilerContext, value::StructInstance},
    identifier::Identifier,
    std::TYPE_NAME_STRING,
    types,
};

use super::rc::RcValue;

#[derive(Debug, BuiltinStruct)]
#[module_id("std")]
#[struct_name("string")]
#[repr(C)]
pub struct BuiltinString {
    #[array]
    pub characters: *const u8,
    pub length: u64,
}

pub struct StringValue {
    value: String,
}

impl StringValue {
    #[must_use]
    pub const fn new_literal(value: String) -> Self {
        Self { value }
    }

    #[must_use]
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
            Identifier::parse("characters"),
            characters_value.as_basic_value_enum(),
        );
        field_values.insert(
            Identifier::parse("length"),
            context
                .const_u64(self.value.len() as u64)
                .as_basic_value_enum(),
        );

        let id = types::structs::InstantiatedStructId(
            *TYPE_NAME_STRING,
            types::TypeArgumentValues::new_empty(),
        );

        let literal_value =
            context
                .global_scope
                .structs
                .inspect_instantiated(&id, |string_handle| {
                    string_handle.unwrap().build_heap_instance(
                        context,
                        &(name.to_string() + "_value"),
                        field_values,
                    )
                });

        RcValue::build_init(name, &StructInstance::new(literal_value, id), context)
    }
}
