use std::collections::HashMap;

use compiler_derive::BuiltinStruct;
use inkwell::{AddressSpace, values::BasicValue as _};

use crate::{
    compile::{
        CompiledFunction,
        context::{AllItems, CompilerContext},
        value::StructInstance,
    },
    identifier::Identifier,
    std::TYPE_NAME_STRING,
    types::{self, structs::InstantiatedStructId},
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
    pub(crate) const fn new_literal(value: String) -> Self {
        Self { value }
    }

    #[must_use]
    pub(crate) fn build_instance<'ctx>(
        &self,
        name: &str,
        compiled_function: &CompiledFunction<'ctx>,
        context: &CompilerContext<'ctx>,
        structs: &mut AllItems<'ctx>,
        types: &mut dyn types::store::TypeStore,
    ) -> RcValue<'ctx> {
        let characters_value = compiled_function
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

        let id = *TYPE_NAME_STRING;

        let literal_value = structs
            .get_or_instantiate_struct(
                &types::structs::InstantiatedStructId::new(
                    id,
                    types::generics::TypeArguments::new_empty(),
                ),
                types,
            )
            .unwrap()
            .build_heap_instance(
                compiled_function,
                context,
                &(name.to_string() + "_value"),
                field_values,
                types,
            );

        RcValue::build_init(
            name,
            &StructInstance::new(
                literal_value,
                types.add(types::Type::new(types::TypeKind::Struct(
                    InstantiatedStructId::new(id, types::generics::TypeArguments::new_empty()),
                ))),
            ),
            // TODO we need to get the actual vtable here!
            context
                .llvm_context
                .ptr_type(AddressSpace::default())
                .const_null(),
            compiled_function,
            context,
            structs,
            types,
        )
    }
}
