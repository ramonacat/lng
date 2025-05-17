use std::{collections::HashMap, sync::LazyLock};

use compiler_derive::BuiltinStruct;
use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValue, PointerValue},
};

use crate::{
    compile::{
        CompiledFunction,
        context::{AllItems, CompilerContext},
        unique_name,
        value::{InstantiatedStructType, StructInstance},
    },
    identifier::Identifier,
    types::{self, structs::InstantiatedStructId},
};

use super::type_descriptor::BuiltinTypeDescriptor;

#[derive(BuiltinStruct)]
#[module_id("std")]
#[struct_name("rc")]
#[generic(TPointee)]
#[repr(C)]
pub struct BuiltinRc<TPointee> {
    pub refcount: u64,
    pub pointee: *const TPointee,
    pub type_descriptor: *const BuiltinTypeDescriptor,
}

#[derive(Debug, Clone)]
pub struct RcValue<'ctx> {
    pointer: PointerValue<'ctx>,
    value_type: types::store::TypeId,
    instantiated_struct_id: InstantiatedStructId,
}

static REFCOUNT_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("refcount"));
static POINTEE_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("pointee"));
static TYPE_DESCRIPTOR_FIELD: LazyLock<Identifier> =
    LazyLock::new(|| Identifier::parse("type_descriptor"));

impl<'ctx> RcValue<'ctx> {
    #[must_use]
    pub(crate) fn build_init<'src>(
        name: &str,
        struct_instance: &StructInstance<'ctx>,
        vtable: PointerValue<'ctx>,
        compiled_function: &CompiledFunction<'ctx>,
        context: &CompilerContext<'ctx>,
        structs: &mut AllItems<'ctx>,
        types: &mut dyn types::store::TypeStore,
    ) -> Self
    where
        'src: 'ctx,
    {
        let mut field_values = HashMap::new();
        field_values.insert(*REFCOUNT_FIELD, context.const_u64(1).as_basic_value_enum());
        field_values.insert(
            *POINTEE_FIELD,
            struct_instance.value().as_basic_value_enum(),
        );
        field_values.insert(*TYPE_DESCRIPTOR_FIELD, vtable.as_basic_value_enum());

        let struct_instance_type_id = struct_instance.type_id();

        let instantiated_struct_id = InstantiatedStructId::new(
            types::structs::StructId::InModule(
                types::modules::ModuleId::parse("std"),
                Identifier::parse("rc"),
            ),
            types::generics::TypeArguments::new(vec![types::generics::TypeArgument::new_value(
                Identifier::parse("TPointee"),
                struct_instance_type_id,
            )]),
        );
        let rc = structs
            .get_or_instantiate_struct(&instantiated_struct_id, types)
            .unwrap()
            .build_heap_instance(compiled_function, context, name, field_values, types);

        Self {
            pointer: rc,
            value_type: struct_instance_type_id,
            instantiated_struct_id,
        }
    }

    #[must_use]
    pub const fn as_ptr(&self) -> PointerValue<'ctx> {
        self.pointer
    }

    #[must_use]
    // TODO we need a vtable for the object!
    pub fn from_pointer(pointer: PointerValue<'ctx>, value_type: types::store::TypeId) -> Self {
        let instantiated_struct_id = InstantiatedStructId::new(
            types::structs::StructId::InModule(
                types::modules::ModuleId::parse("std"),
                Identifier::parse("rc"),
            ),
            types::generics::TypeArguments::new(vec![types::generics::TypeArgument::new_value(
                Identifier::parse("TPointee"),
                value_type,
            )]),
        );

        RcValue {
            pointer,
            value_type,
            instantiated_struct_id,
        }
    }

    #[must_use]
    pub(crate) const fn type_(&self) -> types::store::TypeId {
        self.value_type
    }

    pub(crate) fn pointee(
        &self,
        compiled_function: &CompiledFunction<'ctx>,
        context: &CompilerContext<'ctx>,
        structs: &AllItems<'ctx>,
        types: &dyn types::store::TypeStore,
    ) -> PointerValue<'ctx> {
        dbg!(&self.instantiated_struct_id);
        structs
            .get_struct(&self.instantiated_struct_id)
            .unwrap()
            .build_field_load(
                *POINTEE_FIELD,
                self.pointer,
                compiled_function,
                &unique_name(&["rc", "pointee"]),
                context,
                types,
            )
            .into_pointer_value()
    }
}

// TODO this needs to be cleaned up, so that this code is less unhinged and shorter
#[allow(clippy::too_many_lines)]
pub(crate) fn build_cleanup<'ctx>(
    context: &CompilerContext<'ctx>,
    compiled_function: &CompiledFunction<'ctx>,
    rcs: &[RcValue<'ctx>],
    before: BasicBlock<'ctx>,
    types: &mut dyn types::store::TypeStore,
) -> BasicBlock<'ctx> {
    let mut before = before;
    let mut first_block = before;

    for (i, rc) in rcs.iter().enumerate() {
        let name = format!("rc{i}");
        let previous_before = before;

        before = context.llvm_context.prepend_basic_block(before, &name);
        if i == 0 {
            first_block = before;
        }
        compiled_function.builder.position_at_end(before);

        let rc_handle = InstantiatedStructType::new(
            context
                .builtins
                .rc_handle
                .with_type_arguments(vec![rc.type_()], types),
            HashMap::new(),
        );
        let old_refcount = rc_handle
            .build_field_load(
                *REFCOUNT_FIELD,
                rc.pointer,
                compiled_function,
                &unique_name(&["refcount_old"]),
                context,
                types,
            )
            .into_int_value();
        let new_refcount = compiled_function
            .builder
            .build_int_sub(
                old_refcount,
                context.const_u64(1),
                &unique_name(&["refcount_decremented"]),
            )
            .unwrap();

        let compare = compiled_function
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                new_refcount,
                context.const_u64(0),
                &unique_name(&["refcount_iszero"]),
            )
            .unwrap();

        let free_rc_block = context
            .llvm_context
            .prepend_basic_block(before, &unique_name(&["free_rc"]));
        let do_not_free_rc_block = context
            .llvm_context
            .prepend_basic_block(before, &unique_name(&["do_not_free_rc"]));
        let continuation_block = context
            .llvm_context
            .prepend_basic_block(before, &unique_name(&["continuation"]));

        compiled_function
            .builder
            .build_conditional_branch(compare, free_rc_block, do_not_free_rc_block)
            .unwrap();

        compiled_function.builder.position_at_end(free_rc_block);

        let rc_pointee_value = rc_handle
            .build_field_load(
                *POINTEE_FIELD,
                rc.pointer,
                compiled_function,
                &unique_name(&["free_rc_pointee_value"]),
                context,
                types,
            )
            .into_pointer_value();
        compiled_function
            .builder
            .build_free(rc_pointee_value)
            .unwrap();
        compiled_function.builder.build_free(rc.pointer).unwrap();

        compiled_function
            .builder
            .build_unconditional_branch(continuation_block)
            .unwrap();

        compiled_function
            .builder
            .position_at_end(do_not_free_rc_block);
        rc_handle.build_field_store(
            *REFCOUNT_FIELD,
            rc.pointer,
            new_refcount.as_basic_value_enum(),
            compiled_function,
            context,
            types,
        );

        compiled_function
            .builder
            .build_unconditional_branch(continuation_block)
            .unwrap();

        compiled_function
            .builder
            .position_at_end(continuation_block);

        compiled_function
            .builder
            .build_unconditional_branch(previous_before)
            .unwrap();

        compiled_function.builder.position_at_end(previous_before);
    }

    first_block
}

pub(crate) fn build_prologue<'ctx>(
    compiled_function: &CompiledFunction<'ctx>,
    // TODO we should take the rcs from the function here
    rcs: &[RcValue<'ctx>],
    context: &CompilerContext<'ctx>,
    types: &mut dyn types::store::TypeStore,
) {
    for (i, rc) in rcs.iter().enumerate() {
        let name = format!("rc{i}");

        let rc_handle = InstantiatedStructType::new(
            context
                .builtins
                .rc_handle
                .with_type_arguments(vec![rc.type_()], types),
            HashMap::new(),
        );

        let init_refcount = rc_handle.build_field_load(
            *REFCOUNT_FIELD,
            rc.pointer,
            compiled_function,
            &unique_name(&[&name, "init_refcount"]),
            context,
            types,
        );

        let incremented_refcount = compiled_function
            .builder
            .build_int_add(
                init_refcount.into_int_value(),
                context.const_u64(1),
                &unique_name(&[&name, "init_refcount_incremented"]),
            )
            .unwrap();

        rc_handle.build_field_store(
            *REFCOUNT_FIELD,
            rc.pointer,
            incremented_refcount.as_basic_value_enum(),
            compiled_function,
            context,
            types,
        );
    }
}
