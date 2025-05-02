use std::{collections::HashMap, sync::LazyLock};

use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValue, PointerValue},
};

use crate::{
    compile::{context::CompilerContext, value::StructHandle},
    types::{self, FQName, Identifier},
};

#[derive(Debug, Clone)]
pub struct RcValue<'ctx> {
    pointer: PointerValue<'ctx>,
    value_type: StructHandle<'ctx>,
}

static REFCOUNT_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("refcount"));
static POINTEE_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("pointee"));

pub fn describe_structure<'ctx>() -> StructHandle<'ctx> {
    let struct_name = FQName::parse("std.rc");
    StructHandle::new(types::Struct {
        name: struct_name,
        fields: vec![
            types::StructField {
                struct_name,
                name: *REFCOUNT_FIELD,
                type_: types::Type::U64,
                static_: false,
            },
            types::StructField {
                struct_name,
                name: *POINTEE_FIELD,
                // TODO this is not the right type, but righttyping this requires that we
                // have generics (because pointee is dependant on the type here)
                type_: types::Type::Pointer(Box::new(types::Type::U8)),
                static_: false,
            },
        ],
        impls: HashMap::new(),
    })
}

impl<'ctx> RcValue<'ctx> {
    pub fn build_init<'src>(
        name: &str,
        value: PointerValue<'ctx>,
        value_type: StructHandle<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> Self
    where
        'src: 'ctx,
    {
        let mut field_values = HashMap::new();
        field_values.insert(
            *REFCOUNT_FIELD,
            context
                .llvm_context
                .i64_type()
                .const_int(1, false)
                .as_basic_value_enum(),
        );
        field_values.insert(*POINTEE_FIELD, value.as_basic_value_enum());
        let rc = context
            .builtins
            .rc_handle
            .build_heap_instance(context, name, field_values);

        Self {
            pointer: rc,
            value_type,
        }
    }

    pub const fn as_ptr(&self) -> PointerValue<'ctx> {
        self.pointer
    }

    pub const fn from_pointer(pointer: PointerValue<'ctx>, value_type: StructHandle<'ctx>) -> Self {
        RcValue {
            pointer,
            value_type,
        }
    }

    pub(crate) const fn type_(&self) -> &StructHandle<'ctx> {
        &self.value_type
    }
}

pub fn build_cleanup<'ctx>(
    context: &CompilerContext<'ctx>,
    rcs: &[RcValue<'ctx>],
    before: BasicBlock<'ctx>,
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
        context.builder.position_at_end(before);

        let old_refcount = context
            .builtins
            .rc_handle
            .build_field_load(
                *REFCOUNT_FIELD,
                rc.pointer,
                &(name.to_string() + "refcount_old"),
                context,
            )
            .into_int_value();
        let new_refcount = context
            .builder
            .build_int_sub(
                old_refcount,
                context.llvm_context.i64_type().const_int(1, false),
                &(name.to_string() + "refcount_decremented"),
            )
            .unwrap();

        let compare = context
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                new_refcount,
                context.llvm_context.i64_type().const_int(0, false),
                &(name.to_string() + "refcount_iszero"),
            )
            .unwrap();

        let free_rc_block = context
            .llvm_context
            .prepend_basic_block(before, &(name.to_string() + "free_rc"));
        let do_not_free_rc_block = context
            .llvm_context
            .prepend_basic_block(before, &(name.to_string() + "do_not_free_rc"));
        let continuation_block = context
            .llvm_context
            .prepend_basic_block(before, &(name.to_string() + "continuation"));

        context
            .builder
            .build_conditional_branch(compare, free_rc_block, do_not_free_rc_block)
            .unwrap();

        context.builder.position_at_end(free_rc_block);

        let rc_pointee_value = context
            .builtins
            .rc_handle
            .build_field_load(
                *POINTEE_FIELD,
                rc.pointer,
                &(name.to_string() + "free_rc_pointee_value"),
                context,
            )
            .into_pointer_value();
        context.builder.build_free(rc_pointee_value).unwrap();
        context.builder.build_free(rc.pointer).unwrap();

        context
            .builder
            .build_unconditional_branch(continuation_block)
            .unwrap();

        context.builder.position_at_end(do_not_free_rc_block);
        context.builtins.rc_handle.build_field_store(
            *REFCOUNT_FIELD,
            rc.pointer,
            new_refcount.as_basic_value_enum(),
            context,
        );

        context
            .builder
            .build_unconditional_branch(continuation_block)
            .unwrap();

        context.builder.position_at_end(continuation_block);

        context
            .builder
            .build_unconditional_branch(previous_before)
            .unwrap();

        context.builder.position_at_end(previous_before);
    }

    first_block
}

pub fn build_prologue<'ctx>(rcs: &[RcValue<'ctx>], context: &CompilerContext<'ctx>) {
    for (i, rc) in rcs.iter().enumerate() {
        let name = format!("rc{i}");
        let init_refcount = context.builtins.rc_handle.build_field_load(
            *REFCOUNT_FIELD,
            rc.pointer,
            &format!("{name}_init_refcount"),
            context,
        );

        let incremented_refcount = context
            .builder
            .build_int_add(
                init_refcount.into_int_value(),
                context.llvm_context.i64_type().const_int(1, false),
                &format!("{name}_init_refcount_incremented"),
            )
            .unwrap();

        context.builtins.rc_handle.build_field_store(
            *REFCOUNT_FIELD,
            rc.pointer,
            incremented_refcount.as_basic_value_enum(),
            context,
        );
    }
}
