use std::{collections::HashMap, sync::LazyLock};

use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValue, PointerValue},
};

use crate::{
    compile::{
        context::CompilerContext,
        unique_name,
        value::{InstantiatedStructId, InstantiatedStructType, StructInstance},
    },
    types::{self, FQName, Identifier},
};

#[derive(Debug, Clone)]
pub struct RcValue<'ctx> {
    pointer: PointerValue<'ctx>,
    value_type: InstantiatedStructId,
}

static REFCOUNT_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("refcount"));
static POINTEE_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("pointee"));

pub fn describe_structure() -> types::Struct {
    let struct_name = FQName::parse("std.rc");
    let type_argument = types::TypeArgument::new(Identifier::parse("TPointee"));
    let generic_argument_type =
        types::Type::new_generic(types::TypeKind::Generic(type_argument), vec![type_argument]);

    types::Struct {
        name: struct_name,
        type_arguments: types::TypeArguments::new(vec![type_argument]),
        fields: vec![
            types::StructField {
                struct_name,
                name: *REFCOUNT_FIELD,
                type_: types::Type::new_not_generic(types::TypeKind::U64),
                static_: false,
            },
            types::StructField {
                struct_name,
                name: *POINTEE_FIELD,
                type_: types::Type::new_generic(
                    types::TypeKind::Pointer(Box::new(generic_argument_type)),
                    vec![type_argument],
                ),
                static_: false,
            },
        ],
        impls: HashMap::new(),
    }
}

impl<'ctx> RcValue<'ctx> {
    pub fn build_init<'src>(
        name: &str,
        struct_instance: &StructInstance<'ctx>,
        context: &CompilerContext<'ctx>,
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

        let rc = InstantiatedStructType::new(context.builtins.rc_handle.clone())
            .build_heap_instance(context, name, field_values);

        Self {
            pointer: rc,
            value_type: struct_instance.id(),
        }
    }

    pub const fn as_ptr(&self) -> PointerValue<'ctx> {
        self.pointer
    }

    pub const fn from_pointer(
        pointer: PointerValue<'ctx>,
        value_type: InstantiatedStructId,
    ) -> Self {
        RcValue {
            pointer,
            value_type,
        }
    }

    pub(crate) fn type_(&self) -> InstantiatedStructId {
        self.value_type.clone()
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

        let rc_handle = InstantiatedStructType::new(context.builtins.rc_handle.clone());
        let old_refcount = rc_handle
            .build_field_load(
                *REFCOUNT_FIELD,
                rc.pointer,
                &unique_name(&["refcount_old"]),
                context,
            )
            .into_int_value();
        let new_refcount = context
            .builder
            .build_int_sub(
                old_refcount,
                context.const_u64(1),
                &unique_name(&["refcount_decremented"]),
            )
            .unwrap();

        let compare = context
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

        context
            .builder
            .build_conditional_branch(compare, free_rc_block, do_not_free_rc_block)
            .unwrap();

        context.builder.position_at_end(free_rc_block);

        let rc_pointee_value = rc_handle
            .build_field_load(
                *POINTEE_FIELD,
                rc.pointer,
                &unique_name(&["free_rc_pointee_value"]),
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
        rc_handle.build_field_store(
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

        let rc_handle = InstantiatedStructType::new(context.builtins.rc_handle.clone());

        let init_refcount = rc_handle.build_field_load(
            *REFCOUNT_FIELD,
            rc.pointer,
            &unique_name(&[&name, "init_refcount"]),
            context,
        );

        let incremented_refcount = context
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
            context,
        );
    }
}
