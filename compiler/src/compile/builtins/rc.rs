use std::{collections::HashMap, sync::LazyLock};

use compiler_derive::BuiltinStruct;
use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValue, PointerValue},
};

use crate::{
    compile::{
        context::CompilerContext,
        unique_name,
        value::{InstantiatedStructType, StructInstance},
    },
    identifier::Identifier,
    types::{self, structs::InstantiatedStructId},
};

#[derive(BuiltinStruct)]
#[module_id("std")]
#[struct_name("rc")]
#[generic(TPointee)]
#[repr(C)]
pub struct BuiltinRc<TPointee> {
    pub refcount: u64,
    pub pointee: *const TPointee,
}

#[derive(Debug, Clone)]
pub struct RcValue<'ctx> {
    pointer: PointerValue<'ctx>,
    value_type: types::InstantiatedType,
    instantiated_struct_id: InstantiatedStructId,
}

static REFCOUNT_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("refcount"));
static POINTEE_FIELD: LazyLock<Identifier> = LazyLock::new(|| Identifier::parse("pointee"));

impl<'ctx> RcValue<'ctx> {
    #[must_use]
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

        let mut tav = HashMap::new();
        tav.insert(
            types::TypeArgument::new(Identifier::parse("TPointee")),
            struct_instance.type_(),
        );

        let instantiated_struct_id = InstantiatedStructId::new(
            types::structs::StructId::InModule(
                types::modules::ModuleId::parse("std"),
                Identifier::parse("rc"),
            ),
            types::TypeArgumentValues::new(tav),
        );
        let rc = context
            .global_scope
            .structs
            .inspect_instantiated(&instantiated_struct_id, |s_| {
                s_.unwrap().build_heap_instance(context, name, field_values)
            });

        Self {
            pointer: rc,
            value_type: struct_instance.type_(),
            instantiated_struct_id,
        }
    }

    #[must_use]
    pub const fn as_ptr(&self) -> PointerValue<'ctx> {
        self.pointer
    }

    #[must_use]
    pub fn from_pointer(pointer: PointerValue<'ctx>, value_type: types::InstantiatedType) -> Self {
        let mut tav = HashMap::new();
        tav.insert(
            types::TypeArgument::new(Identifier::parse("TPointee")),
            value_type.clone(),
        );
        let instantiated_struct_id = InstantiatedStructId::new(
            types::structs::StructId::InModule(
                types::modules::ModuleId::parse("std"),
                Identifier::parse("rc"),
            ),
            types::TypeArgumentValues::new(tav),
        );
        RcValue {
            pointer,
            value_type,
            instantiated_struct_id,
        }
    }

    #[must_use]
    pub(crate) fn type_(&self) -> types::InstantiatedType {
        self.value_type.clone()
    }

    pub(crate) fn pointee(&self, context: &CompilerContext<'ctx>) -> PointerValue<'ctx> {
        context
            .global_scope
            .structs
            .inspect_instantiated(&self.instantiated_struct_id, |s_| {
                s_.unwrap()
                    .build_field_load(
                        *POINTEE_FIELD,
                        self.pointer,
                        &unique_name(&["rc", "pointee"]),
                        context,
                    )
                    .into_pointer_value()
            })
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

        let mut tav = HashMap::new();
        tav.insert(
            types::TypeArgument::new(Identifier::parse("TPointee")),
            rc.type_(),
        );

        let rc_handle = InstantiatedStructType::new(
            context
                .builtins
                .rc_handle
                .instantiate(&types::TypeArgumentValues::new(tav))
                .unwrap(),
            HashMap::new(),
        );
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
        let mut tav = HashMap::new();
        tav.insert(
            types::TypeArgument::new(Identifier::parse("TPointee")),
            rc.type_(),
        );

        let rc_handle = InstantiatedStructType::new(
            context
                .builtins
                .rc_handle
                .instantiate(&types::TypeArgumentValues(tav))
                .unwrap(),
            HashMap::new(),
        );

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
