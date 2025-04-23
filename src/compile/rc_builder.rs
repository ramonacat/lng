use inkwell::{basic_block::BasicBlock, values::PointerValue, AddressSpace};

use super::{context::CompilerContext, CompileError};

#[derive(Clone, Copy)]
pub struct RcValue<'ctx> {
    value: PointerValue<'ctx>,
    refcount: PointerValue<'ctx>,
    pointee: PointerValue<'ctx>,
}

impl<'ctx> RcValue<'ctx> {
    pub fn build_init<'src>(
        name: &str,
        value: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> Result<Self, CompileError>
    where
        'src: 'ctx,
    {
        let rc = context
            .builder
            .build_malloc(context.builtins.rc_handle.llvm_type, name)
            .unwrap();

        let rc_refcount = unsafe {
            context
                .builder
                .build_gep(
                    context.builtins.rc_handle.llvm_type,
                    rc,
                    &[
                        context.llvm_context.i32_type().const_int(0, false),
                        context.llvm_context.i32_type().const_int(0, false),
                    ],
                    &(name.to_string() + "refcount"),
                )
                .unwrap()
        };
        let rc_pointee = unsafe {
            context
                .builder
                .build_gep(
                    context.builtins.rc_handle.llvm_type,
                    rc,
                    &[
                        context.llvm_context.i32_type().const_int(0, false),
                        context.llvm_context.i32_type().const_int(1, false),
                    ],
                    &(name.to_string() + "pointee"),
                )
                .unwrap()
        };

        context
            .builder
            .build_store(
                rc_refcount,
                context.llvm_context.i64_type().const_int(1, false),
            )
            .unwrap();
        context.builder.build_store(rc_pointee, value).unwrap();

        Ok(Self {
            value: rc,
            refcount: rc_refcount,
            pointee: rc_pointee,
        })
    }

    pub fn as_ptr(&self) -> PointerValue<'ctx> {
        self.value
    }

    pub fn from_pointer(
        pointer: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
    ) -> RcValue<'ctx> {
        let refcount = unsafe {
            pointer.const_gep(
                context.builtins.rc_handle.llvm_type,
                &[
                    context.llvm_context.i32_type().const_int(0, false),
                    context.llvm_context.i32_type().const_int(0, false),
                ],
            )
        };
        let pointee = unsafe {
            pointer.const_gep(
                context.builtins.rc_handle.llvm_type,
                &[
                    context.llvm_context.i32_type().const_int(0, false),
                    context.llvm_context.i32_type().const_int(1, false),
                ],
            )
        };
        RcValue {
            value: pointer,
            refcount,
            pointee,
        }
    }
}

pub fn build_cleanup<'ctx>(
    context: &CompilerContext<'ctx>,
    rcs: &[RcValue<'ctx>],
    before: BasicBlock<'ctx>,
) -> Result<BasicBlock<'ctx>, CompileError> {
    let mut before = before;
    let mut first_block = before;

    for (i, rc) in rcs.iter().enumerate() {
        let name = format!("rc{}", i);
        let previous_before = before;

        before = context.llvm_context.prepend_basic_block(before, &name);
        if i == 0 {
            first_block = before;
        }
        context.builder.position_at_end(before);

        let old_refcount = context
            .builder
            .build_load(
                context.llvm_context.i64_type(),
                rc.refcount,
                &(name.to_string() + "refcount_old"),
            )
            .unwrap()
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
            .builder
            .build_load(
                context.llvm_context.ptr_type(AddressSpace::default()),
                rc.pointee,
                &(name.to_string() + "free_rc_pointee_value"),
            )
            .unwrap()
            .into_pointer_value();
        context.builder.build_free(rc_pointee_value).unwrap();
        context.builder.build_free(rc.value).unwrap();

        context
            .builder
            .build_unconditional_branch(continuation_block)
            .unwrap();

        context.builder.position_at_end(do_not_free_rc_block);
        context
            .builder
            .build_store(rc.refcount, new_refcount)
            .unwrap();

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
    Ok(first_block)
}

pub(crate) fn build_prologue<'ctx>(rcs: &[RcValue<'ctx>], context: &CompilerContext<'ctx>) {
    for (i, rc) in rcs.iter().enumerate() {
        let name = format!("rc{}", i);
        let init_refcount = context
            .builder
            .build_load(
                context.llvm_context.i64_type(),
                rc.refcount,
                &format!("{name}_init_refcount"),
            )
            .unwrap();
        let incremented_refcount = context
            .builder
            .build_int_add(
                init_refcount.into_int_value(),
                context.llvm_context.i64_type().const_int(1, false),
                &format!("{name}_init_refcount_incremented"),
            )
            .unwrap();

        context
            .builder
            .build_store(rc.refcount, incremented_refcount)
            .unwrap();
    }
}
