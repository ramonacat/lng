use inkwell::{values::PointerValue, AddressSpace};

use super::{context::CompilerContext, Builtins, CompileError, CompiledFunction};

#[derive(Clone, Copy)]
pub struct RcValue<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> RcValue<'ctx> {
    pub fn build_init<'src>(
        name: &str,
        value: PointerValue<'ctx>,
        context: &CompilerContext<'ctx>,
        compiled_function: &mut CompiledFunction<'ctx, 'src>,
        builtins: &Builtins<'ctx>,
    ) -> Result<Self, CompileError>
    where
        'src: 'ctx,
    {
        let rc = context
            .builder
            .build_malloc(builtins.rc_type, name)
            .unwrap();
        let rc_refcount = unsafe {
            context
                .builder
                .build_gep(
                    builtins.rc_type,
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
                    builtins.rc_type,
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

        let llvm_function = compiled_function.llvm_function;
        let name = name.to_string();

        compiled_function.register_exit(move |context: &CompilerContext<'ctx>| {
            let old_refcount = context
                .builder
                .build_load(
                    context.llvm_context.i64_type(),
                    rc_refcount,
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
                .append_basic_block(llvm_function, &(name.to_string() + "free_rc"));
            let do_not_free_rc_block = context
                .llvm_context
                .append_basic_block(llvm_function, &(name.to_string() + "do_not_free_rc"));
            let continuation_block = context
                .llvm_context
                .append_basic_block(llvm_function, &(name.to_string() + "continuation"));

            context
                .builder
                .build_conditional_branch(compare, free_rc_block, do_not_free_rc_block)
                .unwrap();

            context.builder.position_at_end(free_rc_block);

            let rc_pointee_value = context
                .builder
                .build_load(
                    context.llvm_context.ptr_type(AddressSpace::default()),
                    rc_pointee,
                    &(name.to_string() + "free_rc_pointee_value"),
                )
                .unwrap()
                .into_pointer_value();
            context.builder.build_free(rc_pointee_value).unwrap();
            context.builder.build_free(rc).unwrap();

            context
                .builder
                .build_unconditional_branch(continuation_block)
                .unwrap();

            context.builder.position_at_end(do_not_free_rc_block);
            context
                .builder
                .build_store(rc_refcount, new_refcount)
                .unwrap();

            context
                .builder
                .build_unconditional_branch(continuation_block)
                .unwrap();

            context.builder.position_at_end(continuation_block);
        });

        Ok(Self { value: rc })
    }

    pub fn as_ptr(&self) -> PointerValue<'ctx> {
        self.value
    }
}
