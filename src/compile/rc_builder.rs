use inkwell::{builder::Builder, context::Context, values::PointerValue, AddressSpace};

use super::{CompileError, CompiledFunction, Scope};

#[derive(Clone, Copy)]
pub struct RcValue<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> RcValue<'ctx> {
    pub fn build_init<'src>(
        name: &str,
        value: PointerValue<'ctx>,
        global_scope: &Scope<'ctx>,
        context: &'ctx Context,
        compiled_function: &mut CompiledFunction<'ctx, 'src>,
        builder: &Builder<'ctx>,
    ) -> Result<Self, CompileError>
    where
        'src: 'ctx,
    {
        let rc = builder
            .build_malloc(global_scope.builtins.rc_type, name)
            .unwrap();
        let rc_refcount = unsafe {
            builder
                .build_gep(
                    global_scope.builtins.rc_type,
                    rc,
                    &[
                        context.i32_type().const_int(0, false),
                        context.i32_type().const_int(0, false),
                    ],
                    &(name.to_string() + "refcount"),
                )
                .unwrap()
        };
        let rc_pointee = unsafe {
            builder
                .build_gep(
                    global_scope.builtins.rc_type,
                    rc,
                    &[
                        context.i32_type().const_int(0, false),
                        context.i32_type().const_int(1, false),
                    ],
                    &(name.to_string() + "pointee"),
                )
                .unwrap()
        };

        builder
            .build_store(rc_refcount, context.i64_type().const_int(1, false))
            .unwrap();
        builder.build_store(rc_pointee, value).unwrap();

        let llvm_function = compiled_function.llvm_function;
        let name = name.to_string();

        compiled_function.register_exit(move |builder: &Builder<'ctx>, context: &Context| {
            let old_refcount = builder
                .build_load(
                    context.i64_type(),
                    rc_refcount,
                    &(name.to_string() + "refcount_old"),
                )
                .unwrap()
                .into_int_value();
            let new_refcount = builder
                .build_int_sub(
                    old_refcount,
                    context.i64_type().const_int(1, false),
                    &(name.to_string() + "refcount_decremented"),
                )
                .unwrap();

            let compare = builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    new_refcount,
                    context.i64_type().const_int(0, false),
                    &(name.to_string() + "refcount_iszero"),
                )
                .unwrap();

            let free_rc_block =
                context.append_basic_block(llvm_function, &(name.to_string() + "free_rc"));
            let do_not_free_rc_block =
                context.append_basic_block(llvm_function, &(name.to_string() + "do_not_free_rc"));
            let continuation_block =
                context.append_basic_block(llvm_function, &(name.to_string() + "continuation"));

            builder
                .build_conditional_branch(compare, free_rc_block, do_not_free_rc_block)
                .unwrap();

            builder.position_at_end(free_rc_block);

            let rc_pointee_value = builder
                .build_load(
                    context.ptr_type(AddressSpace::default()),
                    rc_pointee,
                    &(name.to_string() + "free_rc_pointee_value"),
                )
                .unwrap()
                .into_pointer_value();
            builder.build_free(rc_pointee_value).unwrap();
            builder.build_free(rc).unwrap();

            builder
                .build_unconditional_branch(continuation_block)
                .unwrap();

            builder.position_at_end(do_not_free_rc_block);
            builder.build_store(rc_refcount, new_refcount).unwrap();

            builder
                .build_unconditional_branch(continuation_block)
                .unwrap();

            builder.position_at_end(continuation_block);
        });

        Ok(Self { value: rc })
    }

    pub fn as_ptr(&self) -> PointerValue<'ctx> {
        self.value
    }
}
