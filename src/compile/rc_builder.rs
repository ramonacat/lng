use inkwell::{
    builder::Builder,
    context::Context,
    values::{BasicMetadataValueEnum, BasicValue, PointerValue},
    AddressSpace,
};

use super::{CompileError, CompiledFunction, Scope};

pub fn build_rc<'ctx, 'src>(
    name: &'ctx str,
    value: PointerValue,
    global_scope: &Scope<'ctx>,
    builder: &Builder<'ctx>,
    context: &'src Context,
    compiled_function: &mut CompiledFunction<'ctx, 'src>,
) -> Result<BasicMetadataValueEnum<'ctx>, CompileError> {
    // TODO generate a unique name for each of the things here!!!
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

        let free_rc_block = context.append_basic_block(llvm_function, "free_rc");
        let do_not_free_rc_block = context.append_basic_block(llvm_function, "do_not_free_rc");
        let continuation_block = context.append_basic_block(llvm_function, "continuation");

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

    Ok(rc.as_basic_value_enum().into())
}
