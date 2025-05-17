use std::collections::HashMap;

use inkwell::values::{AnyValue as _, BasicMetadataValueEnum, BasicValue as _};

use crate::{
    ast,
    compile::{
        CompiledFunction, Compiler,
        builtins::{rc::RcValue, string::StringValue},
        context::CompilerContext,
        errors::{CompileError, IntoCompileError as _},
        unique_name,
        value::{StructInstance, Value},
    },
    identifier::Identifier,
    types::{self, store::TypeStore as _},
};

pub struct ExpressionCompiler<'compiler, 'ctx> {
    compiler: &'compiler mut Compiler<'ctx>,
}

impl<'compiler, 'ctx> ExpressionCompiler<'compiler, 'ctx> {
    pub(crate) fn compile_expression(
        &mut self,
        expression: &types::Expression,
        self_: Option<Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: types::modules::ModuleId,
    ) -> Result<(Option<Value<'ctx>>, Value<'ctx>), CompileError> {
        let position = expression.position;

        match &expression.kind {
            types::ExpressionKind::Call { target, arguments } => {
                let value = self.compile_expression_call(
                    self_.as_ref(),
                    compiled_function,
                    module_path,
                    position,
                    target,
                    arguments,
                )?;

                Ok((None, value))
            }
            types::ExpressionKind::Literal(literal) => {
                Ok(self.compile_literal(compiled_function, literal))
            }
            types::ExpressionKind::LocalVariableAccess(name) => {
                Ok((None, compiled_function.scope.get_value(*name).unwrap()))
            }
            types::ExpressionKind::GlobalVariableAccess(module_id, name) => {
                let value = self.compile_expression_global_variable_access(*module_id, *name);

                Ok((None, value.unwrap()))
            }
            types::ExpressionKind::StructConstructor(target, field_values) => {
                let target =
                    self.compile_expression(target, self_.clone(), compiled_function, module_path)?;

                let (name, target_tav) = match &target.1 {
                    Value::Empty => todo!(),
                    Value::Primitive(_, _) => todo!(),
                    Value::Reference(_) => todo!(),
                    Value::Function(_) => todo!(),
                    Value::InstantiatedStruct(struct_) => {
                        (struct_.id(), struct_.arguments().clone())
                    }
                };
                // TODO ensure the struct is instantiated in the context
                // TODO get the type argument values from the expression!
                let field_values = field_values
                    .iter()
                    .map(|x| {
                        Ok((
                            x.name,
                            self.compile_expression(
                                &x.value,
                                self_.clone(),
                                compiled_function,
                                module_path,
                            )
                            .map(|y| y.1.as_basic_value())?,
                        ))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                let value_struct_id = types::structs::InstantiatedStructId::new(name, target_tav);
                let value_struct = self
                    .compiler
                    .items
                    .get_or_instantiate_struct(&value_struct_id.clone(), &mut self.compiler.types)
                    .unwrap();
                let value = value_struct.build_heap_instance(
                    compiled_function,
                    &self.compiler.context,
                    &unique_name(&[&name.to_string()]),
                    field_values,
                    &self.compiler.types,
                );

                let vtable = self.compiler.build_vtable(&value_struct_id, module_path);

                let rc = RcValue::build_init(
                    &unique_name(&[&name.to_string(), "rc"]),
                    &StructInstance::new(
                        value,
                        self.compiler
                            .types
                            .add(types::Type::new(types::TypeKind::Object(value_struct_id))),
                    ),
                    vtable.as_pointer_value(),
                    compiled_function,
                    &self.compiler.context,
                    &mut self.compiler.items,
                    &mut self.compiler.types,
                );
                compiled_function.rcs.push(rc.clone());

                Ok((None, Value::Reference(rc)))
            }
            types::ExpressionKind::FieldAccess {
                target,
                field: field_name,
            } => {
                let (_, target_value) =
                    self.compile_expression(target, self_, compiled_function, module_path)?;

                let access_result = target_value
                    .read_field_value(
                        *field_name,
                        compiled_function,
                        &self.compiler.context,
                        &self.compiler.items,
                        &self.compiler.types,
                    )
                    .unwrap();

                Ok((Some(target_value), access_result))
            }
            types::ExpressionKind::SelfAccess => Ok((None, self_.unwrap())),
        }
    }

    fn compile_expression_global_variable_access(
        &mut self,
        module_id: types::modules::ModuleId,
        name: Identifier,
    ) -> Option<Value<'ctx>> {
        // TODO there should be something on the global_scope to receive a value by FQN
        {
            self.compiler
                .modules
                .get(module_id)
                .and_then(|x| x.get_variable(name))
        }
        .or_else(|| {
            let function_id = types::functions::FunctionId::InModule(module_id, name);

            let instantiated_function_id = types::functions::InstantiatedFunctionId::new(
                function_id,
                // TODO we need to ensure during typecheck that we won't get
                // here without the right TypeArgumentValues
                types::generics::TypeArguments::new_empty(),
            );
            self.compiler
                .items
                .get_or_instantiate_function(&instantiated_function_id, &mut self.compiler.types)
                .map(|x| {
                    self.compiler
                        .modules
                        .get_mut(module_id)
                        .unwrap()
                        .get_or_create_function(x, &self.compiler.context, &self.compiler.types);

                    Value::Function(function_id)
                })
        })
        .or_else(|| {
            let struct_id = types::structs::StructId::InModule(module_id, name);

            self.compiler
                .items
                .get_or_instantiate_struct(
                    &types::structs::InstantiatedStructId::new(
                        struct_id,
                        types::generics::TypeArguments::new_empty(),
                    ),
                    &mut self.compiler.types,
                )
                .map(|x| {
                    let types::TypeKind::Struct(x) =
                        self.compiler.types.get(x.definition.type_id).kind()
                    else {
                        todo!();
                    };
                    let instantiated_struct_id =
                        types::structs::InstantiatedStructId::new(struct_id, x.arguments().clone());

                    Value::InstantiatedStruct(instantiated_struct_id)
                })
        })
    }

    fn compile_expression_call(
        &mut self,
        self_: Option<&Value<'ctx>>,
        compiled_function: &mut CompiledFunction<'ctx>,
        module_path: types::modules::ModuleId,
        position: ast::SourceSpan,
        target: &types::Expression,
        arguments: &[types::Expression],
    ) -> Result<Value<'ctx>, CompileError> {
        let compiled_target =
            self.compile_expression(target, self_.cloned(), compiled_function, module_path)?;
        let self_value = compiled_target.0;

        let function_id = match &compiled_target.1 {
            Value::Function(function_id) => function_id,
            Value::Empty => todo!(),
            Value::Primitive(_, _) => todo!(),
            Value::Reference(_) => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        };

        let instantiated_function_id = types::functions::InstantiatedFunctionId::new(
            *function_id,
            types::generics::TypeArguments::new_empty(),
        );
        let definition = self
            .compiler
            .items
            .get_or_instantiate_function(&instantiated_function_id, &mut self.compiler.types)
            .unwrap()
            .clone();

        if !self
            .compiler
            .modules
            .get(definition.module_name)
            .unwrap()
            .has_function(definition.type_id)
        {
            self.compiler.compile_function(&definition).unwrap();
        }

        let function_value = self
            .compiler
            .modules
            .get_mut(module_path)
            .unwrap()
            .get_or_create_function(&definition, &self.compiler.context, &self.compiler.types);

        compiled_function
            .builder
            .position_at_end(compiled_function.entry);

        let mut compiled_arguments_iter = arguments
            .iter()
            .map(|a| {
                self.compile_expression(a, self_value.clone(), compiled_function, module_path)
                    .map(|x| x.1)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let call_arguments = compiled_arguments_iter
            .iter_mut()
            .map(|a| match a {
                Value::Primitive(_, v) => v.as_basic_value_enum().into(),
                Value::Reference(rc_value) => rc_value.as_ptr().as_basic_value_enum().into(),
                Value::Function(_) => todo!("implement passing callables as arguments"),
                Value::Empty => todo!(),
                Value::InstantiatedStruct(_) => todo!(),
            })
            .collect::<Vec<BasicMetadataValueEnum>>();

        let call_result = compiled_function
            .builder
            .build_call(
                function_value,
                &call_arguments,
                &unique_name(&["call_result"]),
            )
            .map_err(|e| e.at(position))?;
        let call_result = call_result.as_any_value_enum();
        let value = match self.compiler.types.get(definition.return_type).kind() {
            types::TypeKind::Unit => Value::Empty,
            types::TypeKind::Object(instantiated_struct_id) => {
                let type_ = self
                    .compiler
                    .items
                    .get_or_instantiate_struct(
                        &instantiated_struct_id.clone(),
                        &mut self.compiler.types,
                    )
                    .unwrap()
                    .definition
                    .type_id;

                Value::Reference(RcValue::from_pointer(
                    call_result.into_pointer_value(),
                    type_,
                ))
            }
            types::TypeKind::Array { .. } => todo!(),
            types::TypeKind::Callable { .. } => todo!(),
            types::TypeKind::U64 => todo!(),
            types::TypeKind::U8 => todo!(),
            types::TypeKind::Pointer(_) => todo!(),
            types::TypeKind::Struct(_) => todo!(),
            types::TypeKind::IndirectCallable(_, _) => todo!(),
            types::TypeKind::InterfaceObject { .. } => todo!(),
            types::TypeKind::Generic(_) => todo!(),
            types::TypeKind::Interface(_) => todo!(),
        };
        Ok(value)
    }
    fn compile_literal(
        &mut self,
        compiled_function: &mut CompiledFunction<'ctx>,
        literal: &types::Literal,
    ) -> (Option<Value<'ctx>>, Value<'ctx>) {
        match literal {
            types::Literal::String(s) => {
                let value = StringValue::new_literal(s.clone());
                let name = &unique_name(&["literal", "string"]);
                let rc = value.build_instance(
                    name,
                    compiled_function,
                    &self.compiler.context,
                    &mut self.compiler.items,
                    &mut self.compiler.types,
                );
                compiled_function.rcs.push(rc.clone());

                (None, Value::Reference(rc))
            }
            types::Literal::UnsignedInteger(value) => (
                None,
                Value::Primitive(
                    types::structs::InstantiatedStructId::new(
                        CompilerContext::get_std_type("u64"),
                        types::generics::TypeArguments::new_empty(),
                    ),
                    self.compiler
                        .context
                        .const_u64(*value)
                        .as_basic_value_enum(),
                ),
            ),
        }
    }

    pub(crate) const fn new(compiler: &'compiler mut Compiler<'ctx>) -> Self {
        Self { compiler }
    }
}
