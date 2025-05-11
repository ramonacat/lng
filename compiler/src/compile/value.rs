use std::{collections::HashMap, fmt::Debug};

use inkwell::values::{BasicValue as _, BasicValueEnum, PointerValue};
use itertools::Itertools;

use crate::{identifier::Identifier, types};

use super::{builtins::rc::RcValue, context::CompilerContext};

pub struct InstantiatedStructType<'ctx> {
    pub(crate) definition: types::structs::Struct<types::InstantiatedType>,
    static_field_values: HashMap<Identifier, Value<'ctx>>,
}

pub struct StructInstance<'ctx>(PointerValue<'ctx>, types::InstantiatedType);

impl<'ctx> StructInstance<'ctx> {
    pub(crate) const fn value(&self) -> PointerValue<'ctx> {
        self.0
    }

    pub(crate) const fn new(pointer: PointerValue<'ctx>, type_: types::InstantiatedType) -> Self {
        Self(pointer, type_)
    }

    pub(crate) fn type_(&self) -> types::InstantiatedType {
        self.1.clone()
    }
}

impl Debug for InstantiatedStructType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields = self
            .definition
            .fields
            .iter()
            .map(|f| format!("{}:{}", f.name, f.type_))
            .join(", ");

        write!(f, "Struct({}){{{}}}>", self.definition.id, fields)
    }
}

impl<'ctx> InstantiatedStructType<'ctx> {
    pub fn build_heap_instance(
        &self,
        context: &CompilerContext<'ctx>,
        binding_name: &str,
        mut field_values: HashMap<Identifier, BasicValueEnum<'ctx>>,
    ) -> PointerValue<'ctx> {
        let llvm_type = context.make_struct_type(&self.definition.fields);

        let instance = context
            .builder
            .build_malloc(
                llvm_type.as_llvm_type(),
                &(binding_name.to_string() + "_ptr"),
            )
            .unwrap();

        for field in self.definition.fields.iter().filter(|x| !x.static_) {
            let field_value = field_values.remove(&field.name).unwrap();
            let (_, field_pointer) = llvm_type.field_pointer(field.name, instance, context);

            context
                .builder
                .build_store(field_pointer, field_value)
                .unwrap();
        }

        instance
    }

    pub fn build_field_load(
        &self,
        field: Identifier,
        instance: PointerValue<'ctx>,
        binding_name: &str,
        context: &CompilerContext<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (field_type, field_pointer) = context
            .make_struct_type(&self.definition.fields)
            .field_pointer(field, instance, context);

        context
            .builder
            .build_load(field_type, field_pointer, binding_name)
            .unwrap()
    }

    pub fn build_field_store(
        &self,
        field_name: Identifier,
        instance: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        context: &CompilerContext<'ctx>,
    ) {
        let (_, field_pointer) = context
            .make_struct_type(&self.definition.fields)
            .field_pointer(field_name, instance, context);

        context.builder.build_store(field_pointer, value).unwrap();
    }

    // TODO this should be integrated with build_field_load perhaps?
    pub(crate) fn read_field_value(
        &self,
        _instance: Value<'ctx>,
        name: Identifier,
    ) -> Option<Value<'ctx>> {
        let field = self.definition.fields.iter().find(|f| f.name == name)?;

        if field.static_ {
            return self.static_field_values.get(&name).cloned();
        }

        todo!("support reading non-static fields!");
    }

    pub(crate) fn new(
        description: types::structs::Struct<types::InstantiatedType>,
        mut static_fields: HashMap<Identifier, Value<'ctx>>,
    ) -> Self {
        let default_static_fields: Vec<_> = description
            .impls
            .iter()
            .map(|id| (id.local(), Value::Function(*id)))
            .collect();

        for (name, value) in default_static_fields {
            static_fields.insert(name, value);
        }

        Self {
            definition: description,
            static_field_values: static_fields,
        }
    }
}

// TODO this should be Copy, once InstantiatedStructId is
#[derive(Clone)]
pub enum Value<'ctx> {
    Empty,
    Primitive(types::structs::InstantiatedStructId, BasicValueEnum<'ctx>),
    Reference(RcValue<'ctx>),
    Function(types::functions::FunctionId),
    Struct(types::structs::StructId),
    InstantiatedStruct(types::structs::InstantiatedStructId),
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Primitive(struct_id, basic_value_enum) => {
                write!(f, "{struct_id}({basic_value_enum})")
            }
            Value::Reference(rc_value) => {
                write!(f, "Rc<{:?}>({})", rc_value.type_(), rc_value.as_ptr())
            }
            Value::Function(function_handle) => write!(f, "{function_handle:?}"),
            Value::Struct(struct_handle) => write!(f, "{struct_handle:?}"),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Primitive(_, value) => *value,
            Value::Reference(value) => value.as_ptr().as_basic_value_enum(),
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }

    pub fn read_field_value(
        &self,
        field_path: Identifier,
        context: &CompilerContext<'ctx>,
    ) -> Option<Self> {
        match self {
            Value::Primitive(struct_id, _) => context
                .global_scope
                .structs
                .inspect_instantiated(struct_id, |struct_| {
                    struct_.unwrap().read_field_value(self.clone(), field_path)
                }),
            Value::Reference(ref_) => {
                let ref_type = ref_.type_();
                let (struct_id, type_argument_values) = match ref_type.kind() {
                    types::InstantiatedTypeKind::Unit => todo!(),
                    types::InstantiatedTypeKind::Object {
                        type_name,
                        type_argument_values,
                    } => (*type_name, type_argument_values),
                    types::InstantiatedTypeKind::Array { .. } => todo!(),
                    types::InstantiatedTypeKind::Callable(_) => todo!(),
                    types::InstantiatedTypeKind::U64 => todo!(),
                    types::InstantiatedTypeKind::U8 => todo!(),
                    types::InstantiatedTypeKind::Pointer(_) => todo!(),
                    types::InstantiatedTypeKind::Struct(_) => todo!(),
                    types::InstantiatedTypeKind::Function(_) => todo!(),
                    types::InstantiatedTypeKind::IndirectCallable(_, _) => {
                        todo!()
                    }
                    types::InstantiatedTypeKind::InterfaceObject { .. } => todo!(),
                };
                context.global_scope.structs.inspect_instantiated(
                    &types::structs::InstantiatedStructId::new(
                        struct_id,
                        type_argument_values.clone(),
                    ),
                    |struct_| struct_.unwrap().read_field_value(self.clone(), field_path),
                )
            }
            Value::Function(_) => todo!(),
            Value::Struct(_) => todo!(),
            Value::Empty => todo!(),
            Value::InstantiatedStruct(_) => todo!(),
        }
    }
}
