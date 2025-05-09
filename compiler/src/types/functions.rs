use std::fmt::{Display, Formatter};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    name_mangler::{
        MangledIdentifier, mangle_item_name, mangle_struct_item_name, nomangle_identifier,
    },
};

use super::{
    Statement, Type, TypeArgumentValues, TypeArguments, TypeKind, Visibility, modules::ModuleId,
    structs::StructId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionId {
    InModule(ModuleId, Identifier),
    InStruct(StructId, Identifier),
    // TODO we really shouldn't put the extern in the ID, as this is not the right level for the
    // concern, the function still has Identifier and ModuleId, but the extern identifier is just
    // needed to find the external function
    Extern(Identifier),
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InModule(module_id, identifier) => {
                write!(f, "InModule({module_id}, {identifier})")
            }
            Self::InStruct(struct_id, identifier) => {
                write!(f, "InStruct({struct_id}, {identifier})")
            }
            Self::Extern(identifier) => write!(f, "Extern({identifier})"),
        }
    }
}

impl FunctionId {
    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        match self {
            Self::InModule(module_id, fqname) => mangle_item_name(module_id, fqname),
            Self::InStruct(struct_id, identifier) => mangle_struct_item_name(struct_id, identifier),
            Self::Extern(identifier) => nomangle_identifier(identifier),
        }
    }

    pub(crate) const fn local(&self) -> Identifier {
        match self {
            Self::InModule(_, identifier)
            | Self::InStruct(_, identifier)
            | Self::Extern(identifier) => *identifier,
        }
    }

    // TODO this is a hack, remove ASAP
    pub(crate) fn fqname(&self) -> crate::identifier::FQName {
        match self {
            Self::InModule(module_id, identifier) => module_id.fqname().with_part(*identifier),
            Self::InStruct(struct_id, identifier) => struct_id.fqname().with_part(*identifier),
            Self::Extern(identifier) => FQName::from_identifier(*identifier),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct InstantiatedFunctionId(pub FunctionId, pub TypeArgumentValues);

impl InstantiatedFunctionId {
    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        self.0.into_mangled().with_types(&self.1)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub module_name: ModuleId,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub position: ast::SourceSpan,
    pub visibility: Visibility,
}

impl Function {
    pub(crate) fn type_(&self) -> Type {
        // TODO the function may actually have type arguments, so we need to consider that case
        // here
        Type {
            kind: TypeKind::Callable(self.id),
            argument_values: TypeArgumentValues::new_empty(),
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        Self {
            id: self.id,
            module_name: self.module_name,
            arguments: self
                .arguments
                .iter()
                .map(|x| x.instantiate(type_argument_values))
                .collect(),
            return_type: self.return_type.instantiate(type_argument_values),
            body: self.body.instantiate(type_argument_values),
            position: self.position,
            visibility: self.visibility,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Extern(Identifier),
    Statements(Vec<Statement>),
}
impl FunctionBody {
    fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        match self {
            Self::Extern(identifier) => Self::Extern(*identifier),
            Self::Statements(statements) => Self::Statements(
                statements
                    .iter()
                    .map(|x| x.instantiate(type_argument_values))
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub type_: Type,
    pub position: ast::SourceSpan,
}
impl Argument {
    pub(crate) fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        Self {
            name: self.name,
            type_: self.type_.instantiate(type_argument_values),
            position: self.position,
        }
    }
}

impl std::hash::Hash for Argument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.name, &self.type_).hash(state);
    }
}

impl Eq for Argument {}
impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        (&self.name, &self.type_) == (&other.name, &other.type_)
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}
