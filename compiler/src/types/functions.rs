use std::fmt::{Display, Formatter};

use crate::{
    ast,
    identifier::{FQName, Identifier},
    name_mangler::{MangledIdentifier, nomangle_identifier},
};

use super::{Statement, Type, TypeArgumentValues, TypeArguments, TypeKind, Visibility};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionId {
    FQName(FQName),
    Extern(Identifier),
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FQName(fqname) => write!(f, "FQName({fqname})"),
            Self::Extern(identifier) => write!(f, "Extern({identifier})"),
        }
    }
}

impl FunctionId {
    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        match self {
            Self::FQName(fqname) => fqname.into_mangled(),
            Self::Extern(identifier) => nomangle_identifier(identifier),
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
    // TODO create types::modules::ModuleId and use it here
    pub module_name: FQName,
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
    fn instantiate(&self, _type_argument_values: &TypeArgumentValues) -> Self {
        // TODO actually make sure the generic types are replaced!
        self.clone()
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
