use std::fmt::{Display, Formatter};

use crate::{
    ast,
    name_mangler::{MangledIdentifier, nomangle_identifier},
};

use super::{
    FQName, Identifier, Statement, Type, TypeArgumentValues, TypeArguments, TypeKind, Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionId {
    FQName(FQName),
    Extern(Identifier),
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionId::FQName(fqname) => write!(f, "FQName({fqname})"),
            FunctionId::Extern(identifier) => write!(f, "Extern({identifier})"),
        }
    }
}

impl FunctionId {
    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        match self {
            FunctionId::FQName(fqname) => fqname.into_mangled(),
            FunctionId::Extern(identifier) => nomangle_identifier(identifier),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub position: ast::SourceRange,
    pub visibility: Visibility,
}

impl Function {
    pub(crate) fn type_(&self) -> Type {
        // TODO the function may actually have type arguments, so we need to consider that case
        // here
        Type {
            kind: TypeKind::Callable {
                arguments: self.arguments.clone(),
                return_type: Box::new(self.return_type.clone()),
            },
            argument_values: TypeArgumentValues::new_empty(),
            arguments: TypeArguments::new_empty(),
        }
    }

    pub(crate) fn instantiate(&self, type_argument_values: &TypeArgumentValues) -> Self {
        let arguments = self
            .arguments
            .iter()
            .map(|x| x.instantiate(type_argument_values))
            .collect();
        let return_type = self.return_type.instantiate(type_argument_values);

        Self {
            id: self.id,
            visibility: self.visibility,
            arguments,
            return_type,
            // TODO body probably needs to be instantiated as well, as there could be references to
            // type arguments eg. in let statements
            body: self.body.clone(),
            position: self.position,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Extern(Identifier),
    Statements(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub type_: Type,
    pub position: ast::SourceRange,
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
