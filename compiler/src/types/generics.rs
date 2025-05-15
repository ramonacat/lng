use std::{collections::HashMap, fmt::Formatter};

use itertools::Itertools as _;

use crate::identifier::Identifier;

use super::{AnyType, GenericType, InstantiatedType, TypeError};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeArguments(Vec<TypeArgument>);
impl TypeArguments {
    pub(crate) const fn new_empty() -> Self {
        Self(vec![])
    }

    pub(crate) const fn new(arguments: Vec<TypeArgument>) -> Self {
        Self(arguments)
    }
}

impl std::fmt::Display for TypeArguments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "{}", self.0.iter().map(ToString::to_string).join(","))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeArgument(Identifier);

impl TypeArgument {
    pub const fn new(name: Identifier) -> Self {
        Self(name)
    }
}

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgumentValues<TType: AnyType>(pub(crate) HashMap<TypeArgument, TType>);

impl<TType: AnyType> std::hash::Hash for TypeArgumentValues<TType> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0
            .iter()
            .sorted_by(|x, y| x.0.0.raw().cmp(&y.0.0.raw()))
            .collect_vec()
            .hash(state);
    }
}

impl<TType: AnyType> TypeArgumentValues<TType> {
    pub(crate) fn new_empty() -> Self {
        Self(HashMap::new())
    }

    pub(super) fn get(&self, type_argument: TypeArgument) -> Option<&TType> {
        self.0.get(&type_argument)
    }

    pub(crate) const fn new(tav: HashMap<TypeArgument, TType>) -> Self {
        Self(tav)
    }

    pub(super) fn merge(self, type_argument_values: Self) -> Self {
        let Self(mut values) = self;

        for (name, value) in type_argument_values.0 {
            values.insert(name, value);
        }

        Self(values)
    }
}

impl TypeArgumentValues<GenericType> {
    pub(crate) fn instantiate(
        &self,
        type_argument_values: &TypeArgumentValues<InstantiatedType>,
    ) -> Result<TypeArgumentValues<InstantiatedType>, TypeError> {
        let instantiated_arguments = self
            .0
            .iter()
            .map(|(arg, value)| Ok((*arg, value.instantiate(type_argument_values)?)))
            .collect::<Result<HashMap<_, _>, _>>()?;

        Ok(TypeArgumentValues(instantiated_arguments))
    }
}

impl<TType: AnyType> std::fmt::Display for TypeArgumentValues<TType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(
            f,
            "<{}>",
            self.0
                .iter()
                .map(|(name, value)| format!("{name}={value}"))
                .join(", ")
        )
    }
}
