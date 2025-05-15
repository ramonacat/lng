use std::fmt::Formatter;

use itertools::Itertools as _;

use crate::identifier::Identifier;

use super::store::TypeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeArguments(Vec<TypeArgument>);
impl TypeArguments {
    pub(crate) const fn new_empty() -> Self {
        Self(vec![])
    }

    pub(crate) const fn new(arguments: Vec<TypeArgument>) -> Self {
        Self(arguments)
    }

    pub(crate) fn with_values(&self, argument_values: Vec<TypeId>) -> Self {
        let mut arguments = self.0.clone();

        for (i, value) in argument_values.into_iter().enumerate() {
            arguments[i].1 = Some(value);
        }

        Self(arguments)
    }

    pub(crate) fn arguments(&self) -> &[TypeArgument] {
        &self.0[..]
    }

    pub(crate) fn values(&self) -> Vec<Option<TypeId>> {
        self.0.iter().map(|x| x.1).collect()
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeArgument(Identifier, Option<TypeId>);

impl TypeArgument {
    pub const fn new(name: Identifier) -> Self {
        Self(name, None)
    }

    pub const fn new_value(name: Identifier, value: TypeId) -> Self {
        Self(name, Some(value))
    }

    pub const fn name(&self) -> Identifier {
        self.0
    }

    pub const fn value(&self) -> Option<TypeId> {
        self.1
    }
}

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
