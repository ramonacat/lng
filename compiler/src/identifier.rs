use itertools::Itertools as _;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::LazyLock;
use std::sync::RwLock;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Identifier(SymbolU32);

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier({self})")
    }
}

static IDENTIFIERS: LazyLock<RwLock<StringInterner<StringBackend>>> =
    LazyLock::new(|| RwLock::new(StringInterner::default()));

impl Identifier {
    pub fn parse(raw: &str) -> Self {
        let symbol = IDENTIFIERS.write().unwrap().get_or_intern(raw);
        Self(symbol)
    }

    pub(crate) fn raw(self) -> String {
        IDENTIFIERS
            .read()
            .unwrap()
            .resolve(self.0)
            .unwrap()
            .to_string()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw())
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, other: &str) -> bool {
        *self == Self::parse(other)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct FQName(Identifier);

impl std::fmt::Debug for FQName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FQName({self})")
    }
}

impl Display for FQName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parts().iter().map(|x| x.raw()).join("."))
    }
}

impl FQName {
    pub(crate) fn parse(raw: &str) -> Self {
        let interned = Identifier::parse(raw);

        Self(interned)
    }

    pub(crate) fn from_identifier(id: Identifier) -> Self {
        Self::parse(&id.raw())
    }

    pub(crate) fn with_part(self, new_part: Identifier) -> Self {
        let raw = self.0.raw();

        if raw.is_empty() {
            Self::parse(&format!("{new_part}"))
        } else {
            Self::parse(&format!("{raw}.{new_part}"))
        }
    }

    fn parts(self) -> Vec<Identifier> {
        let raw = self.0.raw();

        raw.split('.').map(Identifier::parse).collect()
    }

    pub(crate) fn len(self) -> usize {
        self.parts().len()
    }
}
