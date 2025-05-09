use itertools::Itertools as _;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::LazyLock;
use std::sync::RwLock;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::name_mangler::MangledIdentifier;
use crate::name_mangler::mangle_fq_name;

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
pub struct FQName(SymbolU32);

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

static FQNAMES: LazyLock<RwLock<StringInterner<StringBackend>>> =
    LazyLock::new(|| RwLock::new(StringInterner::default()));

impl FQName {
    // TODO check all usages of parse&from_parts that can be simplified
    pub fn parse(raw: &str) -> Self {
        // TODO do we really want to have a separate interns for FQNames? Or should those just
        // wrap an Identifier?
        let interned = FQNAMES.write().unwrap().get_or_intern(raw);

        Self(interned)
    }

    pub fn from_identifier(id: Identifier) -> Self {
        Self::parse(&id.raw())
    }

    fn from_parts(path: impl Iterator<Item = impl Into<Identifier>>) -> Self {
        Self::parse(&path.map(Into::<Identifier>::into).join("."))
    }

    pub fn with_part(self, new_part: Identifier) -> Self {
        let raw = FQNAMES.read().unwrap().resolve(self.0).unwrap().to_string();

        if raw.is_empty() {
            Self::parse(&format!("{new_part}"))
        } else {
            Self::parse(&format!("{raw}.{new_part}"))
        }
    }

    pub fn parts(self) -> Vec<Identifier> {
        let raw = FQNAMES.read().unwrap().resolve(self.0).unwrap().to_string();

        raw.split('.').map(Identifier::parse).collect()
    }

    pub(crate) fn into_mangled(self) -> MangledIdentifier {
        mangle_fq_name(self)
    }

    pub fn last(self) -> Identifier {
        *self.parts().last().unwrap()
    }

    pub fn without_last(self) -> Self {
        let parts = self.parts();
        let len = parts.len();

        Self::from_parts(&mut parts[..len - 1].iter().copied())
    }

    pub(crate) fn split_first(self) -> (Identifier, Self) {
        let parts = self.parts();
        let (first, rest) = parts.split_first().unwrap();

        (*first, Self::from_parts(rest.iter().copied()))
    }

    pub(crate) fn len(self) -> usize {
        self.parts().len()
    }
}
