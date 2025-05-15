use std::{collections::HashMap, fmt::Display, sync::atomic::AtomicU64};

use super::Type;

static STORE_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(u64, u64);

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type({})", self.0)
    }
}

// TODO this should not be Clone, but there's a hack in the compiler that makes a copy
#[derive(Debug, Clone)]
pub struct SingleStore {
    store_id: u64,
    items: HashMap<TypeId, Type>,
    current_id: u64,
}

pub trait TypeStore {
    fn add(&mut self, type_: Type) -> TypeId;
    fn get(&self, id: TypeId) -> &Type;
}

impl SingleStore {
    pub(crate) fn new() -> Self {
        Self {
            items: HashMap::new(),
            current_id: 0,
            store_id: STORE_ID.fetch_add(1, std::sync::atomic::Ordering::AcqRel),
        }
    }

    const fn next_id(&mut self) -> TypeId {
        self.current_id += 1;

        TypeId(self.store_id, self.current_id)
    }
}

impl TypeStore for SingleStore {
    fn add(&mut self, type_: Type) -> TypeId {
        if let Some((id, _)) = self
            .items
            .iter()
            .find(|(_, found_type)| found_type == &&type_)
        {
            return *id;
        }

        let id = self.next_id();

        self.items.insert(id, type_);

        id
    }

    fn get(&self, id: TypeId) -> &Type {
        assert!(id.0 == self.store_id, "{} {}", id.0, self.store_id);

        self.items.get(&id).unwrap()
    }
}

// TODO remove Clone
#[derive(Debug, Clone)]
pub struct MultiStore {
    default: SingleStore,
    secondary: HashMap<u64, SingleStore>,
}

impl MultiStore {
    pub(crate) fn new(types: SingleStore) -> Self {
        Self {
            default: types,
            secondary: HashMap::new(),
        }
    }

    pub(crate) fn merge_with(&mut self, other: Self) {
        if self.default.store_id != other.default.store_id {
            self.secondary.insert(other.default.store_id, other.default);
        }

        for secondary in other.secondary {
            self.secondary.insert(secondary.0, secondary.1);
        }
    }
}

impl TypeStore for MultiStore {
    fn add(&mut self, type_: Type) -> TypeId {
        self.default.add(type_)
    }

    fn get(&self, id: TypeId) -> &Type {
        dbg!(id, self.default.store_id, self.secondary.keys());

        if id.0 == self.default.store_id {
            return self.default.get(id);
        }

        self.secondary.get(&id.0).unwrap().get(id)
    }
}
