use imbl::shared_ptr::RcK;

pub mod bi_map;
pub mod cons;
pub mod im_bi_map;
pub mod im_shadow_map;
pub mod shadow_map;

pub mod im_ord_map {
    pub use imbl::ordmap::{ConsumingIter as IntoIter, Iter};

    pub type ImOrdMap<K, V> = imbl::GenericOrdMap<K, V, crate::Ptr>;
}

pub mod ord_map {
    pub use std::collections::btree_map::{IntoIter, Iter, IterMut};

    pub type OrdMap<K, V> = std::collections::BTreeMap<K, V>;
}

pub mod im_ord_set {
    pub use imbl::ordset::{ConsumingIter as IntoIter, Iter};

    pub type ImOrdSet<T> = imbl::GenericOrdSet<T, crate::Ptr>;
}

pub mod ord_set {
    pub use std::collections::btree_set::{IntoIter, Iter};

    pub type OrdSet<T> = std::collections::BTreeSet<T>;
}

pub mod im_hash_map {
    use std::hash::RandomState;

    pub use imbl::hashmap::{ConsumingIter as IntoIter, Iter, IterMut};

    pub type ImHashMap<K, V, S = RandomState> = imbl::GenericHashMap<K, V, S, crate::Ptr>;
}

pub mod hash_map {
    use std::hash::RandomState;

    pub use std::collections::hash_map::{IntoIter, Iter, IterMut};

    pub type HashMap<K, V, S = RandomState> = std::collections::HashMap<K, V, S>;
}

pub mod im_hash_set {
    use std::hash::RandomState;

    pub use imbl::hashset::{ConsumingIter as IntoIter, Iter};

    pub type ImHashSet<T, S = RandomState> = imbl::GenericHashSet<T, S, crate::Ptr>;
}

pub mod hash_set {
    use std::hash::RandomState;

    pub use std::collections::hash_set::{IntoIter, Iter};

    pub type HashSet<T, S = RandomState> = std::collections::HashSet<T, S>;
}

pub mod im_vec {
    pub use imbl::vector::{ConsumingIter as IntoIter, Iter, IterMut};

    pub type ImVec<T> = imbl::GenericVector<T, crate::Ptr>;
}

pub type Ptr = RcK;

pub use im_bi_map::ImBiMap;
pub use im_hash_map::ImHashMap;
pub use im_hash_set::ImHashSet;
pub use im_ord_map::ImOrdMap;
pub use im_ord_set::ImOrdSet;
pub use im_shadow_map::ImShadowMap;
pub use im_vec::ImVec;

pub use bi_map::BiMap;
pub use cons::{Cons, Nil};
pub use hash_map::HashMap;
pub use hash_set::HashSet;
pub use ord_map::OrdMap;
pub use ord_set::OrdSet;
pub use shadow_map::ShadowMap;
