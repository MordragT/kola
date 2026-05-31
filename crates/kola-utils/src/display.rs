use std::fmt;

use crate::interner::{PathInterner, PathKey, StrInterner, StrKey};

pub trait DisplayWith<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, t: &T) -> fmt::Result;
}

impl DisplayWith<StrInterner> for StrKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

impl DisplayWith<PathInterner> for PathKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &PathInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}
