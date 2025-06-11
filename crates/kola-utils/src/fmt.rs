use std::fmt;

use crate::interner::{StrInterner, StrKey};

pub trait DisplayWithInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result;
}

impl DisplayWithInterner for StrKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

pub struct WithInterner<'a, T> {
    pub value: &'a T,
    pub interner: &'a StrInterner,
}

impl<'a, T> WithInterner<'a, T> {
    pub fn new(value: &'a T, interner: &'a StrInterner) -> Self {
        Self { value, interner }
    }
}

impl<T> fmt::Display for WithInterner<'_, T>
where
    T: DisplayWithInterner,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f, self.interner)
    }
}

pub trait StrInternerExt {
    fn display<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T>
    where
        T: DisplayWithInterner;

    #[inline]
    fn to_string<T>(&self, value: &T) -> String
    where
        T: DisplayWithInterner,
    {
        self.display(value).to_string()
    }
}

impl StrInternerExt for StrInterner {
    fn display<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T>
    where
        T: DisplayWithInterner,
    {
        WithInterner::new(value, self)
    }
}
