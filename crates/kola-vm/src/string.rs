use std::{borrow::Cow, fmt, str::Chars};

use camino::Utf8Path;

use crate::{arenas::StringIndex, heap::Heap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapString(pub StringIndex);

impl HeapString {
    #[inline]
    pub fn get(self, heap: &Heap) -> RawString<'_> {
        heap.get_string(self)
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawString<'a>(pub Cow<'a, str>);

impl<'a> RawString<'a> {
    #[inline]
    pub fn into_owned(self) -> RawString<'static> {
        RawString(Cow::Owned(self.0.into_owned()))
    }

    #[inline]
    pub fn alloc(&self, heap: &mut Heap) -> HeapString {
        heap.alloc_string(self)
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn contains(&self, pat: char) -> bool {
        self.0.contains(pat)
    }

    #[inline]
    pub fn chars(&self) -> Chars<'_> {
        self.0.chars()
    }
}

impl RawString<'static> {
    #[inline]
    pub fn new() -> Self {
        Self(Cow::Borrowed(""))
    }

    #[inline]
    pub fn pop_front(&mut self) -> Option<char> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_mut().remove(0))
        }
    }

    #[inline]
    pub fn pop_back(&mut self) -> Option<char> {
        self.0.to_mut().pop()
    }

    #[inline]
    pub fn push_front(&mut self, c: char) {
        self.0.to_mut().insert(0, c);
    }

    #[inline]
    pub fn push_back(&mut self, c: char) {
        self.0.to_mut().push(c);
    }

    #[inline]
    pub fn prepend(mut self, c: char) -> Self {
        self.0.to_mut().insert(0, c);
        self
    }

    #[inline]
    pub fn append(mut self, c: char) -> Self {
        self.0.to_mut().push(c);
        self
    }

    #[inline]
    pub fn concat(mut self, other: &RawString) -> Self {
        self.0.to_mut().push_str(&other.0);
        self
    }
}

impl From<String> for RawString<'static> {
    fn from(s: String) -> Self {
        Self(Cow::Owned(s))
    }
}

impl<'a> From<&'a str> for RawString<'a> {
    fn from(s: &'a str) -> Self {
        Self(Cow::Borrowed(s))
    }
}

impl From<char> for RawString<'static> {
    fn from(c: char) -> Self {
        Self(Cow::Owned(c.to_string()))
    }
}

impl fmt::Display for RawString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<str> for RawString<'_> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<Utf8Path> for RawString<'_> {
    fn as_ref(&self) -> &Utf8Path {
        Utf8Path::new(&self.0)
    }
}
