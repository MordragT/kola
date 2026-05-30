use std::{borrow::Cow, fmt, vec};

use kola_utils::{
    interner::StrInterner,
    interner_ext::{DisplayWithInterner, InternerExt, SerializeWithInterner},
};
use serde::ser::SerializeSeq;

use crate::{arenas::RangeIdx, heap::Heap, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapList(pub RangeIdx<Value>);

impl HeapList {
    #[inline]
    pub fn get(self, heap: &Heap) -> RawList<'_> {
        heap.get_list(self)
    }

    #[inline]
    pub fn len(self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.0.is_empty()
    }
}

/// A flat list of values backed by a `Cow` slice.
///
/// Borrowed lists are read-only views (e.g. from the heap).
/// Owned lists (`'static`) support mutation and consumption-pattern operations.
#[derive(Debug, Clone, PartialEq)]
pub struct RawList<'a>(pub Cow<'a, [Value]>);

// ——— read-only / constructors (any lifetime) ———

impl<'a> RawList<'a> {
    /// Convert to an owned, `'static` version (clones only if borrowed).
    #[inline]
    pub fn into_owned(self) -> RawList<'static> {
        RawList(Cow::Owned(self.0.into_owned()))
    }

    #[inline]
    pub fn alloc(&self, heap: &mut Heap) -> HeapList {
        heap.alloc_list(self)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }

    #[inline]
    pub fn first(&self) -> Option<&Value> {
        self.0.first()
    }

    #[inline]
    pub fn last(&self) -> Option<&Value> {
        self.0.last()
    }

    #[inline]
    pub fn contains(&self, value: &Value) -> bool {
        self.0.contains(value)
    }

    /// Zero-copy split at `index`. Both halves borrow the original data.
    #[inline]
    pub fn split_at(&self, index: usize) -> (RawList<'_>, RawList<'_>) {
        let (head, tail) = self.0.split_at(index);
        (RawList(Cow::Borrowed(head)), RawList(Cow::Borrowed(tail)))
    }

    #[inline]
    pub fn as_slice(&self) -> &[Value] {
        &self.0
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.0.iter()
    }
}

// ——— mutating / consuming methods (only on owned data) ———

impl RawList<'static> {
    #[inline]
    pub fn new() -> Self {
        Self(Cow::Borrowed(&[]))
    }

    #[inline]
    pub fn unit(value: Value) -> Self {
        Self(Cow::Owned(vec![value]))
    }

    #[inline]
    pub fn push_front(&mut self, value: Value) {
        self.0.to_mut().insert(0, value);
    }

    #[inline]
    pub fn push_back(&mut self, value: Value) {
        self.0.to_mut().push(value);
    }

    /// Prepend a value, consuming the list.
    #[inline]
    pub fn prepend(mut self, value: Value) -> Self {
        self.push_front(value);
        self
    }

    /// Append a value, consuming the list.
    #[inline]
    pub fn append(mut self, value: Value) -> Self {
        self.push_back(value);
        self
    }

    /// Concatenate two lists, consuming both.
    #[inline]
    pub fn concat(mut self, other: Self) -> Self {
        self.0.to_mut().extend(other.0.into_owned());
        self
    }

    /// Split off the first element. Returns `None` if empty.
    ///
    /// Consumes the list. The tail is a new owned list.
    #[inline]
    pub fn split_first(mut self) -> Option<(Value, Self)> {
        let v = self.0.to_mut();
        if v.is_empty() {
            return None;
        }
        let head = v.remove(0);
        Some((head, Self(Cow::Owned(std::mem::take(v)))))
    }

    /// Split off the last element. Returns `None` if empty.
    ///
    /// Consumes the list. The head is a new owned list.
    #[inline]
    pub fn split_last(mut self) -> Option<(Self, Value)> {
        let v = self.0.to_mut();
        let tail = v.pop()?;
        Some((Self(Cow::Owned(std::mem::take(v))), tail))
    }

    /// Reverse the list, consuming it.
    #[inline]
    pub fn reverse(self) -> Self {
        let v: Vec<Value> = self.0.into_owned().into_iter().rev().collect();
        Self(Cow::Owned(v))
    }
}

// ——— conversions ———

impl<'a> IntoIterator for RawList<'a> {
    type Item = Value;
    type IntoIter = vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_owned().into_iter()
    }
}

impl FromIterator<Value> for RawList<'static> {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(Cow::Owned(iter.into_iter().collect()))
    }
}

// ——— display / serialize ———

impl<'a> DisplayWithInterner<str> for RawList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for value in self.iter() {
            if !first {
                write!(f, ", ")?;
            }
            value.fmt(f, interner)?;
            first = false;
        }
        write!(f, "]")
    }
}

impl<'a> SerializeWithInterner<str> for RawList<'a> {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for value in self.iter() {
            seq.serialize_element(&interner.with(value))?;
        }
        seq.end()
    }
}
