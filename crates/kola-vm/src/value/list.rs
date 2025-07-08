use std::fmt;

use derive_more::From;
use kola_collections::{ImVec, Ptr, im_vec};
use kola_utils::{
    interner::StrInterner,
    interner_ext::{DisplayWithInterner, InternerExt, SerializeWithInterner},
};
use serde::ser::SerializeSeq;

use super::Value;

#[derive(Debug, From, Clone, PartialEq)]
pub struct List(ImVec<Value>);

impl List {
    #[inline]
    pub fn new() -> Self {
        Self(ImVec::new())
    }

    #[inline]
    pub fn unit(value: Value) -> Self {
        Self(ImVec::unit(value))
    }

    #[inline]
    pub fn push_front(&mut self, value: Value) {
        self.0.push_front(value);
    }

    #[inline]
    pub fn push_back(&mut self, value: Value) {
        self.0.push_back(value);
    }

    #[inline]
    pub fn prepend(&self, value: &Value) -> Self {
        let mut list = self.clone();
        list.push_front(value.clone());

        list
    }

    #[inline]
    pub fn append(&self, value: &Value) -> Self {
        let mut list = self.clone();
        list.push_back(value.clone());

        list
    }

    #[inline]
    pub fn concat(&self, other: &Self) -> Self {
        let mut list = self.0.clone();
        list.extend(other.0.iter().cloned());

        Self(list)
    }

    #[inline]
    pub fn contains(&self, value: &Value) -> bool {
        self.0.contains(value)
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }

    #[inline]
    pub fn first(&self) -> Option<Value> {
        self.0.front().cloned()
    }

    #[inline]
    pub fn last(&self) -> Option<Value> {
        self.0.back().cloned()
    }

    #[inline]
    pub fn split_at(&self, index: usize) -> (Self, Self) {
        let (head, tail) = self.0.clone().split_at(index);
        (Self(head), Self(tail))
    }

    #[inline]
    pub fn split_first(&self) -> Option<(Value, Self)> {
        let mut tail = self.0.clone();
        let head = tail.pop_front()?;

        Some((head, Self(tail)))
    }

    #[inline]
    pub fn split_last(&self) -> Option<(Self, Value)> {
        let mut tail = self.0.clone();
        let head = tail.pop_back()?;

        Some((Self(tail), head))
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
    pub fn reverse(self) -> Self {
        Self(self.0.into_iter().rev().collect())
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.0.iter()
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = im_vec::IntoIter<Value, Ptr>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(ImVec::from_iter(iter))
    }
}

impl DisplayWithInterner<str> for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for value in self.0.iter() {
            if !first {
                write!(f, ", ")?;
            }
            value.fmt(f, interner)?;
            first = false;
        }
        write!(f, "]")
    }
}

impl SerializeWithInterner<str> for List {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for value in &self.0 {
            seq.serialize_element(&interner.with(value))?;
        }
        seq.end()
    }
}
