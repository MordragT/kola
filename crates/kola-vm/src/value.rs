use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_builtins::BuiltinId;
use kola_collections::{ImShadowMap, ImVec};
use kola_ir::instr::{Func, Tag};
use kola_utils::{
    fmt::DisplayWithInterner,
    interner::{StrInterner, StrKey},
};
use std::fmt;

use crate::{cont::Cont, env::Env};

/// Values produced by evaluating expressions
#[derive(Debug, EnumAsInner, From, Clone, PartialEq)]
pub enum Value {
    /// A unit value (no value)
    None,
    /// A boolean value
    Bool(bool),
    /// A character value
    Char(char),
    /// A numeric value (floating point)
    Num(f64),
    /// A string value
    Str(String),
    /// A function closure (environment, function definition)
    Closure(Closure),
    /// A captured continuation
    Cont(Cont),
    /// A built-in function (e.g., `__builtin_first`)
    Builtin(BuiltinId),
    /// A tag
    Tag(Tag),
    /// A variant (tag, value)
    Variant(Variant),
    /// A record (map of labels to values)
    Record(Record),
    /// A list of values
    List(List),
}

impl Value {
    pub fn str(s: impl Into<String>) -> Self {
        Value::Str(s.into())
    }

    pub fn variant(tag: Tag, value: Self) -> Self {
        Value::Variant(Variant::new(tag, value))
    }
}

impl DisplayWithInterner for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        match self {
            Value::None => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Cont(_) => write!(f, "<continuation>"),
            Value::Builtin(b) => write!(f, "{}", b),
            Value::Tag(t) => t.fmt(f, interner),
            Value::Variant(v) => v.fmt(f, interner),
            Value::Record(r) => r.fmt(f, interner),
            Value::List(l) => l.fmt(f, interner),
        }
    }
}

#[derive(Debug, From, Clone, PartialEq)]
pub struct Closure {
    pub env: Env,
    pub func: Func,
}

impl Closure {
    #[inline]
    pub fn new(env: Env, func: Func) -> Self {
        Self { env, func }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    tag: Tag,
    value: Box<Value>,
}

impl Variant {
    #[inline]
    pub fn new(tag: Tag, value: Value) -> Self {
        Self {
            tag,
            value: Box::new(value),
        }
    }

    #[inline]
    pub fn tag(&self) -> Tag {
        self.tag
    }

    #[inline]
    pub fn value(&self) -> &Value {
        &self.value
    }
}

impl DisplayWithInterner for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        self.tag.fmt(f, interner)?;
        write!(f, "(")?;
        self.value.fmt(f, interner)?;
        write!(f, ")")
    }
}

#[derive(Debug, From, Clone, PartialEq)]
pub struct Record(ImShadowMap<StrKey, Value>);

impl Record {
    #[inline]
    pub fn new() -> Self {
        Self(ImShadowMap::new())
    }

    #[inline]
    pub fn unit(label: StrKey, value: Value) -> Self {
        let mut record = Self::new();
        record.insert(label, value);
        record
    }

    #[inline]
    pub fn insert(&mut self, key: StrKey, value: Value) {
        self.0.insert(key, value);
    }

    #[inline]
    pub fn get(&self, key: StrKey) -> Option<&Value> {
        self.0.get(&key)
    }

    #[inline]
    pub fn remove(&mut self, key: StrKey) -> Option<Value> {
        self.0.remove(&key)
    }

    #[inline]
    pub fn contains_key(&self, key: StrKey) -> bool {
        self.0.contains_key(&key)
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = StrKey> {
        self.0.keys().copied()
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.0.values()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Extend a record at the specified field path
    pub fn extend_at_path(&mut self, field_path: &[StrKey], value: Value) -> Result<(), String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: extend at this level
            self.insert(field_path[0], value);
            Ok(())
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let nested = self
                .get(label)
                .cloned()
                .unwrap_or_else(|| Value::Record(Record::new()));
            let Value::Record(mut nested_record) = nested else {
                return Err(format!("Cannot extend non-record at field path"));
            };
            nested_record.extend_at_path(&field_path[1..], value)?;
            self.insert(label, Value::Record(nested_record));
            Ok(())
        }
    }

    /// Restrict (remove) a field at the specified field path
    pub fn restrict_at_path(&mut self, field_path: &[StrKey]) -> Result<(), String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: remove at this level
            self.remove(field_path[0]);
            Ok(())
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let Some(nested) = self.get(label).cloned() else {
                return Err(format!("Cannot restrict non-existing field"));
            };
            let Value::Record(mut nested_record) = nested else {
                return Err(format!("Cannot restrict non-record at field path"));
            };
            nested_record.restrict_at_path(&field_path[1..])?;
            self.insert(label, Value::Record(nested_record));
            Ok(())
        }
    }

    /// Update a field at the specified field path using an update function
    pub fn update_at_path<F>(&mut self, field_path: &[StrKey], update_fn: F) -> Result<(), String>
    where
        F: FnOnce(Value) -> Result<Value, String>,
    {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: update at this level
            let label = field_path[0];
            let Some(current_val) = self.get(label).cloned() else {
                return Err(format!("Cannot update non-existing field"));
            };
            let new_value = update_fn(current_val)?;
            self.insert(label, new_value);
            Ok(())
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let Some(nested) = self.get(label).cloned() else {
                return Err(format!("Cannot update non-existing field"));
            };
            let Value::Record(mut nested_record) = nested else {
                return Err(format!("Cannot update non-record at field path"));
            };
            nested_record.update_at_path(&field_path[1..], update_fn)?;
            self.insert(label, Value::Record(nested_record));
            Ok(())
        }
    }
}

impl DisplayWithInterner for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (key, value) in self.0.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{} = ", interner[*key])?;
            value.fmt(f, interner)?;
            first = false;
        }
        write!(f, "}}")
    }
}

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
    pub fn reverse(&self) -> Self {
        Self(self.0.iter().cloned().rev().collect())
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.0.iter()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(ImVec::from_iter(iter))
    }
}

impl DisplayWithInterner for List {
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
