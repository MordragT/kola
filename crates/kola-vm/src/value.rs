use derive_more::From;
use kola_builtins::BuiltinId;
use kola_collections::{ImShadowMap, ImVec};
use kola_ir::instr::{Func, Tag};
use kola_utils::{
    as_variant,
    fmt::DisplayWithInterner,
    interner::{StrInterner, StrKey},
};
use std::{fmt, ops::RangeBounds};

use crate::{cont::Cont, env::Env};

/// Values produced by evaluating expressions
#[derive(Debug, From, Clone, PartialEq)]
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
    Func(Env, Func),
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

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Value::Char(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Value::Num(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Value::Func(_, _))
    }

    pub fn is_cont(&self) -> bool {
        matches!(self, Value::Cont(_))
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, Value::Builtin(_))
    }

    pub fn is_tag(&self) -> bool {
        matches!(self, Value::Tag(_))
    }

    pub fn is_variant(&self) -> bool {
        matches!(self, Value::Variant(_))
    }

    pub fn is_record(&self) -> bool {
        matches!(self, Value::Record(_))
    }

    pub fn as_bool(&self) -> Option<bool> {
        as_variant!(self, Self::Bool).copied()
    }

    pub fn as_char(&self) -> Option<char> {
        as_variant!(self, Self::Char).copied()
    }

    pub fn as_num(&self) -> Option<f64> {
        as_variant!(self, Self::Num).copied()
    }

    pub fn as_str(&self) -> Option<&str> {
        as_variant!(self, Self::Str).map(|s| s.as_str())
    }

    pub fn as_func(&self) -> Option<&Func> {
        todo!()
    }

    pub fn as_cont(&self) -> Option<&Cont> {
        as_variant!(self, Self::Cont)
    }

    pub fn as_builtin(&self) -> Option<&BuiltinId> {
        as_variant!(self, Self::Builtin)
    }

    pub fn as_tag(&self) -> Option<&Tag> {
        as_variant!(self, Self::Tag)
    }

    pub fn as_variant(&self) -> Option<&Variant> {
        as_variant!(self, Self::Variant)
    }

    pub fn as_record(&self) -> Option<&Record> {
        as_variant!(self, Self::Record)
    }

    pub fn into_bool(self) -> Option<bool> {
        as_variant!(self, Self::Bool)
    }

    pub fn into_char(self) -> Option<char> {
        as_variant!(self, Self::Char)
    }

    pub fn into_num(self) -> Option<f64> {
        as_variant!(self, Self::Num)
    }

    pub fn into_str(self) -> Option<String> {
        as_variant!(self, Self::Str)
    }

    pub fn into_func(self) -> Option<(Env, Func)> {
        // as_variant!(self, Self::Func)
        todo!()
    }

    pub fn into_cont(self) -> Option<Cont> {
        as_variant!(self, Self::Cont)
    }

    pub fn into_variant(self) -> Option<Variant> {
        as_variant!(self, Self::Variant)
    }

    pub fn into_record(self) -> Option<Record> {
        as_variant!(self, Self::Record)
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
            Value::Func(_, _) => write!(f, "<function>"),
            Value::Cont(_) => write!(f, "<continuation>"),
            Value::Builtin(b) => write!(f, "{}", b),
            Value::Tag(t) => t.fmt(f, interner),
            Value::Variant(v) => v.fmt(f, interner),
            Value::Record(r) => r.fmt(f, interner),
            Value::List(l) => l.fmt(f, interner),
        }
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
    pub fn prepend(&mut self, value: Value) {
        self.0.push_front(value);
    }

    #[inline]
    pub fn append(&mut self, value: Value) {
        self.0.push_back(value);
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }

    // TODO does this panic and if yes is that okay ?
    pub fn split_at(&self, index: usize) -> (Self, Self) {
        let (head, tail) = self.0.clone().split_at(index);
        (Self(head), Self(tail))
    }

    pub fn split_first(&self) -> Option<(Value, Self)> {
        let mut tail = self.0.clone();
        let head = tail.pop_front()?;

        Some((head, Self(tail)))
    }

    pub fn split_last(&self) -> Option<(Self, Value)> {
        let mut tail = self.0.clone();
        let head = tail.pop_back()?;

        Some((Self(tail), head))
    }

    // __builtin_first

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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
