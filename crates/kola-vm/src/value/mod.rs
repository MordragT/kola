use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_builtins::BuiltinId;
use kola_ir::instr::Tag;
use kola_utils::{
    interner::StrInterner,
    interner_ext::{DisplayWithInterner, SerializeWithInterner},
};
use serde::Deserialize;
use std::fmt;

use crate::cont::Cont;

mod closure;
mod list;
mod record;
mod variant;

pub use closure::Closure;
pub use list::List;
pub use record::Record;
pub use variant::Variant;

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

impl DisplayWithInterner<str> for Value {
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

impl SerializeWithInterner<str> for Value {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Value::None => serializer.serialize_unit(),
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::Char(c) => serializer.serialize_char(*c),
            Value::Num(n) => serializer.serialize_f64(*n),
            Value::Str(s) => serializer.serialize_str(s),
            Value::Variant(v) => v.serialize(serializer, interner),
            Value::Record(r) => r.serialize(serializer, interner),
            Value::List(l) => l.serialize(serializer, interner),
            _ => Err(serde::ser::Error::custom(
                "Cannot serialize this Value variant",
            )),
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

pub fn to_usize_exact(value: f64) -> Option<usize> {
    // 1. Check for special floating-point values (NaN, Inf)
    if !value.is_finite() {
        // is_finite() checks for NaN, +/- infinity
        return None;
    }

    // 2. Check if the number is an exact integer
    // `floor()` is used here. If value is 10.0, floor is 10.0. If value is 10.7, floor is 10.0.
    // So `value == value.floor()` is true ONLY if value is an exact integer.
    if value != value.floor() {
        return None; // Not an exact integer
    }

    // 3. Check if the number is positive (since usize is unsigned)
    if value < 0.0 {
        return None; // Negative number
    }

    // 4. Check for overflow against usize::MAX
    // Note: f64 can represent all integers up to 2^53 exactly.
    // usize::MAX depends on the architecture (32-bit, 64-bit).
    // So check for explicit overflow.
    if value > usize::MAX as f64 {
        return None; // Value is too large for usize
    }

    // 5. If all checks pass, it's safe to cast.
    // The cast `as usize` is then safe because we've covered all edge cases.
    Some(value as usize)
}
