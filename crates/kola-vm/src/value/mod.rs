use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_builtins::BuiltinId;
use kola_ir::instr::Tag;
use kola_protocol::{TypeProtocol, ValueProtocol};
use kola_utils::{
    interner::StrInterner,
    interner_ext::{DisplayWithInterner, SerializeWithInterner},
};
use std::fmt::{self, Display};

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
    /// A Type representation
    TypeRep(TypeProtocol),
}

impl Value {
    pub fn str(s: impl Into<String>) -> Self {
        Value::Str(s.into())
    }

    pub fn variant(tag: Tag, value: Self) -> Self {
        Value::Variant(Variant::new(tag, value))
    }

    pub fn from_json(
        type_proto: &TypeProtocol,
        json: &str,
        interner: &mut StrInterner,
    ) -> Result<Self, String> {
        let proto = serde_json::from_str::<ValueProtocol>(json).map_err(|e| e.to_string())?;

        type_proto.validate_value(&proto)?;

        Ok(Self::from_protocol(proto, interner))
    }

    pub fn from_protocol(proto: ValueProtocol, interner: &mut StrInterner) -> Self {
        match proto {
            ValueProtocol::Unit => Value::None,
            ValueProtocol::Bool(b) => Value::Bool(b),
            ValueProtocol::Char(c) => Value::Char(c),
            ValueProtocol::Num(n) => Value::Num(n),
            ValueProtocol::Str(s) => Value::Str(s),
            ValueProtocol::Variant(t, v) => {
                let tag = Tag(interner.intern(t));
                let value = Self::from_protocol(*v, interner);
                let variant = Variant::new(tag, value);
                Value::Variant(variant)
            }
            ValueProtocol::Record(r) => {
                let mut record = Record::new();

                // TODO if ValueProtocol was guaranteed to be sorted,
                // this manual insertion (and binary searching) could be avoided.

                for (label, value) in r {
                    let label = interner.intern(label);
                    let value = Self::from_protocol(value, interner);
                    record.insert(label, value);
                }

                Value::Record(record)
            }
            ValueProtocol::List(l) => {
                let list = l
                    .into_iter()
                    .map(|v| Self::from_protocol(v, interner))
                    .collect();
                Value::List(list)
            }
        }
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
            Value::TypeRep(proto) => proto.to_json().unwrap().fmt(f),
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
