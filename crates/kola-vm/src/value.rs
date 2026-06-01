use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_builtins::BuiltinId;
use kola_ir::instr::Tag;
use kola_protocol::{TypeProtocol, ValueProtocol};
use kola_utils::{display::DisplayWith, serde::SerializeWith};
use serde::ser::{SerializeMap, SerializeSeq};
use std::fmt;

use crate::{
    closure::Closure, heap::Heap, list::ListIdx, record::RecordIdx, string::StringIdx,
    variant::VariantIdx, witness::WitnessIdx,
};

/// Values produced by evaluating expressions
#[derive(Debug, Default, EnumAsInner, From, Clone, Copy, PartialEq)]
pub enum Value {
    /// A unit value (no value)
    #[default]
    None,
    /// A boolean value
    Bool(bool),
    /// A character value
    Char(char),
    /// A numeric value (floating point)
    Num(f64),
    /// A string value
    Str(Option<StringIdx>),
    /// A function closure (environment, function definition)
    Closure(Closure),
    /// A built-in function (e.g., `__builtin_first`)
    Builtin(BuiltinId),
    /// A tag
    Tag(Tag),
    /// A variant (tag, value)
    Variant(VariantIdx),
    /// A record (map of labels to values)
    Record(Option<RecordIdx>),
    /// A list of values
    List(Option<ListIdx>),
    /// A Type representation
    Witness(WitnessIdx),
}

const _: () = assert!(Value::BYTES <= 16);

impl Value {
    pub const BYTES: usize = std::mem::size_of::<Self>();

    pub fn from_json(
        type_proto: &TypeProtocol,
        json: &str,
        heap: &mut Heap,
    ) -> Result<Self, String> {
        let proto = serde_json::from_str::<ValueProtocol>(json).map_err(|e| e.to_string())?;

        type_proto.validate_value(&proto)?;

        Ok(Self::from_protocol(proto, heap))
    }

    pub fn from_protocol(proto: ValueProtocol, heap: &mut Heap) -> Self {
        match proto {
            ValueProtocol::Unit => Value::None,
            ValueProtocol::Bool(b) => Value::Bool(b),
            ValueProtocol::Char(c) => Value::Char(c),
            ValueProtocol::Num(n) => Value::Num(n),
            ValueProtocol::Str(s) => Value::Str(heap.strings.alloc(&s)),
            ValueProtocol::Variant(t, v) => {
                let tag = Tag(heap.intern_str(t));
                let value = Self::from_protocol(*v, heap);
                Value::Variant(heap.variants.alloc(tag, value))
            }
            ValueProtocol::Record(r) => {
                let pairs = r
                    .into_iter()
                    .map(|(label, value)| {
                        let label = heap.strings.interner.intern(label);
                        let value = Self::from_protocol(value, heap);
                        (label, value)
                    })
                    .collect::<Vec<_>>();

                Value::Record(heap.records.alloc(&pairs))
            }
            ValueProtocol::List(l) => {
                let values = l
                    .into_iter()
                    .map(|v| Self::from_protocol(v, heap))
                    .collect::<Vec<_>>();
                Value::List(heap.lists.alloc(&values))
            }
        }
    }
}

impl DisplayWith<Heap> for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        match self {
            Value::None => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => {
                if let Some(s) = s {
                    s.fmt(f, &heap.strings)
                } else {
                    write!(f, "\"\"")
                }
            }
            Value::Closure(_) => write!(f, "<closure>"),
            // Value::Cont(_) => write!(f, "<continuation>"),
            Value::Builtin(b) => write!(f, "{}", b),
            Value::Tag(t) => t.fmt(f, &heap.strings.interner),
            Value::Variant(v) => v.fmt(f, heap),
            Value::Record(r) => {
                if let Some(r) = r {
                    r.fmt(f, heap)
                } else {
                    write!(f, "{{}}")
                }
            }
            Value::List(l) => {
                if let Some(l) = l {
                    l.fmt(f, heap)
                } else {
                    write!(f, "[]")
                }
            }
            Value::Witness(w) => w.fmt(f, heap),
        }
    }
}

impl SerializeWith<Heap> for Value {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Value::None => serializer.serialize_unit(),
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::Char(c) => serializer.serialize_char(*c),
            Value::Num(n) => serializer.serialize_f64(*n),
            Value::Str(s) => {
                if let Some(s) = s {
                    s.serialize(serializer, &heap.strings)
                } else {
                    serializer.serialize_str("")
                }
            }
            Value::Variant(v) => v.serialize(serializer, heap),
            Value::Record(r) => {
                if let Some(r) = r {
                    r.serialize(serializer, heap)
                } else {
                    let map = serializer.serialize_map(Some(0))?;
                    map.end()
                }
            }
            Value::List(l) => {
                if let Some(l) = l {
                    l.serialize(serializer, heap)
                } else {
                    let seq = serializer.serialize_seq(Some(0))?;
                    seq.end()
                }
            }
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
