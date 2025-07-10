use std::{borrow::Cow, marker::PhantomData};

use kola_utils::interner::{Interner, Key};
use serde::{
    Deserialize, Serialize,
    de::{DeserializeSeed, VariantAccess, Visitor},
    ser::{SerializeMap, SerializeSeq},
};

pub type TypeInterner = Interner<TypeProtocol>;
pub type TypeKey = Key<TypeProtocol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeSchemeProtocol {
    pub forall: u32,
    pub ty: TypeProtocol,
}

impl TypeSchemeProtocol {
    pub fn new(forall: u32, ty: TypeProtocol) -> Self {
        Self { forall, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeProtocol {
    Unit,
    Bool,
    Num,
    Char,
    Str,
    List(Box<Self>),
    TypeRep(Box<Self>),
    Record(Vec<(String, Self)>),
    Variant(Vec<(String, Self)>),
    Func(Box<Self>, Box<Self>),
    Var(u32),
}

impl TypeProtocol {
    pub const UNIT: Self = TypeProtocol::Unit;
    pub const BOOL: Self = TypeProtocol::Bool;
    pub const NUM: Self = TypeProtocol::Num;
    pub const CHAR: Self = TypeProtocol::Char;
    pub const STR: Self = TypeProtocol::Str;

    pub fn list(inner: Self) -> Self {
        TypeProtocol::List(Box::new(inner))
    }

    pub fn type_rep(inner: Self) -> Self {
        TypeProtocol::TypeRep(Box::new(inner))
    }

    pub fn record(fields: Vec<(String, Self)>) -> Self {
        TypeProtocol::Record(fields)
    }

    pub fn variant(variants: Vec<(String, Self)>) -> Self {
        TypeProtocol::Variant(variants)
    }

    pub fn func(input: Self, output: Self) -> Self {
        TypeProtocol::Func(Box::new(input), Box::new(output))
    }

    pub fn var(id: u32) -> Self {
        TypeProtocol::Var(id)
    }

    pub fn to_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(self)
    }

    pub fn from_json(json: &str) -> serde_json::Result<Self> {
        serde_json::from_str(json)
    }

    pub fn validate_value(&self, value: &ValueProtocol) -> Result<(), String> {
        match (self, value) {
            (TypeProtocol::Unit, ValueProtocol::Unit) => Ok(()),
            (TypeProtocol::Bool, ValueProtocol::Bool(_)) => Ok(()),
            (TypeProtocol::Num, ValueProtocol::Num(_)) => Ok(()),
            (TypeProtocol::Char, ValueProtocol::Char(_)) => Ok(()),
            (TypeProtocol::Str, ValueProtocol::Str(_)) => Ok(()),
            (TypeProtocol::List(inner_ty), ValueProtocol::List(values)) => {
                for v in values {
                    inner_ty.validate_value(v)?
                }
                Ok(())
            }
            (TypeProtocol::Record(type_fields), ValueProtocol::Record(val_fields)) => {
                if type_fields.len() != val_fields.len() {
                    return Err(format!(
                        "Field count mismatch: expected {}, got {}",
                        type_fields.len(),
                        val_fields.len()
                    ));
                }

                // TODO if I could guarantee that the fields are sorted, I could skip sorting here
                // and also skip the clone.

                // Clone and sort both lists by field name for efficient comparison
                let mut type_fields = type_fields.clone();
                let mut val_fields = val_fields.clone();
                type_fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                val_fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));

                for ((type_name, type_ty), (val_name, val_val)) in
                    type_fields.iter().zip(val_fields.iter())
                {
                    if type_name != val_name {
                        return Err(format!(
                            "Field name mismatch: expected '{}', got '{}'",
                            type_name, val_name
                        ));
                    }
                    type_ty.validate_value(val_val)?;
                }
                Ok(())
            }
            (TypeProtocol::Variant(type_variants), ValueProtocol::Variant(tag, val)) => {
                // Find the variant by tag
                match type_variants.iter().find(|(name, _)| name == tag) {
                    Some((_, ty)) => ty.validate_value(val),
                    None => Err(format!("Unknown variant: '{}'", tag)),
                }
            }
            // Fallback for all other mismatches
            _ => Err(format!(
                "Type mismatch: expected {:?}, got {:?}",
                self, value
            )),
        }
    }
}

impl From<TypeProtocol> for Cow<'_, TypeProtocol> {
    fn from(value: TypeProtocol) -> Self {
        Cow::Owned(value)
    }
}

impl<'a> From<&'a TypeProtocol> for Cow<'a, TypeProtocol> {
    fn from(value: &'a TypeProtocol) -> Self {
        Cow::Borrowed(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueProtocol {
    Unit,
    Bool(bool),
    Num(f64),
    Char(char),
    Str(String),
    List(Vec<Self>),
    Record(Vec<(String, Self)>),
    Variant(String, Box<Self>),
}

impl ValueProtocol {
    pub fn unit() -> Self {
        ValueProtocol::Unit
    }

    pub fn bool(value: bool) -> Self {
        ValueProtocol::Bool(value)
    }

    pub fn num(value: f64) -> Self {
        ValueProtocol::Num(value)
    }

    pub fn char(value: char) -> Self {
        ValueProtocol::Char(value)
    }

    pub fn str(value: impl Into<String>) -> Self {
        ValueProtocol::Str(value.into())
    }

    pub fn list(values: Vec<Self>) -> Self {
        ValueProtocol::List(values)
    }

    pub fn record(fields: Vec<(String, Self)>) -> Self {
        ValueProtocol::Record(fields)
    }

    pub fn variant(tag: impl Into<String>, value: Self) -> Self {
        ValueProtocol::Variant(tag.into(), Box::new(value))
    }
}

impl Serialize for ValueProtocol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        match self {
            ValueProtocol::Unit => serializer.serialize_unit(),
            ValueProtocol::Bool(v) => serializer.serialize_bool(*v),
            ValueProtocol::Num(v) => serializer.serialize_f64(*v),
            ValueProtocol::Char(v) => serializer.serialize_char(*v),
            ValueProtocol::Str(v) => serializer.serialize_str(v),
            ValueProtocol::List(values) => {
                let mut seq = serializer.serialize_seq(Some(values.len()))?;
                for value in values {
                    seq.serialize_element(value)?;
                }
                seq.end()
            }
            ValueProtocol::Record(fields) => {
                let mut map = serializer.serialize_map(Some(fields.len()))?;
                for (key, value) in fields {
                    map.serialize_entry(key, value)?;
                }
                map.end()
            }
            ValueProtocol::Variant(tag, value) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry(tag, value)?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for ValueProtocol {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_any(ValueVisitor)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueVisitor;

impl<'de> Visitor<'de> for ValueVisitor {
    type Value = ValueProtocol;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a valid value protocol")
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Unit)
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Bool(v))
    }

    // TODO number conversion errors

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Num(v as f64))
    }

    fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Num(v as f64))
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Num(v as f64))
    }

    fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Num(v as f64))
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Num(v))
    }

    fn visit_char<E>(self, v: char) -> Result<Self::Value, E> {
        Ok(ValueProtocol::Char(v))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
        Ok(ValueProtocol::str(v))
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(ValueProtocol::variant("None", ValueProtocol::Unit))
    }

    fn visit_some<D>(self, value: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let value = value.deserialize_any(self)?;
        Ok(ValueProtocol::variant("Some", value))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut values = Vec::new();
        while let Some(value) = seq.next_element_seed(self)? {
            values.push(value);
        }
        Ok(ValueProtocol::list(values))
    }

    fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        let mut fields = Vec::new();
        while let Some((key, value)) = map.next_entry_seed(PhantomData, self)? {
            fields.push((key, value));
        }
        Ok(ValueProtocol::record(fields))
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::EnumAccess<'de>,
    {
        let (tag, value_access) = data.variant::<String>()?;
        let value = value_access.newtype_variant_seed(self)?;

        Ok(ValueProtocol::variant(tag, value))
    }
}

impl<'de> DeserializeSeed<'de> for ValueVisitor {
    type Value = ValueProtocol;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}
