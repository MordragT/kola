use std::{borrow::Cow, marker::PhantomData};

use kola_utils::interner::{Interner, Key};
use serde::{
    Deserialize, Serialize,
    de::{DeserializeSeed, VariantAccess, Visitor},
    ser::{SerializeMap, SerializeSeq},
};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum KindProtocol {
    #[default]
    Type,
    Row,
    Label,
}

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

#[macro_export]
macro_rules! ty {
    // Simple type literals
    (Unit) => { $crate::TypeProtocol::UNIT };
    (Bool) => { $crate::TypeProtocol::BOOL };
    (Num) => { $crate::TypeProtocol::NUM };
    (Char) => { $crate::TypeProtocol::CHAR };
    (Str) => { $crate::TypeProtocol::STR };

    // Compound: (List InnerType)
    ((List $inner:tt)) => { $crate::TypeProtocol::list(ty!($inner)) };

    // Type variables — numeric literal
    ($var:literal) => { $crate::TypeProtocol::var($var, $crate::KindProtocol::Type) };

    // Witnesses
    ((TypeWit $inner:literal)) => {
        $crate::TypeProtocol::witness($crate::TypeProtocol::var($inner, $crate::KindProtocol::Type))
    };
    ((LabelWit $inner:literal)) => {
        $crate::TypeProtocol::witness($crate::TypeProtocol::var($inner, $crate::KindProtocol::Label))
    };

    // Record types  `{ "field": Type, … }`
    ({ $($field:literal : $field_type:tt),* $(,)? }) => {
        $crate::TypeProtocol::record(::std::vec![
            $((::std::string::String::from($field), ty!($field_type))),*
        ])
    };

    // Variant types  `[ "Tag": Type, … ]`
    ([ $($variant:literal : $variant_type:tt),* $(,)? ]) => {
        $crate::TypeProtocol::variant(::std::vec![
            $((::std::string::String::from($variant), ty!($variant_type))),*
        ])
    };

    // Function type  `Input -> Output`
    (($left:tt -> $right:tt)) => {
        $crate::TypeProtocol::func(
            ty!($left),
            ty!($right),
        )
    };
}

/// The inner type representation, referencing strings via their safe `u32` indices into the symbol table.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeVariant {
    Unit,
    Bool,
    Num,
    Char,
    Str,
    List(Box<Self>),
    // (local_key_idx, child_table_offset, child_variant)
    Record(Vec<(u32, u32, Self)>),
    // (local_tag_idx, child_table_offset, child_variant)
    Variant(Vec<(u32, u32, Self)>),
    // (input_variant, output_table_offset, output_variant)
    Func(Box<Self>, u32, Box<Self>),
    Label(u32),
    Var(u32, KindProtocol),
    Witness(Box<Self>),
}

impl TypeVariant {
    fn validate_internal(
        &self,
        value: &ValueProtocol,
        table: &[String],
        base_offset: u32,
    ) -> Result<(), String> {
        match (self, value) {
            (TypeVariant::Unit, ValueProtocol::Unit) => Ok(()),
            (TypeVariant::Bool, ValueProtocol::Bool(_)) => Ok(()),
            (TypeVariant::Num, ValueProtocol::Num(_)) => Ok(()),
            (TypeVariant::Char, ValueProtocol::Char(_)) => Ok(()),
            (TypeVariant::Str, ValueProtocol::Str(_)) => Ok(()),
            (TypeVariant::List(inner_ty), ValueProtocol::List(values)) => {
                for v in values {
                    inner_ty.validate_internal(v, table, base_offset)?;
                }
                Ok(())
            }
            (TypeVariant::Record(type_fields), ValueProtocol::Record(val_fields)) => {
                if type_fields.len() != val_fields.len() {
                    return Err(format!(
                        "Field count mismatch: expected {}, got {}",
                        type_fields.len(),
                        val_fields.len()
                    ));
                }

                let mut val_fields = val_fields.clone();
                val_fields.sort_unstable_by(|a, b| a.0.cmp(&b.0));

                for ((key_idx, child_offset, type_ty), (val_name, val_val)) in
                    type_fields.iter().zip(val_fields.iter())
                {
                    // Apply current base offset to look up our static type string
                    let type_name = &table[(base_offset + *key_idx) as usize];
                    if type_name != val_name {
                        return Err(format!(
                            "Field name mismatch: expected '{}', got '{}'",
                            type_name, val_name
                        ));
                    }

                    // Push the base offset forward for this child's independent sub-table slice
                    type_ty.validate_internal(val_val, table, base_offset + *child_offset)?;
                }
                Ok(())
            }
            (TypeVariant::Variant(type_variants), ValueProtocol::Variant(tag, val)) => {
                let target = type_variants
                    .iter()
                    .find(|(tag_idx, _, _)| &table[(base_offset + *tag_idx) as usize] == tag);

                match target {
                    Some((_, child_offset, ty)) => {
                        ty.validate_internal(val, table, base_offset + *child_offset)
                    }
                    None => Err(format!("Unknown variant: '{}'", tag)),
                }
            }
            _ => Err(format!(
                "Type mismatch: expected {:?}, got {:?}",
                self, value
            )),
        }
    }
}

/// The outer protocol envelope containing the unified string table and the indexed type tree
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeProtocol {
    pub table: Vec<String>,
    pub ty: TypeVariant,
}

impl TypeProtocol {
    pub const UNIT: Self = TypeProtocol {
        table: Vec::new(),
        ty: TypeVariant::Unit,
    };
    pub const BOOL: Self = TypeProtocol {
        table: Vec::new(),
        ty: TypeVariant::Bool,
    };
    pub const NUM: Self = TypeProtocol {
        table: Vec::new(),
        ty: TypeVariant::Num,
    };
    pub const CHAR: Self = TypeProtocol {
        table: Vec::new(),
        ty: TypeVariant::Char,
    };
    pub const STR: Self = TypeProtocol {
        table: Vec::new(),
        ty: TypeVariant::Str,
    };

    // Trivial wrappers just pass the inner table straight through!
    pub fn list(inner: Self) -> Self {
        Self {
            table: inner.table,
            ty: TypeVariant::List(Box::new(inner.ty)),
        }
    }

    pub fn witness(inner: Self) -> Self {
        Self {
            table: inner.table,
            ty: TypeVariant::Witness(Box::new(inner.ty)),
        }
    }

    pub fn func(input: Self, output: Self) -> Self {
        let mut table = input.table;
        let output_offset = table.len() as u32;
        table.extend(output.table);

        Self {
            table,
            ty: TypeVariant::Func(Box::new(input.ty), output_offset, Box::new(output.ty)),
        }
    }

    pub fn record(fields: Vec<(String, Self)>) -> Self {
        let mut combined_table = Vec::new();
        let mut indexed_fields = Vec::with_capacity(fields.len());

        // 1. Reserve the first slots in our table for this record's local keys
        for (name, _) in &fields {
            combined_table.push(name.clone());
        }

        // 2. Append child tables and record their relative offsets
        let mut current_child_offset = combined_table.len() as u32;
        for (key_idx, (_, field_proto)) in fields.into_iter().enumerate() {
            let child_table_len = field_proto.table.len() as u32;
            combined_table.extend(field_proto.table);

            indexed_fields.push((key_idx as u32, current_child_offset, field_proto.ty));
            current_child_offset += child_table_len;
        }

        // 3. Pre-sort fields alphabetically using our local keys for O(N) validation
        indexed_fields
            .sort_by(|a, b| combined_table[a.0 as usize].cmp(&combined_table[b.0 as usize]));

        Self {
            table: combined_table,
            ty: TypeVariant::Record(indexed_fields),
        }
    }

    pub fn variant(variants: Vec<(String, Self)>) -> Self {
        let mut combined_table = Vec::new();
        let mut indexed_variants = Vec::with_capacity(variants.len());

        for (name, _) in &variants {
            combined_table.push(name.clone());
        }

        let mut current_child_offset = combined_table.len() as u32;
        for (tag_idx, (_, var_proto)) in variants.into_iter().enumerate() {
            let child_table_len = var_proto.table.len() as u32;
            combined_table.extend(var_proto.table);

            indexed_variants.push((tag_idx as u32, current_child_offset, var_proto.ty));
            current_child_offset += child_table_len;
        }

        indexed_variants
            .sort_by(|a, b| combined_table[a.0 as usize].cmp(&combined_table[b.0 as usize]));

        Self {
            table: combined_table,
            ty: TypeVariant::Variant(indexed_variants),
        }
    }

    pub fn label(label: impl Into<String>) -> Self {
        Self {
            table: ::std::vec![label.into()],
            ty: TypeVariant::Label(0),
        }
    }

    pub fn var(id: u32, kind: KindProtocol) -> Self {
        Self {
            table: Vec::new(),
            ty: TypeVariant::Var(id, kind),
        }
    }

    pub fn as_label(&self) -> Option<&str> {
        if let TypeVariant::Label(idx) = self.ty {
            Some(&self.table[idx as usize])
        } else {
            None
        }
    }

    pub fn to_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(self)
    }

    pub fn from_json(json: &str) -> serde_json::Result<Self> {
        serde_json::from_str(json)
    }

    pub fn validate_value(&self, value: &ValueProtocol) -> Result<(), String> {
        // Start validation at root base_offset = 0
        self.ty.validate_internal(value, &self.table, 0)
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
