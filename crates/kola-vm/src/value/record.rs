use std::fmt;

use derive_more::From;
use kola_collections::ShadowMap;
use kola_utils::{
    interner::{StrInterner, StrKey},
    interner_ext::{DisplayWithInterner, InternerExt, SerializeWithInterner},
};
use serde::ser::SerializeMap;

use super::Value;

#[derive(Debug, From, Clone, PartialEq)]
pub struct Record(ShadowMap<StrKey, Value>);

impl Record {
    #[inline]
    pub fn new() -> Self {
        Self(ShadowMap::new())
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
    pub fn remove(&mut self, key: StrKey) -> Option<Value> {
        self.0.remove(&key)
    }

    #[inline]
    pub fn get(&self, key: StrKey) -> Option<&Value> {
        self.0.get(&key)
    }

    #[inline]
    pub fn first(&self) -> Option<(&StrKey, &Value)> {
        self.0.first().map(|(k, v)| (k, v))
    }

    #[inline]
    pub fn pop_first(&mut self) -> Option<(StrKey, Value)> {
        self.0.pop_first()
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

    /// Returns the number of visible entries in the record.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.keys().count()
    }

    /// Essentially a concatenation of two records.
    /// Through the shadow semantics, the left record's entries will take precedence over the right's
    #[inline]
    pub fn merge_left(self, other: Self) -> Self {
        Self(self.0.merge_left(other.0))
    }

    /// Essentially a concatenation of two records.
    /// Through the shadow semantics, the right record's entries will take precedence over the left's
    #[inline]
    pub fn merge_right(self, other: Self) -> Self {
        Self(self.0.merge_right(other.0))
    }

    /// 1. r & {} or {} & r = r
    /// 2. {a: t | r} & {a: u | s} = {a: (t & u) | (r & s)}
    /// 3. {a : t | r} & {b: u | s} = {a: t | b : u | (u & r)} if a != b
    pub fn merge(mut self, mut other: Self) -> Option<Self> {
        if self.is_empty() {
            return Some(other);
        }

        if other.is_empty() {
            return Some(self);
        }

        // TODO I think due to that both records are ordered,
        // the insert operations which does a binary search can be optimized,
        // by just pushing onto the inner Vec.

        let mut merged = Self::new();

        while let Some(((left_label, left_value), (right_label, right_value))) =
            self.pop_first().zip(other.pop_first())
        {
            if left_label == right_label {
                // Same label, merge values
                let (Value::Record(left), Value::Record(right)) = (left_value, right_value) else {
                    return None; // Cannot merge non-record values
                };

                let merged_value = left.merge(right)?;
                merged.insert(left_label, Value::Record(merged_value));
            } else {
                // Different labels, insert both
                merged.insert(left_label, left_value);
                merged.insert(right_label, right_value);
            }
        }

        // Insert any remaining values from either record
        if !self.is_empty() {
            for (label, value) in self.0 {
                merged.insert(label, value);
            }
        } else if !other.is_empty() {
            for (label, value) in other.0 {
                merged.insert(label, value);
            }
        }

        Some(merged)
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

impl DisplayWithInterner<str> for Record {
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

impl SerializeWithInterner<str> for Record {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(None)?;
        for (key, value) in &self.0 {
            map.serialize_entry(&interner[*key], &interner.with(value))?;
        }
        map.end()
    }
}
