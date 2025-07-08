use std::fmt;

use derive_more::From;
use kola_collections::ImShadowMap;
use kola_utils::{
    fmt::DisplayWithInterner,
    interner::{StrInterner, StrKey},
};
use serde::{Serialize, ser::SerializeMap};

use super::Value;

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

impl Serialize for Record {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // TODO len is probably wrong because it referes to all items (including overriden ones)
        // Probably better to fix that inside the ImShadowMap
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for (key, value) in &self.0 {
            map.serialize_entry(&key.to_string(), value)?;
        }
        map.end()
    }
}
