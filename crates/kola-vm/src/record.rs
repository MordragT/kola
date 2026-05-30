use std::{borrow::Cow, fmt};

use kola_utils::{
    interner::{StrInterner, StrKey},
    interner_ext::{DisplayWithInterner, InternerExt, SerializeWithInterner},
};

use crate::{arenas::RangeIdx, heap::Heap, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapRecord(pub RangeIdx<(StrKey, Value)>);

impl HeapRecord {
    pub fn get(self, heap: &Heap) -> RawRecord<'_> {
        heap.get_record(self)
    }
}

/// A record is an ordered sequence of `(StrKey, Value)` pairs sorted by key.
///
/// Duplicate keys are allowed (shadow semantics): the most recently inserted
/// value for a key appears first in sorted order. `get()` returns the first
/// match, so later inserts shadow earlier ones.
///
/// Shadowed entries are preserved: if the visible entry is removed, the next
/// one resurfaces.
#[derive(Debug, Clone, PartialEq)]
pub struct RawRecord<'a>(pub Cow<'a, [(StrKey, Value)]>);

// ——— read-only methods (any lifetime) ———

impl<'a> RawRecord<'a> {
    #[inline]
    pub fn new() -> Self {
        Self(Cow::Borrowed(&[]))
    }

    /// Convert to an owned, `'static` version (clones only if borrowed).
    #[inline]
    pub fn into_owned(self) -> RawRecord<'static> {
        RawRecord(Cow::Owned(self.0.into_owned()))
    }

    #[inline]
    pub fn alloc(&self, heap: &mut Heap) -> HeapRecord {
        heap.alloc_record(self)
    }

    /// Binary search for the first occurrence of `key`.
    /// Returns the index if found, or the insertion point if not.
    #[inline]
    fn search(&self, key: &StrKey) -> Result<usize, usize> {
        match self.0.binary_search_by_key(key, |(k, _)| *k) {
            Ok(mut idx) => {
                // Walk left to find the *first* occurrence (most recently inserted)
                while idx > 0 && self.0[idx - 1].0 == *key {
                    idx -= 1;
                }
                Ok(idx)
            }
            Err(idx) => Err(idx),
        }
    }

    #[inline]
    pub fn get(&self, key: StrKey) -> Option<&Value> {
        self.search(&key).ok().map(|idx| &self.0[idx].1)
    }

    #[inline]
    pub fn contains_key(&self, key: StrKey) -> bool {
        self.search(&key).is_ok()
    }

    #[inline]
    pub fn first(&self) -> Option<(&StrKey, &Value)> {
        self.0.first().map(|(k, v)| (k, v))
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the total number of entries (including shadowed).
    #[inline]
    pub fn len_all(&self) -> usize {
        self.0.len()
    }

    /// Returns an iterator over all entries (including shadowed).
    #[inline]
    pub fn iter_all(&self) -> impl Iterator<Item = &(StrKey, Value)> {
        self.0.iter()
    }

    /// Returns an iterator over visible (non-shadowed) entries.
    ///
    /// For keys that appear multiple times, only the first occurrence
    /// (the most recently inserted value) is yielded.
    pub fn iter(&self) -> impl Iterator<Item = (&StrKey, &Value)> {
        let mut seen = std::collections::HashSet::new();
        self.0.iter().filter_map(
            move |(k, v)| {
                if seen.insert(*k) { Some((k, v)) } else { None }
            },
        )
    }

    /// Returns the number of visible (non-shadowed) entries.
    #[inline]
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = StrKey> {
        self.iter().map(|(k, _)| *k)
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.iter().map(|(_, v)| v)
    }
}

// ——— mutating methods (only on `'static` / owned data) ———

impl RawRecord<'static> {
    /// Insert a key-value pair at the correct sorted position.
    ///
    /// If the key already exists, this new value is inserted *before* the
    /// existing one, shadowing it.
    #[inline]
    pub fn insert(&mut self, key: StrKey, value: Value) {
        let idx = match self.search(&key) {
            Ok(idx) => idx,  // insert before first existing occurrence → shadows it
            Err(idx) => idx, // insert at computed position
        };
        self.0.to_mut().insert(idx, (key, value));
    }

    /// Remove the first (visible) occurrence of `key`, unshadowing the next.
    #[inline]
    pub fn remove(&mut self, key: StrKey) -> Option<Value> {
        let idx = self.search(&key).ok()?;
        Some(self.0.to_mut().remove(idx).1)
    }

    #[inline]
    pub fn pop_first(&mut self) -> Option<(StrKey, Value)> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_mut().remove(0))
        }
    }

    #[inline]
    pub fn unit(label: StrKey, value: Value) -> Self {
        Self(Cow::Owned(vec![(label, value)]))
    }

    /// Merge two records, left-biased: keys from `self` shadow `other`.
    ///
    /// Shadowed entries are preserved — removing the left value makes the
    /// right value resurfaces.
    pub fn merge_left(self, other: RawRecord) -> Self {
        let mut entries = self.0.into_owned();
        let mut other_entries = other.0.into_owned();
        entries.append(&mut other_entries);
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self(Cow::Owned(entries))
    }

    /// Merge two records, right-biased: keys from `other` shadow `self`.
    pub fn merge_right(self, other: Self) -> Self {
        other.merge_left(self)
    }

    /// Deep merge: same key → recursively merge record values.
    ///
    /// 1. `r & {}` or `{} & r` = `r`
    /// 2. `{a: t | r} & {a: u | s}` = `{a: (t & u) | (r & s)}`
    /// 3. `{a: t | r} & {b: u | s}` = `{a: t | b: u | (r & s)}` if `a ≠ b`
    pub fn merge(mut self, mut other: Self) -> Option<Self> {
        if self.is_empty() {
            return Some(other);
        }
        if other.is_empty() {
            return Some(self);
        }

        let mut merged = Self::new();

        while let Some(((left_label, left_value), (right_label, right_value))) =
            self.pop_first().zip(other.pop_first())
        {
            if left_label == right_label {
                let (Value::Record(left_inner), Value::Record(right_inner)) =
                    (left_value, right_value)
                else {
                    return None;
                };
                let merged_value = left_inner.merge(right_inner)?;
                merged.insert(left_label, Value::Record(merged_value));
            } else {
                merged.insert(left_label, left_value);
                merged.insert(right_label, right_value);
            }
        }

        // Drain remaining entries
        while let Some((label, value)) = self.pop_first() {
            merged.insert(label, value);
        }
        while let Some((label, value)) = other.pop_first() {
            merged.insert(label, value);
        }

        Some(merged)
    }

    /// Extend a record at the specified field path.
    pub fn extend_at_path(&mut self, field_path: &[StrKey], value: Value) -> Result<(), String> {
        match field_path {
            [] => Err("Empty field path".to_string()),
            [label] => {
                self.insert(*label, value);
                Ok(())
            }
            [label, rest @ ..] => {
                let nested = self
                    .get(*label)
                    .cloned()
                    .unwrap_or_else(|| Value::Record(RawRecord::new().into_owned()));
                let Value::Record(mut nested_record) = nested else {
                    return Err("Cannot extend non-record at field path".into());
                };
                nested_record.extend_at_path(rest, value)?;
                self.insert(*label, Value::Record(nested_record));
                Ok(())
            }
        }
    }

    /// Restrict (remove) a field at the specified field path.
    pub fn restrict_at_path(&mut self, field_path: &[StrKey]) -> Result<(), String> {
        match field_path {
            [] => Err("Empty field path".to_string()),
            [label] => {
                self.remove(*label);
                Ok(())
            }
            [label, rest @ ..] => {
                let Some(nested) = self.get(*label).cloned() else {
                    return Err("Cannot restrict non-existing field".into());
                };
                let Value::Record(mut nested_record) = nested else {
                    return Err("Cannot restrict non-record at field path".into());
                };
                nested_record.restrict_at_path(rest)?;
                self.insert(*label, Value::Record(nested_record));
                Ok(())
            }
        }
    }

    /// Update a field at the specified field path using an update function.
    pub fn update_at_path<F>(&mut self, field_path: &[StrKey], update_fn: F) -> Result<(), String>
    where
        F: FnOnce(Value) -> Result<Value, String>,
    {
        match field_path {
            [] => Err("Empty field path".to_string()),
            [label] => {
                let Some(current_val) = self.get(*label).cloned() else {
                    return Err("Cannot update non-existing field".into());
                };
                let new_value = update_fn(current_val)?;
                self.remove(*label);
                self.insert(*label, new_value);
                Ok(())
            }
            [label, rest @ ..] => {
                let Some(nested) = self.get(*label).cloned() else {
                    return Err("Cannot update non-existing field".into());
                };
                let Value::Record(mut nested_record) = nested else {
                    return Err("Cannot update non-record at field path".into());
                };
                nested_record.update_at_path(rest, update_fn)?;
                self.remove(*label);
                self.insert(*label, Value::Record(nested_record));
                Ok(())
            }
        }
    }
}

// ——— display / serialize ———

impl<'a> DisplayWithInterner<str> for RawRecord<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (key, value) in self.iter() {
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

impl<'a> SerializeWithInterner<str> for RawRecord<'a> {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(self.len()))?;
        for (key, value) in self.iter() {
            map.serialize_entry(&interner[*key], &interner.with(value))?;
        }
        map.end()
    }
}
