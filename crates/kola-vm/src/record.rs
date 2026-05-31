use std::{fmt, num::NonZeroU32, range::Range};

use kola_utils::{display::DisplayWith, interner::StrKey, serde::SerializeWith};
use serde::ser::SerializeMap;

use crate::{heap::Heap, value::Value};

/// A `Copy`-friendly index into a `RecordArena`.
///
/// Represents a contiguous range of `(StrKey, Value)` pairs in the arena's
/// backing storage. All arena operations take this index rather than `&[(StrKey, Value)]`,
/// enabling `Value::Record(RecordIdx)` to be `Copy` (8 bytes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecordIdx(Range<NonZeroU32>);

impl RecordIdx {
    #[inline]
    pub fn is_empty(self) -> bool {
        self.0.start == self.0.end
    }

    #[inline]
    pub fn len(self) -> usize {
        (self.0.end.get() - self.0.start.get()) as usize
    }

    #[inline]
    fn start(self) -> usize {
        self.0.start.get() as usize - 1
    }

    #[inline]
    fn end(self) -> usize {
        self.0.end.get() as usize - 1
    }

    #[inline]
    fn make(start: usize, end: usize) -> Self {
        RecordIdx(Range {
            start: NonZeroU32::new((start + 1) as u32).expect("record arena overflow"),
            end: NonZeroU32::new((end + 1) as u32).expect("record arena overflow"),
        })
    }
}

impl DisplayWith<Heap> for RecordIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        let pairs = heap
            .records
            .get(*self)
            .into_iter()
            .map(|(k, v)| (&heap.str_interner[*k], v));
        f.debug_map().entries(pairs).finish()
    }
}

impl SerializeWith<Heap> for RecordIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let pairs = heap.records.get(*self);
        let mut map = serializer.serialize_map(Some(pairs.len()))?;

        for (key, val) in pairs {
            map.serialize_entry(&heap.str_interner[*key], &heap.with(val))?;
        }

        map.end()
    }
}

/// An append-only arena for records.
///
/// All record entries are stored as `(StrKey, Value)` pairs in a `Vec`,
/// sorted by key. Duplicate keys are allowed (shadow semantics): the
/// most recently inserted value for a key appears first.
///
/// Mutating operations (`insert`, `remove`, etc.) create new regions
/// in the arena. The original record is untouched.
#[derive(Debug, Clone)]
pub struct RecordArena {
    data: Vec<(StrKey, Value)>,
}

impl RecordArena {
    #[inline]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    // ── read-only operations ──────────────────────────────────────

    /// Get the slice of entries for the given index.
    #[inline]
    pub fn get(&self, idx: RecordIdx) -> &[(StrKey, Value)] {
        &self.data[idx.start()..idx.end()]
    }

    /// Get the length (including shadowed entries).
    #[inline]
    pub fn len(&self, idx: RecordIdx) -> usize {
        idx.len()
    }

    /// Check if the record is empty.
    #[inline]
    pub fn is_empty(&self, idx: RecordIdx) -> bool {
        idx.is_empty()
    }

    #[inline]
    pub fn binary_search(&self, idx: RecordIdx, key: StrKey) -> Result<usize, usize> {
        let slice = self.get(idx);
        let mut idx = slice.binary_search_by_key(&key, |(k, _)| *k)?;

        while idx > 0 && slice[idx - 1].0 == key {
            // Move left to find the first occurrence of the key (visible entry)
            idx -= 1;
        }

        Ok(idx)
    }

    /// Get the visible value by key (returns the first/shadowing entry).
    #[inline]
    pub fn get_value(&self, idx: RecordIdx, key: StrKey) -> Option<Value> {
        let slice = self.get(idx);
        let mut idx = slice.binary_search_by_key(&key, |(k, _)| *k).ok()?;

        while idx > 0 && slice[idx - 1].0 == key {
            // Move left to find the first occurrence of the key (visible entry)
            idx -= 1;
        }

        Some(slice[idx].1)
    }

    /// Check if the record contains the given key.
    #[inline]
    pub fn contains_key(&self, idx: RecordIdx, key: StrKey) -> bool {
        self.binary_search(idx, key).is_ok()
    }

    /// Get the first (smallest key) entry.
    #[inline]
    pub fn first(&self, idx: RecordIdx) -> Option<(StrKey, Value)> {
        self.get(idx).first().copied()
    }

    /// Get the last (largest key) entry.
    #[inline]
    pub fn last(&self, idx: RecordIdx) -> Option<(StrKey, Value)> {
        self.get(idx).last().copied()
    }

    /// Iterate over all entries (including shadowed).
    #[inline]
    pub fn iter(&self, idx: RecordIdx) -> impl Iterator<Item = (StrKey, Value)> {
        self.get(idx).iter().copied()
    }

    /// Iterate over all keys (including shadowed).
    #[inline]
    pub fn keys(&self, idx: RecordIdx) -> impl Iterator<Item = StrKey> + '_ {
        self.get(idx).iter().map(|(k, _)| *k)
    }

    /// Iterate over all values (including shadowed).
    #[inline]
    pub fn values(&self, idx: RecordIdx) -> impl Iterator<Item = Value> + '_ {
        self.get(idx).iter().map(|(_, v)| *v)
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new record from a slice of entries.
    /// Sorts them by key to maintain the sorted invariant.
    #[inline]
    pub fn alloc(&mut self, entries: &[(StrKey, Value)]) -> RecordIdx {
        let start = self.data.len();
        self.data.extend_from_slice(entries);
        let end = self.data.len();
        self.data[start..end].sort_by_key(|(k, _)| *k);
        RecordIdx::make(start, end)
    }

    /// Allocate an empty record.
    #[inline]
    pub fn alloc_empty(&mut self) -> RecordIdx {
        let pos = self.data.len();
        RecordIdx::make(pos, pos)
    }

    /// Allocate a single-entry record.
    #[inline]
    pub fn alloc_unit(&mut self, key: StrKey, value: Value) -> RecordIdx {
        let start = self.data.len();
        self.data.push((key, value));
        let end = self.data.len();
        RecordIdx::make(start, end)
    }

    /// Copy an existing record within the arena.
    #[inline]
    pub fn copy(&mut self, idx: RecordIdx) -> RecordIdx {
        let start = self.data.len();
        self.data.extend_from_within(idx.start()..idx.end());
        let end = self.data.len();
        RecordIdx::make(start, end)
    }

    // ── mutating operations ───────────────────────────────────────

    /// Insert a key-value pair at the correct sorted position.
    ///
    /// If the key already exists, the new entry is inserted before the
    /// existing one, shadowing it. The old entry is preserved so that
    /// if the visible entry is later removed, the shadowed value resurfaces.
    ///
    /// Returns a new index for the resulting record.
    pub fn insert(&mut self, idx: RecordIdx, key: StrKey, value: Value) -> RecordIdx {
        let start = idx.start();
        let end = idx.end();

        // Find the insertion point (before any existing entry with the same key)
        let insert_pos = match self.binary_search(idx, key) {
            Ok(pos) => pos, // insert at existing key position → shadows it
            Err(pos) => pos,
        };

        let new_start = self.data.len();

        // Copy entries before insertion point
        if insert_pos > 0 {
            self.data.extend_from_within(start..start + insert_pos);
        }

        // Insert new entry (this will be the visible one)
        self.data.push((key, value));

        // Copy the old entry at insert_pos (now shadowed) plus everything after it
        let remaining_start = start + insert_pos;
        if remaining_start < end {
            self.data.extend_from_within(remaining_start..end);
        }

        let new_end = self.data.len();
        RecordIdx::make(new_start, new_end)
    }
    /// Remove the first (visible) occurrence of `key`.
    ///
    /// Returns `Some((removed_value, new_idx))` for the resulting record.
    /// If the key doesn't exist, returns `None`.
    /// If there is a shadowed entry for the same key, it resurfaces.
    pub fn remove(&mut self, idx: RecordIdx, key: StrKey) -> Option<(Value, RecordIdx)> {
        let start = idx.start();
        let end = idx.end();

        let pos = match self.binary_search(idx, key) {
            Ok(pos) => pos,
            Err(_) => return None, // key not found
        };

        let removed_value = self.data[start + pos].1;

        let new_start = self.data.len();

        // Copy entries before the removed entry
        if pos > 0 {
            self.data.extend_from_within(start..start + pos);
        }

        // Copy entries after the removed entry (shadowed entries with same key
        // become visible again)
        if start + pos + 1 < end {
            self.data.extend_from_within(start + pos + 1..end);
        }

        let new_end = self.data.len();
        Some((removed_value, RecordIdx::make(new_start, new_end)))
    }

    /// Split off the first (smallest key) entry.
    ///
    /// Returns `Some(((key, value), rest_idx))` if the record is non-empty.
    /// The original record is untouched.
    pub fn split_front(&mut self, idx: RecordIdx) -> Option<((StrKey, Value), RecordIdx)> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let first = self.data[start];

        // Copy the tail (all entries except the first)
        let new_start = self.data.len();
        self.data.extend_from_within(start + 1..end);
        let new_end = self.data.len();

        Some((first, RecordIdx::make(new_start, new_end)))
    }

    /// Split off the last (largest key) entry.
    ///
    /// Returns `Some((rest_idx, (key, value)))` if the record is non-empty.
    /// The original record is untouched.
    pub fn split_back(&mut self, idx: RecordIdx) -> Option<(RecordIdx, (StrKey, Value))> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let last = self.data[end - 1];

        // Copy the head (all entries except the last)
        let new_start = self.data.len();
        self.data.extend_from_within(start..end - 1);
        let new_end = self.data.len();

        Some((RecordIdx::make(new_start, new_end), last))
    }

    /// Merge two records using row-polymorphic merge semantics.
    ///
    /// Rules:
    /// 1. `r & {}` or `{} & r` = `r`
    /// 2. `{a: t | r} & {a: u | s}` = `{a: (t & u) | (r & s)}` (same key, merge values recursively)
    /// 3. `{a: t | r} & {b: u | s}` = `{a: t | (r & {b: u | s})}` if `a < b`
    ///    `{a: t | r} & {b: u | s}` = `{b: u | ({a: t | r} & s)}` if `b < a`
    ///
    /// Returns `None` if merging fails (e.g., non-record values at the same key).
    pub fn merge(&mut self, left: RecordIdx, right: RecordIdx) -> Option<RecordIdx> {
        let l_start = left.start();
        let l_end = left.end();
        let r_start = right.start();
        let r_end = right.end();

        // Rule 1: empty cases
        if l_start == l_end {
            return Some(right);
        }
        if r_start == r_end {
            return Some(left);
        }

        let new_start = self.data.len();

        let mut i = l_start;
        let mut j = r_start;

        while i < l_end && j < r_end {
            match self.data[i].0.cmp(&self.data[j].0) {
                std::cmp::Ordering::Less => {
                    // Rule 3: left label comes first, keep left entry
                    self.data.push(self.data[i]);
                    i += 1;
                }
                std::cmp::Ordering::Greater => {
                    // Rule 3: right label comes first, keep right entry
                    self.data.push(self.data[j]);
                    j += 1;
                }
                std::cmp::Ordering::Equal => {
                    // Rule 2: same label, recursively merge values
                    let key = self.data[i].0;
                    let left_val = self.data[i].1;
                    let right_val = self.data[j].1;

                    let merged_val = match (&left_val, &right_val) {
                        (Value::Record(l_idx), Value::Record(r_idx)) => {
                            Value::Record(self.merge(*l_idx, *r_idx)?)
                        }
                        _ => return None, // Cannot merge non-record values
                    };

                    self.data.push((key, merged_val));
                    i += 1;
                    j += 1;
                }
            }
        }

        // Drain remaining entries from whichever record has leftovers
        if i < l_end {
            self.data.extend_from_within(i..l_end);
        }
        if j < r_end {
            self.data.extend_from_within(j..r_end);
        }

        let new_end = self.data.len();
        Some(RecordIdx::make(new_start, new_end))
    }

    /// Merge two records by concatenating their entries, with `left` taking precedence over `right`.
    pub fn merge_left(&mut self, left: RecordIdx, right: RecordIdx) -> RecordIdx {
        let start = self.data.len();
        self.data.extend_from_within(left.start()..left.end());
        self.data.extend_from_within(right.start()..right.end());
        let end = self.data.len();

        self.data[start..end].sort_by_key(|(k, _)| *k);

        RecordIdx::make(start, end)
    }

    /// Merge two records by concatenating their entries, with `right` taking precedence over `left`.
    pub fn merge_right(&mut self, left: RecordIdx, right: RecordIdx) -> RecordIdx {
        let start = self.data.len();
        self.data.extend_from_within(right.start()..right.end());
        self.data.extend_from_within(left.start()..left.end());
        let end = self.data.len();

        self.data[start..end].sort_by_key(|(k, _)| *k);

        RecordIdx::make(start, end)
    }

    /// Extend a record at the specified field path.
    ///
    /// Inserts the value at the final label in the path, creating
    /// intermediate nested records as needed.
    pub fn extend_at_path(
        &mut self,
        idx: RecordIdx,
        field_path: &[StrKey],
        value: Value,
    ) -> Result<RecordIdx, String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: insert at this level
            Ok(self.insert(idx, field_path[0], value))
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let nested = self
                .get_value(idx, label)
                .unwrap_or_else(|| Value::Record(self.alloc_empty()));

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot extend non-record at field '{:?}'", label));
            };

            let new_nested = self.extend_at_path(nested_idx, &field_path[1..], value)?;
            Ok(self.insert(idx, label, Value::Record(new_nested)))
        }
    }

    /// Restrict (remove) a field at the specified field path.
    ///
    /// Removes the field at the final label in the path. If the
    /// path navigates through missing fields, returns an error.
    pub fn restrict_at_path(
        &mut self,
        idx: RecordIdx,
        field_path: &[StrKey],
    ) -> Result<RecordIdx, String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: remove at this level
            Ok(self
                .remove(idx, field_path[0])
                .map(|(_, new_idx)| new_idx)
                .unwrap_or(idx))
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let nested = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot restrict non-existing field at '{:?}'", label))?;

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot restrict non-record at field '{:?}'", label));
            };

            let new_nested = self.restrict_at_path(nested_idx, &field_path[1..])?;
            Ok(self.insert(idx, label, Value::Record(new_nested)))
        }
    }

    /// Update a field at the specified field path using an update function.
    ///
    /// The update function receives the current value and should return
    /// the new value (or an error).
    pub fn update_at_path<F>(
        &mut self,
        idx: RecordIdx,
        field_path: &[StrKey],
        update_fn: F,
    ) -> Result<RecordIdx, String>
    where
        F: FnOnce(Value) -> Result<Value, String>,
    {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            // Base case: update at this level
            let label = field_path[0];
            let current_val = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot update non-existing field at '{:?}'", label))?;

            let new_value = update_fn(current_val)?;
            Ok(self.insert(idx, label, new_value))
        } else {
            // Recursive case: navigate deeper
            let label = field_path[0];
            let nested = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot update non-existing field at '{:?}'", label))?;

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot update non-record at field '{:?}'", label));
            };

            let new_nested = self.update_at_path(nested_idx, &field_path[1..], update_fn)?;
            Ok(self.insert(idx, label, Value::Record(new_nested)))
        }
    }
}

impl Default for RecordArena {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for RecordArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RecordArena({} entries)", self.data.len())
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     // ── test helpers ───────────────────────────────────────────

//     /// Convenience: create a StrKey from a &str using a simple interner.
//     /// For tests we use a global-ish approach: intern the string.
//     fn k(s: &str) -> StrKey {
//         // We use a thread-local or lazy interner for tests.
//         use std::sync::LazyLock;
//         static mut INTERNER: LazyLock<kola_utils::interner::StrInterner> =
//             LazyLock::new(kola_utils::interner::StrInterner::new);

//         unsafe { INTERNER.intern(s) }
//     }

//     fn v_none() -> Value {
//         Value::None
//     }

//     fn r(arena: &mut RecordArena, pairs: &[(&str, Value)]) -> RecordIdx {
//         let entries: Vec<(StrKey, Value)> =
//             pairs.iter().map(|(key, v)| (k(*key), v.clone())).collect();
//         arena.alloc(&entries)
//     }

//     // ── basic alloc/get ───────────────────────────────────────

//     #[test]
//     fn test_alloc_and_get() {
//         let mut arena = RecordArena::new();
//         let empty = arena.alloc_empty();
//         assert!(arena.is_empty(empty));
//         assert_eq!(arena.len(empty), 0);

//         let single = arena.alloc_unit(k("a"), v_none());
//         assert_eq!(arena.len(single), 1);
//         assert_eq!(arena.get(single), &[(k("a"), v_none())]);
//     }

//     #[test]
//     fn test_copy() {
//         let mut arena = RecordArena::new();
//         let original = arena.alloc(&[(k("a"), v_none()), (k("b"), v_none())]);
//         let copied = arena.copy(original);

//         assert_eq!(arena.get(original), arena.get(copied));
//         assert_ne!(original, copied);
//         // Original is untouched
//         assert_eq!(arena.len(original), 2);
//     }

//     // ── insert with shadowing ─────────────────────────────────

//     #[test]
//     fn test_insert_new_key() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc_empty();

//         let idx = arena.insert(idx, k("b"), Value::Num(2.0));
//         let idx = arena.insert(idx, k("a"), Value::Num(1.0));
//         let idx = arena.insert(idx, k("c"), Value::Num(3.0));

//         assert_eq!(arena.len(idx), 3);
//         assert_eq!(
//             arena.get(idx),
//             &[
//                 (k("a"), Value::Num(1.0)),
//                 (k("b"), Value::Num(2.0)),
//                 (k("c"), Value::Num(3.0)),
//             ]
//         );
//     }

//     #[test]
//     fn test_insert_shadowing() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc(&[(k("a"), Value::Num(1.0)), (k("b"), Value::Num(2.0))]);

//         // Insert a new value for "a" – should shadow the old one
//         let idx = arena.insert(idx, k("a"), Value::Num(10.0));

//         // Should have 3 entries: a=10, a=1 (shadowed), b=2
//         assert_eq!(arena.len(idx), 3);
//         assert_eq!(arena.get_value(idx, k("a")), Some(Value::Num(10.0)));
//         assert_eq!(arena.get_value(idx, k("b")), Some(Value::Num(2.0)));

//         // Remove "a" – the shadowed value should resurface
//         let (_, idx) = arena.remove(idx, k("a")).unwrap();
//         assert_eq!(arena.get_value(idx, k("a")), Some(Value::Num(1.0)));
//         assert_eq!(arena.len(idx), 2);
//     }

//     #[test]
//     fn test_insert_multiple_shadows() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc_empty();

//         let idx = arena.insert(idx, k("x"), Value::Num(1.0));
//         let idx = arena.insert(idx, k("x"), Value::Num(10.0));
//         let idx = arena.insert(idx, k("x"), Value::Num(100.0));

//         // Three entries, visible is 100.0
//         assert_eq!(arena.len(idx), 3);
//         assert_eq!(arena.get_value(idx, k("x")), Some(Value::Num(100.0)));

//         // Remove once → 10.0 resurfaces
//         let (_, idx) = arena.remove(idx, k("x")).unwrap();
//         assert_eq!(arena.get_value(idx, k("x")), Some(Value::Num(10.0)));
//         assert_eq!(arena.len(idx), 2);

//         // Remove again → 1.0 resurfaces
//         let (_, idx) = arena.remove(idx, k("x")).unwrap();
//         assert_eq!(arena.get_value(idx, k("x")), Some(Value::Num(1.0)));
//         assert_eq!(arena.len(idx), 1);

//         // Remove again → key is gone
//         let (_, idx) = arena.remove(idx, k("x")).unwrap();
//         assert!(arena.get_value(idx, k("x")).is_none());
//         assert!(arena.is_empty(idx));
//     }

//     // ── remove ───────────────────────────────────────────────

//     #[test]
//     fn test_remove_non_existent() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc(&[(k("a"), v_none())]);

//         let result = arena.remove(idx, k("b"));
//         assert!(result.is_none());
//         assert_eq!(arena.len(idx), 1); // unchanged
//     }

//     #[test]
//     fn test_remove_visible() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc(&[
//             (k("a"), Value::Num(1.0)),
//             (k("b"), Value::Num(2.0)),
//             (k("c"), Value::Num(3.0)),
//         ]);

//         let (_, idx) = arena.remove(idx, k("b")).unwrap();
//         assert_eq!(arena.len(idx), 2);
//         assert_eq!(
//             arena.get(idx),
//             &[(k("a"), Value::Num(1.0)), (k("c"), Value::Num(3.0)),]
//         );
//     }

//     // ── split_front / split_back ─────────────────────────────

//     #[test]
//     fn test_split_front() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc(&[
//             (k("a"), Value::Num(1.0)),
//             (k("b"), Value::Num(2.0)),
//             (k("c"), Value::Num(3.0)),
//         ]);

//         let ((key, val), rest) = arena.split_front(idx).unwrap();
//         assert_eq!(key, k("a"));
//         assert_eq!(val, Value::Num(1.0));
//         assert_eq!(
//             arena.get(rest),
//             &[(k("b"), Value::Num(2.0)), (k("c"), Value::Num(3.0)),]
//         );
//         // Original untouched
//         assert_eq!(arena.len(idx), 3);
//     }

//     #[test]
//     fn test_split_back() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc(&[
//             (k("a"), Value::Num(1.0)),
//             (k("b"), Value::Num(2.0)),
//             (k("c"), Value::Num(3.0)),
//         ]);

//         let (rest, (key, val)) = arena.split_back(idx).unwrap();
//         assert_eq!(key, k("c"));
//         assert_eq!(val, Value::Num(3.0));
//         assert_eq!(
//             arena.get(rest),
//             &[(k("a"), Value::Num(1.0)), (k("b"), Value::Num(2.0)),]
//         );
//         // Original untouched
//         assert_eq!(arena.len(idx), 3);
//     }

//     #[test]
//     fn test_split_front_empty() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc_empty();
//         assert!(arena.split_front(idx).is_none());
//     }

//     #[test]
//     fn test_split_back_empty() {
//         let mut arena = RecordArena::new();
//         let idx = arena.alloc_empty();
//         assert!(arena.split_back(idx).is_none());
//     }

//     #[test]
//     fn test_split_front_with_shadow() {
//         let mut arena = RecordArena::new();
//         let mut idx = arena.alloc_empty();
//         idx = arena.insert(idx, k("a"), Value::Num(10.0));
//         idx = arena.insert(idx, k("a"), Value::Num(1.0)); // shadowed

//         let ((key, val), rest) = arena.split_front(idx).unwrap();
//         assert_eq!(key, k("a"));
//         assert_eq!(val, Value::Num(10.0)); // visible entry
//         assert_eq!(arena.len(rest), 1);
//         // The shadowed "a" is still in rest
//         assert_eq!(arena.get_value(rest, k("a")), Some(Value::Num(1.0)));
//     }

//     // ── merge ────────────────────────────────────────────────

//     #[test]
//     fn test_merge_empty_left() {
//         let mut arena = RecordArena::new();
//         let empty = arena.alloc_empty();
//         let right = arena.alloc(&[(k("a"), v_none())]);

//         let merged = arena.merge(empty, right).unwrap();
//         assert_eq!(arena.len(merged), 1);
//         assert_eq!(arena.get(merged), &[(k("a"), v_none())]);
//     }

//     #[test]
//     fn test_merge_empty_right() {
//         let mut arena = RecordArena::new();
//         let left = arena.alloc(&[(k("a"), v_none())]);
//         let empty = arena.alloc_empty();

//         let merged = arena.merge(left, empty).unwrap();
//         assert_eq!(arena.len(merged), 1);
//         assert_eq!(arena.get(merged), &[(k("a"), v_none())]);
//     }

//     #[test]
//     fn test_merge_disjoint_keys() {
//         let mut arena = RecordArena::new();
//         let left = arena.alloc(&[(k("a"), Value::Num(1.0))]);
//         let right = arena.alloc(&[(k("b"), Value::Num(2.0))]);

//         let merged = arena.merge(left, right).unwrap();
//         assert_eq!(arena.len(merged), 2);
//         assert_eq!(
//             arena.get(merged),
//             &[(k("a"), Value::Num(1.0)), (k("b"), Value::Num(2.0)),]
//         );
//     }

//     #[test]
//     fn test_merge_overlapping_keys() {
//         let mut arena = RecordArena::new();
//         let left = arena.alloc(&[(k("a"), Value::Num(1.0)), (k("b"), Value::Num(2.0))]);
//         let right = arena.alloc(&[(k("b"), Value::Num(3.0)), (k("c"), Value::Num(4.0))]);

//         // merge expects both values at "b" to be records that can be merged.
//         // Since they're not records, this should return None.
//         assert!(arena.merge(left, right).is_none());
//     }

//     #[test]
//     fn test_merge_nested_records() {
//         let mut arena = RecordArena::new();

//         // left = { a: { x: 10 }, b: 1 }
//         let inner_left = arena.alloc(&[(k("x"), Value::Num(10.0))]);
//         let left = arena.alloc(&[
//             (k("a"), Value::Record(inner_left)),
//             (k("b"), Value::Num(1.0)),
//         ]);

//         // right = { a: { y: 20 }, c: 2 }
//         let inner_right = arena.alloc(&[(k("y"), Value::Num(20.0))]);
//         let right = arena.alloc(&[
//             (k("a"), Value::Record(inner_right)),
//             (k("c"), Value::Num(2.0)),
//         ]);

//         let merged = arena.merge(left, right).unwrap();

//         // merged should be { a: { x: 10, y: 20 }, b: 1, c: 2 }
//         assert_eq!(arena.len(merged), 3);

//         // Check a's nested record
//         let a_val = arena.get_value(merged, k("a")).unwrap();
//         let Value::Record(a_idx) = a_val else {
//             panic!("expected record")
//         };
//         assert_eq!(arena.get_value(a_idx, k("x")), Some(Value::Num(10.0)));
//         assert_eq!(arena.get_value(a_idx, k("y")), Some(Value::Num(20.0)));

//         // Check b and c
//         assert_eq!(arena.get_value(merged, k("b")), Some(Value::Num(1.0)));
//         assert_eq!(arena.get_value(merged, k("c")), Some(Value::Num(2.0)));
//     }
// }
