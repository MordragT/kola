use kola_utils::{display::DisplayWith, interner::StrKey, serde::SerializeWith};
use serde::ser::SerializeMap;
use std::{
    fmt::{self, Debug},
    num::NonZeroU32,
};

use crate::{heap::Heap, value::Value};

/// A 4-byte pointer to a node inside the `RecordArena`.
/// Uses `NonZeroU32` so that `Option<RecordIdx>` (representing an empty record `{}`)
/// takes up exactly 4 bytes via niche optimization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecordIdx(NonZeroU32);

impl RecordIdx {
    #[inline]
    pub fn as_usize(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[inline]
    fn make(offset: usize) -> Self {
        let raw = (offset + 1) as u32;
        Self(NonZeroU32::new(raw).expect("RecordArena offset overflow"))
    }
}

impl DisplayWith<Heap> for RecordIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        let pairs = heap
            .records
            .iter(Some(*self))
            .map(|(k, v)| (&heap.str_interner[k], v));
        f.debug_map().entries(pairs).finish()
    }
}

impl SerializeWith<Heap> for RecordIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let pairs = heap.records.iter(Some(*self)).collect::<Vec<_>>();
        let mut map = serializer.serialize_map(Some(pairs.len()))?;

        for (key, val) in pairs {
            map.serialize_entry(&heap.str_interner[key], &heap.with(&val))?;
        }

        map.end()
    }
}

/// A node in a sorted, singly-linked list representing record fields.
/// Kept sorted by `StrKey` to ensure elegant $O(N)$ row-polymorphic merges.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RecordNode<K> {
    key: K,
    value: Value,
    tail: Option<RecordIdx>,
}

/// An append-only arena for records leveraging true structural sharing.
#[derive(Debug, Clone)]
pub struct RecordArena<K = StrKey> {
    data: Vec<RecordNode<K>>,
}

impl<K> RecordArena<K>
where
    K: Debug + Copy + Eq + Ord,
{
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

    #[inline]
    fn alloc_node(&mut self, key: K, value: Value, tail: Option<RecordIdx>) -> RecordIdx {
        let offset = self.data.len();
        self.data.push(RecordNode { key, value, tail });
        RecordIdx::make(offset)
    }

    // ── read-only operations ──────────────────────────────────────

    /// Get the length of the record (including shadowed entries).
    pub fn len(&self, idx: Option<RecordIdx>) -> usize {
        let mut count = 0;
        let mut curr = idx;
        while let Some(id) = curr {
            let RecordNode { tail, .. } = self.data[id.as_usize()];
            count += 1;
            curr = tail;
        }
        count
    }

    /// Check if the record is empty.
    #[inline]
    pub fn is_empty(&self, idx: Option<RecordIdx>) -> bool {
        idx.is_none()
    }

    /// Get the visible value by key (returns the first/shadowing entry).
    /// Since the chain is sorted, we can stop early if we pass the target key.
    pub fn get_value(&self, idx: Option<RecordIdx>, key: K) -> Option<Value> {
        let mut curr = idx;
        while let Some(id) = curr {
            let RecordNode {
                key: k,
                value,
                tail,
            } = self.data[id.as_usize()];
            if k == key {
                return Some(value);
            }
            if k > key {
                break; // Optimization: Sorted invariant means it's not further down the line
            }
            curr = tail;
        }
        None
    }

    /// Check if the record contains the given key.
    #[inline]
    pub fn contains_key(&self, idx: Option<RecordIdx>, key: K) -> bool {
        self.get_value(idx, key).is_some()
    }

    /// Get the first (smallest key) entry.
    /// **O(1) Complexity.**
    pub fn first(&self, idx: Option<RecordIdx>) -> Option<(K, Value)> {
        let id = idx?;
        let RecordNode { key, value, .. } = self.data[id.as_usize()];
        Some((key, value))
    }

    /// Get the last (largest key) entry.
    pub fn last(&self, idx: Option<RecordIdx>) -> Option<(K, Value)> {
        let mut curr = idx?;
        loop {
            let RecordNode { key, value, tail } = self.data[curr.as_usize()];
            if let Some(next) = tail {
                curr = next;
            } else {
                return Some((key, value));
            }
        }
    }

    /// Iterate over all entries (including shadowed).
    #[inline]
    pub fn iter(&self, idx: Option<RecordIdx>) -> RecordIter<'_, K> {
        RecordIter {
            arena: self,
            current: idx,
        }
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new record from a slice of entries.
    /// Sorts them by key and builds the chain backwards to preserve sorting and shadowing.
    pub fn alloc(&mut self, entries: &[(K, Value)]) -> Option<RecordIdx> {
        if entries.is_empty() {
            return None;
        }
        let mut sorted = entries.to_vec();
        sorted.sort_by_key(|(k, _)| *k);

        let mut curr = None;
        for (k, v) in sorted.into_iter().rev() {
            curr = Some(self.alloc_node(k, v, curr));
        }
        curr
    }

    /// Allocate an empty record `{}`.
    #[inline]
    pub fn alloc_empty(&mut self) -> Option<RecordIdx> {
        None
    }

    /// Allocate a single-entry record.
    #[inline]
    pub fn alloc_unit(&mut self, key: K, value: Value) -> Option<RecordIdx> {
        Some(self.alloc_node(key, value, None))
    }

    // ── mutating operations (leveraging structural sharing) ───────

    /// Insert a key-value pair at the correct sorted position.
    ///
    /// **Structural Sharing Win:** Suffixes containing keys >= `key` are
    /// fully shared. We only allocate nodes for fields that sort before `key`.
    pub fn insert(&mut self, idx: Option<RecordIdx>, key: K, value: Value) -> RecordIdx {
        let mut curr = idx;
        let mut prefix = Vec::new();

        while let Some(id) = curr {
            let RecordNode {
                key: k,
                value: v,
                tail,
            } = self.data[id.as_usize()];
            if k >= key {
                break; // Insert here to maintain sorting and enable shadowing
            }
            prefix.push((k, v));
            curr = tail;
        }

        // Link the new node directly to the shared remaining tail
        let mut new_idx = self.alloc_node(key, value, curr);

        // Rebuild prefix spine backwards
        for (k, v) in prefix.into_iter().rev() {
            new_idx = self.alloc_node(k, v, Some(new_idx));
        }

        new_idx
    }

    /// Remove the first (visible) occurrence of `key`.
    ///
    /// **Structural Sharing Win:** The entire tail following the removed element
    /// is shared directly without copying.
    pub fn remove(&mut self, idx: Option<RecordIdx>, key: K) -> Option<(Value, Option<RecordIdx>)> {
        let mut curr = idx;
        let mut prefix = Vec::new();
        let mut removed_val = None;

        while let Some(id) = curr {
            let RecordNode {
                key: k,
                value,
                tail,
            } = self.data[id.as_usize()];
            if k == key {
                removed_val = Some(value);
                curr = tail; // Drop this node; the remaining tail is shared
                break;
            }
            if k > key {
                return None; // Key doesn't exist
            }
            prefix.push((k, value));
            curr = tail;
        }

        let val = removed_val?;
        let mut new_idx = curr;

        for (k, v) in prefix.into_iter().rev() {
            new_idx = Some(self.alloc_node(k, v, new_idx));
        }

        Some((val, new_idx))
    }

    /// Split off the first (smallest key) entry.
    ///
    /// **O(1) Time and Space.** Zero allocations. Directly returns the shared tail.
    pub fn split_front(&self, idx: Option<RecordIdx>) -> Option<((K, Value), Option<RecordIdx>)> {
        let id = idx?;
        let RecordNode { key, value, tail } = &self.data[id.as_usize()];
        Some(((*key, *value), *tail))
    }

    /// Split off the last (largest key) entry.
    pub fn split_back(
        &mut self,
        idx: Option<RecordIdx>,
    ) -> Option<(Option<RecordIdx>, (K, Value))> {
        let id = idx?;
        let mut curr = Some(id);
        let mut prefix = Vec::new();

        while let Some(current_id) = curr {
            let RecordNode { key, value, tail } = self.data[current_id.as_usize()];
            if tail.is_none() {
                let last_pair = (key, value);
                let mut new_list = None;
                for (k, v) in prefix.into_iter().rev() {
                    new_list = Some(self.alloc_node(k, v, new_list));
                }
                return Some((new_list, last_pair));
            }
            prefix.push((key, value));
            curr = tail;
        }
        None
    }

    /// Merge two records using row-polymorphic merge semantics.
    ///
    /// Completely iterative spine zip-merge.
    /// **Structural Sharing Win:** When either the left or right record runs
    /// out of entries, the entire remainder of the other record is attached
    /// in **O(1) time** as a shared tail. No leftover elements are copied!
    pub fn merge(
        &mut self,
        left: Option<RecordIdx>,
        right: Option<RecordIdx>,
    ) -> Option<Option<RecordIdx>> {
        let mut l_curr = left;
        let mut r_curr = right;
        let mut prefix = Vec::new();

        while let (Some(l_id), Some(r_id)) = (l_curr, r_curr) {
            let RecordNode {
                key: l_k,
                value: l_v,
                tail: l_tail,
            } = self.data[l_id.as_usize()];
            let RecordNode {
                key: r_k,
                value: r_v,
                tail: r_tail,
            } = self.data[r_id.as_usize()];

            if l_k == r_k {
                let merged_val = match (l_v, r_v) {
                    (Value::Record(l_rec), Value::Record(r_rec)) => {
                        Value::Record(self.merge(l_rec, r_rec)?)
                    }
                    _ => return None, // Type mismatch error: Cannot merge non-record values
                };
                prefix.push((l_k, merged_val));
                l_curr = l_tail;
                r_curr = r_tail;
            } else if l_k < r_k {
                prefix.push((l_k, l_v));
                l_curr = l_tail;
            } else {
                prefix.push((r_k, r_v));
                r_curr = r_tail;
            }
        }

        // Link right into the unmerged trailing side instantaneously
        let mut new_idx = if l_curr.is_some() { l_curr } else { r_curr };

        // Rebuild the merged spine
        for (k, v) in prefix.into_iter().rev() {
            new_idx = Some(self.alloc_node(k, v, new_idx));
        }

        Some(new_idx)
    }

    /// Merge two records, with `left` taking precedence over `right` on duplicate keys.
    pub fn merge_left(
        &mut self,
        left: Option<RecordIdx>,
        right: Option<RecordIdx>,
    ) -> Option<RecordIdx> {
        let mut l_curr = left;
        let mut r_curr = right;
        let mut prefix = Vec::new();

        while let (Some(l_id), Some(r_id)) = (l_curr, r_curr) {
            let RecordNode {
                key: l_k,
                value: l_v,
                tail: l_tail,
            } = self.data[l_id.as_usize()];
            let RecordNode {
                key: r_k,
                value: r_v,
                tail: r_tail,
            } = self.data[r_id.as_usize()];

            if l_k <= r_k {
                prefix.push((l_k, l_v));
                l_curr = l_tail;
            } else {
                prefix.push((r_k, r_v));
                r_curr = r_tail;
            }
        }

        let mut new_idx = if l_curr.is_some() { l_curr } else { r_curr };
        for (k, v) in prefix.into_iter().rev() {
            new_idx = Some(self.alloc_node(k, v, new_idx));
        }
        new_idx
    }

    /// Merge two records, with `right` taking precedence over `left` on duplicate keys.
    pub fn merge_right(
        &mut self,
        left: Option<RecordIdx>,
        right: Option<RecordIdx>,
    ) -> Option<RecordIdx> {
        let mut l_curr = left;
        let mut r_curr = right;
        let mut prefix = Vec::new();

        while let (Some(l_id), Some(r_id)) = (l_curr, r_curr) {
            let RecordNode {
                key: l_k,
                value: l_v,
                tail: l_tail,
            } = self.data[l_id.as_usize()];
            let RecordNode {
                key: r_k,
                value: r_v,
                tail: r_tail,
            } = self.data[r_id.as_usize()];

            if r_k <= l_k {
                prefix.push((r_k, r_v));
                r_curr = r_tail;
            } else {
                prefix.push((l_k, l_v));
                l_curr = l_tail;
            }
        }

        let mut new_idx = if l_curr.is_some() { l_curr } else { r_curr };
        for (k, v) in prefix.into_iter().rev() {
            new_idx = Some(self.alloc_node(k, v, new_idx));
        }
        new_idx
    }

    // ── path operations ───────────────────────────────────────────

    pub fn extend_at_path(
        &mut self,
        idx: Option<RecordIdx>,
        field_path: &[K],
        value: Value,
    ) -> Result<Option<RecordIdx>, String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            Ok(Some(self.insert(idx, field_path[0], value)))
        } else {
            let label = field_path[0];
            let nested = self.get_value(idx, label).unwrap_or(Value::Record(None));

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot extend non-record at field '{:?}'", label));
            };

            let new_nested = self.extend_at_path(nested_idx, &field_path[1..], value)?;
            Ok(Some(self.insert(idx, label, Value::Record(new_nested))))
        }
    }

    pub fn restrict_at_path(
        &mut self,
        idx: Option<RecordIdx>,
        field_path: &[K],
    ) -> Result<Option<RecordIdx>, String> {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            Ok(self
                .remove(idx, field_path[0])
                .map(|(_, new_idx)| new_idx)
                .unwrap_or(idx))
        } else {
            let label = field_path[0];
            let nested = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot restrict non-existing field at '{:?}'", label))?;

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot restrict non-record at field '{:?}'", label));
            };

            let new_nested = self.restrict_at_path(nested_idx, &field_path[1..])?;
            Ok(Some(self.insert(idx, label, Value::Record(new_nested))))
        }
    }

    pub fn update_at_path<F>(
        &mut self,
        idx: Option<RecordIdx>,
        field_path: &[K],
        update_fn: F,
    ) -> Result<Option<RecordIdx>, String>
    where
        F: FnOnce(Value) -> Result<Value, String>,
    {
        if field_path.is_empty() {
            return Err("Empty field path".to_string());
        }

        if field_path.len() == 1 {
            let label = field_path[0];
            let current_val = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot update non-existing field at '{:?}'", label))?;

            let new_value = update_fn(current_val)?;
            Ok(Some(self.insert(idx, label, new_value)))
        } else {
            let label = field_path[0];
            let nested = self
                .get_value(idx, label)
                .ok_or_else(|| format!("Cannot update non-existing field at '{:?}'", label))?;

            let Value::Record(nested_idx) = nested else {
                return Err(format!("Cannot update non-record at field '{:?}'", label));
            };

            let new_nested = self.update_at_path(nested_idx, &field_path[1..], update_fn)?;
            Ok(Some(self.insert(idx, label, Value::Record(new_nested))))
        }
    }
}

// ── Iterator Support ──────────────────────────────────────────

pub struct RecordIter<'a, K = StrKey> {
    arena: &'a RecordArena<K>,
    current: Option<RecordIdx>,
}

impl<'a, K> Iterator for RecordIter<'a, K>
where
    K: Copy,
{
    type Item = (K, Value);

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.current?;
        let RecordNode { key, value, tail } = self.arena.data[id.as_usize()];
        self.current = tail;
        Some((key, value))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_and_unit_allocations() {
        let mut arena = RecordArena::new();

        // Empty records are completely free (None)
        let empty = arena.alloc_empty();
        assert!(empty.is_none());
        assert!(arena.is_empty(empty));
        assert_eq!(arena.len(empty), 0);

        // Unit record allocation
        let unit = arena.alloc_unit(10, Value::Num(42.0));
        assert!(unit.is_some());
        assert_eq!(arena.len(unit), 1);
        assert_eq!(arena.get_value(unit, 10), Some(Value::Num(42.0)));
        assert_eq!(arena.get_value(unit, 99), None);
    }

    #[test]
    fn test_bulk_allocation_and_sorting() {
        let mut arena = RecordArena::new();

        // Pass out-of-order keys to alloc
        let entries = vec![
            (3, Value::Num(30.0)),
            (1, Value::Num(10.0)),
            (2, Value::Num(20.0)),
        ];
        let rec = arena.alloc(&entries);

        assert_eq!(arena.len(rec), 3);

        // Iteration should respect sorted order of keys
        let collected = arena.iter(rec).collect::<Vec<_>>();
        assert_eq!(
            collected,
            vec![
                (1, Value::Num(10.0)),
                (2, Value::Num(20.0)),
                (3, Value::Num(30.0))
            ]
        );
    }

    #[test]
    fn test_insert_and_structural_sharing() {
        let mut arena = RecordArena::new();

        // Start with an initial record: {2: 20, 4: 40}
        let r1 = arena.alloc(&[(2, Value::Num(20.0)), (4, Value::Num(40.0))]);

        // Insert a value in the middle: {2: 20, 3: 30, 4: 40}
        let r2 = arena.insert(r1, 3, Value::Num(30.0));

        // Verify r2 state
        let collected_r2: Vec<_> = arena.iter(some(r2)).collect();
        assert_eq!(
            collected_r2,
            vec![
                (2, Value::Num(20.0)),
                (3, Value::Num(30.0)),
                (4, Value::Num(40.0))
            ]
        );

        // CRITICAL: Verify r1 was completely untouched (true immutability)
        let collected_r1: Vec<_> = arena.iter(r1).collect();
        assert_eq!(
            collected_r1,
            vec![(2, Value::Num(20.0)), (4, Value::Num(40.0))]
        );

        // Early break check: searching for missing key past its sorting position
        assert_eq!(arena.get_value(some(r2), 5), None);
    }

    #[test]
    fn test_shadowing_and_removal() {
        let mut arena = RecordArena::new();

        // Create initial record
        let r1 = arena.alloc_unit(1, Value::Num(100.0));

        // Insert duplicate key to create a shadow: r2 = {1: 200, 1: 100}
        let r2 = arena.insert(r1, 1, Value::Num(200.0));
        assert_eq!(arena.len(some(r2)), 2);

        // The visible value should be the newest one
        assert_eq!(arena.get_value(some(r2), 1), Some(Value::Num(200.0)));

        // Remove the shadow -> the older value should resurface
        let (removed, r3) = arena.remove(some(r2), 1).unwrap();
        assert_eq!(removed, Value::Num(200.0));
        assert_eq!(arena.len(r3), 1);
        assert_eq!(arena.get_value(r3, 1), Some(Value::Num(100.0)));

        // Trying to remove a non-existent key returns None
        assert!(arena.remove(r3, 99).is_none());
    }

    #[test]
    fn test_destructuring_splits() {
        let mut arena = RecordArena::new();
        let rec = arena.alloc(&[
            (1, Value::Num(10.0)),
            (2, Value::Num(20.0)),
            (3, Value::Num(30.0)),
        ]);

        // O(1) split front
        let ((k_head, v_head), tail) = arena.split_front(rec).unwrap();
        assert_eq!(k_head, 1);
        assert_eq!(v_head, Value::Num(10.0));
        assert_eq!(arena.len(tail), 2);

        // Split back
        let (head, (k_tail, v_tail)) = arena.split_back(rec).unwrap();
        assert_eq!(k_tail, 3);
        assert_eq!(v_tail, Value::Num(30.0));
        assert_eq!(arena.len(head), 2);
    }

    #[test]
    fn test_row_polymorphic_merge() {
        let mut arena = RecordArena::new();

        // Disjoint records
        let left = arena.alloc(&[(1, Value::Num(10.0)), (3, Value::Num(30.0))]);
        let right = arena.alloc(&[(2, Value::Num(20.0)), (4, Value::Num(40.0))]);

        let merged = arena.merge(left, right).unwrap();
        let result: Vec<_> = arena.iter(merged).collect();
        assert_eq!(
            result,
            vec![
                (1, Value::Num(10.0)),
                (2, Value::Num(20.0)),
                (3, Value::Num(30.0)),
                (4, Value::Num(40.0))
            ]
        );

        // Recurse Deep Overlapping Merges: { a: { x: 1 } } & { a: { y: 2 } }
        let nested_left = arena.alloc_unit(100, Value::Num(1.0));
        let nested_right = arena.alloc_unit(200, Value::Num(2.0));

        let rec_left = arena.alloc_unit(5, Value::Record(nested_left));
        let rec_right = arena.alloc_unit(5, Value::Record(nested_right));

        let deep_merged = arena.merge(rec_left, rec_right).unwrap();

        // Extract inner merged record at key 5
        if let Some(Value::Record(inner_idx)) = arena.get_value(deep_merged, 5) {
            let inner_pairs: Vec<_> = arena.iter(inner_idx).collect();
            assert_eq!(
                inner_pairs,
                vec![(100, Value::Num(1.0)), (200, Value::Num(2.0))]
            );
        } else {
            panic!("Expected nested record to be merged recursively");
        }

        // Type clash failure: merging same key with incompatible scalar value
        let bad_right = arena.alloc_unit(5, Value::Num(999.0));
        assert!(arena.merge(rec_left, bad_right).is_none());
    }

    #[test]
    fn test_merge_precedence() {
        let mut arena = RecordArena::new();
        let left = arena.alloc_unit(1, Value::Num(10.0));
        let right = arena.alloc_unit(1, Value::Num(20.0));

        // Merge Left keeps left values over right values on duplicate
        let m_left = arena.merge_left(left, right);
        assert_eq!(arena.get_value(m_left, 1), Some(Value::Num(10.0)));

        // Merge Right keeps right values over left values on duplicate
        let m_right = arena.merge_right(left, right);
        assert_eq!(arena.get_value(m_right, 1), Some(Value::Num(20.0)));
    }

    #[test]
    fn test_path_operations() {
        let mut arena = RecordArena::new();

        // Test deeply nested path extensions: extend {a: {}} at path ["a", "b"] with 42
        let inner = arena.alloc_empty();
        let root = arena.alloc_unit(1, Value::Record(inner)); // key 1 is "a"

        let path = vec![1, 2]; // path: ["a", "b"]
        let updated_root = arena.extend_at_path(root, &path, Value::Num(42.0)).unwrap();

        // Navigate down to see if it created the nested layout correctly
        if let Some(Value::Record(level_1)) = arena.get_value(updated_root, 1) {
            assert_eq!(arena.get_value(level_1, 2), Some(Value::Num(42.0)));
        } else {
            panic!("Failed to extend record at path");
        }

        // Deep update path operation
        let modified_root = arena
            .update_at_path(updated_root, &path, |v| {
                if let Value::Num(n) = v {
                    Ok(Value::Num(n + 8.0))
                } else {
                    Err("not an int".into())
                }
            })
            .unwrap();

        if let Some(Value::Record(level_1)) = arena.get_value(modified_root, 1) {
            assert_eq!(arena.get_value(level_1, 2), Some(Value::Num(50.0))); // 42 + 8
        }

        // Path restriction (removal)
        let restricted_root = arena.restrict_at_path(modified_root, &path).unwrap();
        if let Some(Value::Record(level_1)) = arena.get_value(restricted_root, 1) {
            assert!(arena.is_empty(level_1));
        }
    }

    // Helper to wrap raw RecordIdx options inside tests seamlessly
    fn some(idx: RecordIdx) -> Option<RecordIdx> {
        Some(idx)
    }
}
