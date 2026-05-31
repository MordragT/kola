use std::{fmt, num::NonZeroU32, range::Range};

use kola_utils::interner::StrKey;

use crate::value::Value;

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
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

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

    /// Get a value by key (returns the first/shadowing entry).
    pub fn get_value(&self, idx: RecordIdx, key: StrKey) -> Option<&Value> {
        let slice = self.get(idx);
        slice
            .binary_search_by_key(&key, |(k, _)| *k)
            .ok()
            .map(|pos| &slice[pos].1)
    }

    /// Check if the record contains the given key.
    pub fn contains_key(&self, idx: RecordIdx, key: StrKey) -> bool {
        let slice = self.get(idx);
        slice.binary_search_by_key(&key, |(k, _)| *k).is_ok()
    }

    /// Get the first (oldest) entry.
    pub fn first(&self, idx: RecordIdx) -> Option<(&StrKey, &Value)> {
        self.get(idx).first().map(|(k, v)| (k, v))
    }

    /// Iterate over all entries (including shadowed).
    pub fn iter(&self, idx: RecordIdx) -> impl Iterator<Item = &(StrKey, Value)> {
        self.get(idx).iter()
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new record from a slice of entries.
    /// The entries must be sorted by key.
    #[inline]
    pub fn alloc(&mut self, entries: &[(StrKey, Value)]) -> RecordIdx {
        let start = self.data.len();
        self.data.extend_from_slice(entries);
        let end = self.data.len();
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
    /// existing one, shadowing it. The old entry is skipped in the copy.
    ///
    /// Returns a new index for the resulting record.
    pub fn insert(&mut self, idx: RecordIdx, key: StrKey, value: Value) -> RecordIdx {
        let start = idx.start();
        let end = idx.end();
        let slice = &self.data[start..end];

        // Find insertion point
        let insert_pos = match slice.binary_search_by_key(&key, |(k, _)| *k) {
            Ok(pos) => pos, // insert at existing key position → shadows it
            Err(pos) => pos,
        };

        // Check if we're shadowing an existing entry
        let has_shadow = insert_pos < (end - start) && self.data[start + insert_pos].0 == key;
        let skip_start = if has_shadow {
            start + insert_pos + 1
        } else {
            start + insert_pos
        };

        let new_start = self.data.len();

        // Copy entries before insertion point
        if insert_pos > 0 {
            self.data.extend_from_within(start..start + insert_pos);
        }

        // Insert new entry
        self.data.push((key, value));

        // Copy entries after insertion point (skipping shadowed entry if any)
        if skip_start < end {
            self.data.extend_from_within(skip_start..end);
        }

        let new_end = self.data.len();
        RecordIdx::make(new_start, new_end)
    }

    /// Remove the first (visible) occurrence of `key`.
    ///
    /// Returns a new index for the resulting record.
    /// If the key doesn't exist, returns the original index unchanged.
    pub fn remove(&self, idx: RecordIdx, key: StrKey) -> RecordIdx {
        let start = idx.start();
        let end = idx.end();
        let slice = &self.data[start..end];

        let pos = match slice.binary_search_by_key(&key, |(k, _)| *k) {
            Ok(pos) => pos,
            Err(_) => return idx, // key not found, return unchanged
        };

        let new_start = self.data.len();

        // Copy entries before the removed entry
        if pos > 0 {
            self.data.extend_from_within(start..start + pos);
        }

        // Copy entries after the removed entry
        if start + pos + 1 < end {
            self.data.extend_from_within(start + pos + 1..end);
        }

        let new_end = self.data.len();
        RecordIdx::make(new_start, new_end)
    }

    /// Merge two records, left-biased: keys from `left` shadow `right`.
    ///
    /// Both records must be sorted by key. The result is also sorted.
    pub fn merge_left(&mut self, left: RecordIdx, right: RecordIdx) -> RecordIdx {
        let l_start = left.start();
        let l_end = left.end();
        let r_start = right.start();
        let r_end = right.end();

        let new_start = self.data.len();

        // Sorted merge: interleave entries from both records
        let mut i = l_start;
        let mut j = r_start;

        while i < l_end && j < r_end {
            match self.data[i].0.cmp(&self.data[j].0) {
                std::cmp::Ordering::Less => {
                    self.data.push(self.data[i].clone());
                    i += 1;
                }
                std::cmp::Ordering::Greater => {
                    self.data.push(self.data[j].clone());
                    j += 1;
                }
                std::cmp::Ordering::Equal => {
                    // Left shadows right
                    self.data.push(self.data[i].clone());
                    i += 1;
                    j += 1;
                }
            }
        }

        // Drain remaining
        if i < l_end {
            self.data.extend_from_within(i..l_end);
        }
        if j < r_end {
            self.data.extend_from_within(j..r_end);
        }

        let new_end = self.data.len();
        RecordIdx::make(new_start, new_end)
    }

    /// Merge two records, right-biased: keys from `right` shadow `left`.
    pub fn merge_right(&mut self, left: RecordIdx, right: RecordIdx) -> RecordIdx {
        self.merge_left(right, left)
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
    use kola_utils::interner::StrInterner;

    fn key(interner: &mut StrInterner, s: &str) -> StrKey {
        interner.intern(s)
    }

    #[test]
    fn test_alloc_empty() {
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();
        assert!(arena.is_empty(idx));
        assert_eq!(arena.len_all(idx), 0);
    }

    #[test]
    fn test_alloc_unit() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let k = key(&mut interner, "x");
        let idx = arena.alloc_unit(k, Value::Num(42.0));

        assert_eq!(arena.len_all(idx), 1);
        assert_eq!(arena.get_value(idx, k), Some(&Value::Num(42.0)));
    }

    #[test]
    fn test_insert_basic() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let kx = key(&mut interner, "x");
        let ky = key(&mut interner, "y");
        let kz = key(&mut interner, "z");

        let idx = arena.insert(idx, ky, Value::Num(2.0));
        let idx = arena.insert(idx, kx, Value::Num(1.0));
        let idx = arena.insert(idx, kz, Value::Num(3.0));

        assert_eq!(arena.len_all(idx), 3);
        // Entries are sorted: x, y, z
        assert_eq!(arena.get_value(idx, kx), Some(&Value::Num(1.0)));
        assert_eq!(arena.get_value(idx, ky), Some(&Value::Num(2.0)));
        assert_eq!(arena.get_value(idx, kz), Some(&Value::Num(3.0)));
    }

    #[test]
    fn test_insert_shadow() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let kx = key(&mut interner, "x");

        let idx = arena.insert(idx, kx, Value::Num(1.0));
        assert_eq!(arena.get_value(idx, kx), Some(&Value::Num(1.0)));

        // Insert same key → shadows the old one
        let idx = arena.insert(idx, kx, Value::Num(99.0));
        assert_eq!(arena.get_value(idx, kx), Some(&Value::Num(99.0)));
        // Still only 1 visible entry, but 2 total (shadowed)
        assert_eq!(arena.len_all(idx), 2);
    }

    #[test]
    fn test_remove() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let kx = key(&mut interner, "x");
        let ky = key(&mut interner, "y");

        let idx = arena.insert(idx, kx, Value::Num(1.0));
        let idx = arena.insert(idx, ky, Value::Num(2.0));

        let idx = arena.remove(idx, kx);
        assert_eq!(arena.get_value(idx, kx), None);
        assert_eq!(arena.get_value(idx, ky), Some(&Value::Num(2.0)));
    }

    #[test]
    fn test_remove_unshadow() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let kx = key(&mut interner, "x");

        // Insert two values for the same key
        let idx = arena.insert(idx, kx, Value::Num(1.0));
        let idx = arena.insert(idx, kx, Value::Num(2.0));

        // Visible value is 2.0
        assert_eq!(arena.get_value(idx, kx), Some(&Value::Num(2.0)));

        // Remove the visible one → 1.0 resurfaces
        let idx = arena.remove(idx, kx);
        assert_eq!(arena.get_value(idx, kx), Some(&Value::Num(1.0)));
    }

    #[test]
    fn test_remove_not_found() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let kx = key(&mut interner, "x");
        let ky = key(&mut interner, "y");

        let idx = arena.insert(idx, kx, Value::Num(1.0));
        let original = idx;

        // Remove non-existent key → returns same index
        let idx = arena.remove(idx, ky);
        assert_eq!(idx, original);
    }

    #[test]
    fn test_merge_left() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();

        let kx = key(&mut interner, "x");
        let ky = key(&mut interner, "y");
        let kz = key(&mut interner, "z");

        let left = arena.alloc_empty();
        let left = arena.insert(left, kx, Value::Num(1.0));
        let left = arena.insert(left, ky, Value::Num(2.0));

        let right = arena.alloc_empty();
        let right = arena.insert(right, ky, Value::Num(20.0));
        let right = arena.insert(right, kz, Value::Num(3.0));

        let merged = arena.merge_left(left, right);

        // x from left, y from left (shadows right), z from right
        assert_eq!(arena.get_value(merged, kx), Some(&Value::Num(1.0)));
        assert_eq!(arena.get_value(merged, ky), Some(&Value::Num(2.0)));
        assert_eq!(arena.get_value(merged, kz), Some(&Value::Num(3.0)));
    }

    #[test]
    fn test_merge_right() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();

        let kx = key(&mut interner, "x");
        let ky = key(&mut interner, "y");
        let kz = key(&mut interner, "z");

        let left = arena.alloc_empty();
        let left = arena.insert(left, kx, Value::Num(1.0));
        let left = arena.insert(left, ky, Value::Num(2.0));

        let right = arena.alloc_empty();
        let right = arena.insert(right, ky, Value::Num(20.0));
        let right = arena.insert(right, kz, Value::Num(3.0));

        let merged = arena.merge_right(left, right);

        // x from left, y from right (shadows left), z from right
        assert_eq!(arena.get_value(merged, kx), Some(&Value::Num(1.0)));
        assert_eq!(arena.get_value(merged, ky), Some(&Value::Num(20.0)));
        assert_eq!(arena.get_value(merged, kz), Some(&Value::Num(3.0)));
    }

    #[test]
    fn test_iter_visible() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let ka = key(&mut interner, "a");
        let kb = key(&mut interner, "b");

        let idx = arena.insert(idx, ka, Value::Num(1.0));
        let idx = arena.insert(idx, kb, Value::Num(2.0));
        let idx = arena.insert(idx, ka, Value::Num(10.0)); // shadow

        let visible: Vec<_> = arena.iter(idx).collect();
        assert_eq!(visible.len(), 2);
        assert_eq!(visible[0].1, Value::Num(10.0)); // a: 10 (shadowing)
        assert_eq!(visible[1].1, Value::Num(2.0)); // b: 2
    }

    #[test]
    fn test_iter_all() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();
        let idx = arena.alloc_empty();

        let ka = key(&mut interner, "a");
        let idx = arena.insert(idx, ka, Value::Num(1.0));
        let idx = arena.insert(idx, ka, Value::Num(2.0));

        let all: Vec<_> = arena.iter_all(idx).collect();
        assert_eq!(all.len(), 2);
    }

    #[test]
    fn test_copy_preserves_original() {
        let mut interner = StrInterner::new();
        let mut arena = RecordArena::new();

        let kx = key(&mut interner, "x");
        let original = arena.alloc_unit(kx, Value::Num(1.0));
        let copy = arena.copy(original);

        let ky = key(&mut interner, "y");
        let modified = arena.insert(copy, ky, Value::Num(2.0));

        // Original untouched
        assert_eq!(arena.len_all(original), 1);
        assert_eq!(arena.len_all(copy), 1);
        assert_eq!(arena.len_all(modified), 2);
    }
}
