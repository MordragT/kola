use std::{fmt, num::NonZeroU32, range::Range};

use kola_utils::{display::DisplayWith, serde::SerializeWith};
use serde::ser::SerializeSeq;

use crate::{heap::Heap, value::Value};

/// A `Copy`-friendly index into a `ListArena`.
///
/// Represents a contiguous range of `Value`s in the arena's backing storage.
/// All arena operations take this index rather than `&[Value]`,
/// enabling `Value::List(ListIdx)` to be `Copy` (8 bytes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ListIdx(Range<NonZeroU32>);

impl ListIdx {
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
        ListIdx(Range {
            start: NonZeroU32::new((start + 1) as u32).expect("list arena overflow"),
            end: NonZeroU32::new((end + 1) as u32).expect("list arena overflow"),
        })
    }
}

impl DisplayWith<Heap> for ListIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        let slice = heap.lists.get(*self);
        f.debug_list().entries(slice.iter()).finish()
    }
}

impl SerializeWith<Heap> for ListIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let slice = heap.lists.get(*self);
        let mut seq = serializer.serialize_seq(Some(slice.len()))?;

        for v in slice {
            seq.serialize_element(&heap.with(v))?;
        }

        seq.end()
    }
}

/// An append-only arena for lists.
///
/// All list elements are stored contiguously in a single `Vec<Value>`.
/// `ListIdx` values point into this backing storage and are `Copy`.
///
/// Mutating operations (`push_front`, `split_first`, etc.) use
/// `extend_from_within` to copy values directly within the arena.
/// No heap allocation occurs for any operation.
#[derive(Debug, Clone)]
pub struct ListArena {
    data: Vec<Value>,
}

impl ListArena {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    // ── read-only operations ──────────────────────────────────────

    /// Get the slice of values for the given index.
    #[inline]
    pub fn get(&self, idx: ListIdx) -> &[Value] {
        &self.data[idx.start()..idx.end()]
    }

    /// Get the length of the list.
    #[inline]
    pub fn len(&self, idx: ListIdx) -> usize {
        idx.len()
    }

    /// Check if the list is empty.
    #[inline]
    pub fn is_empty(&self, idx: ListIdx) -> bool {
        idx.is_empty()
    }

    /// Get a reference to the element at the given index within the list.
    #[inline]
    pub fn get_element(&self, idx: ListIdx, index: usize) -> Option<Value> {
        self.get(idx).get(index).copied()
    }

    /// Get a reference to the first element.
    #[inline]
    pub fn first(&self, idx: ListIdx) -> Option<Value> {
        self.get(idx).first().copied()
    }

    /// Get a reference to the last element.
    #[inline]
    pub fn last(&self, idx: ListIdx) -> Option<Value> {
        self.get(idx).last().copied()
    }

    /// Check if the list contains the given value.
    #[inline]
    pub fn contains(&self, idx: ListIdx, value: Value) -> bool {
        self.get(idx).contains(&value)
    }

    /// Iterate over the values in the list.
    #[inline]
    pub fn iter(&self, idx: ListIdx) -> impl Iterator<Item = Value> {
        self.get(idx).iter().copied()
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new list from a slice of values.
    #[inline]
    pub fn alloc(&mut self, values: &[Value]) -> ListIdx {
        let start = self.data.len();
        self.data.extend_from_slice(values);
        let end = self.data.len();
        ListIdx::make(start, end)
    }

    /// Allocate an empty list.
    #[inline]
    pub fn alloc_empty(&mut self) -> ListIdx {
        let pos = self.data.len();
        ListIdx::make(pos, pos)
    }

    /// Allocate a single-element list.
    #[inline]
    pub fn alloc_unit(&mut self, value: Value) -> ListIdx {
        let start = self.data.len();
        self.data.push(value);
        let end = self.data.len();
        ListIdx::make(start, end)
    }

    /// Allocate a new list from an iterator of values.
    #[inline]
    pub fn alloc_iter<I>(&mut self, iter: I) -> ListIdx
    where
        I: IntoIterator<Item = Value>,
    {
        let start = self.data.len();
        self.data.extend(iter);
        let end = self.data.len();
        ListIdx::make(start, end)
    }

    /// Allocate a new list from an iterator of `Result<Value, E>`.
    #[inline]
    pub fn try_alloc_iter<I, E>(&mut self, iter: I) -> Result<ListIdx, E>
    where
        I: IntoIterator<Item = Result<Value, E>>,
    {
        let start = self.data.len();
        for item in iter {
            self.data.push(item?);
        }
        let end = self.data.len();
        Ok(ListIdx::make(start, end))
    }

    /// Copy an existing list within the arena.
    /// Returns a new index pointing to an identical copy.
    #[inline]
    pub fn copy(&mut self, idx: ListIdx) -> ListIdx {
        let start = self.data.len();
        self.data.extend_from_within(idx.start()..idx.end());
        let end = self.data.len();
        ListIdx::make(start, end)
    }

    // ── mutating operations (copy values within arena) ────────────

    /// Prepend a value to the list.
    ///
    /// Returns a new index for the resulting list.
    /// The original list is untouched.
    pub fn push_front(&mut self, idx: ListIdx, value: Value) -> ListIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Push the new value first
        self.data.push(value);

        // Then copy the original list
        self.data.extend_from_within(start..end);

        let new_end = self.data.len();
        ListIdx::make(new_start, new_end)
    }

    /// Append a value to the list.
    ///
    /// Returns a new index for the resulting list.
    /// The original list is untouched.
    pub fn push_back(&mut self, idx: ListIdx, value: Value) -> ListIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Copy the original list first
        self.data.extend_from_within(start..end);

        // Then push the new value
        self.data.push(value);

        let new_end = self.data.len();
        ListIdx::make(new_start, new_end)
    }

    /// Concatenate two lists.
    ///
    /// Returns a new index for the resulting list.
    pub fn concat(&mut self, left: ListIdx, right: ListIdx) -> ListIdx {
        let new_start = self.data.len();
        self.data.extend_from_within(left.start()..left.end());
        self.data.extend_from_within(right.start()..right.end());
        let new_end = self.data.len();
        ListIdx::make(new_start, new_end)
    }

    /// Split off the first element.
    ///
    /// Returns `Some((first, tail_idx))` if the list is non-empty.
    /// The original list is untouched.
    pub fn split_front(&mut self, idx: ListIdx) -> Option<(Value, ListIdx)> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let first = self.data[start];

        // Copy the tail (all elements except the first)
        let new_start = self.data.len();
        self.data.extend_from_within(start + 1..end);
        let new_end = self.data.len();

        Some((first, ListIdx::make(new_start, new_end)))
    }

    /// Split off the last element.
    ///
    /// Returns `Some((head_idx, last))` if the list is non-empty.
    /// The original list is untouched.
    pub fn split_back(&mut self, idx: ListIdx) -> Option<(ListIdx, Value)> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let last = self.data[end - 1];

        // Copy the head (all elements except the last)
        let new_start = self.data.len();
        self.data.extend_from_within(start..end - 1);
        let new_end = self.data.len();

        Some((ListIdx::make(new_start, new_end), last))
    }

    /// Split the list at the given index.
    ///
    /// Returns `(head_idx, tail_idx)`. The original list is untouched.
    pub fn split_at(&mut self, idx: ListIdx, index: usize) -> (ListIdx, ListIdx) {
        let start = idx.start();
        let end = idx.end();

        let head_start = self.data.len();
        self.data.extend_from_within(start..start + index);
        let head_end = self.data.len();

        let tail_start = self.data.len();
        self.data.extend_from_within(start + index..end);
        let tail_end = self.data.len();

        (
            ListIdx::make(head_start, head_end),
            ListIdx::make(tail_start, tail_end),
        )
    }

    /// Reverse the list.
    ///
    /// Returns a new index for the reversed list.
    /// The original list is untouched.
    pub fn reverse(&mut self, idx: ListIdx) -> ListIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Copy elements in reverse order
        for i in (start..end).rev() {
            self.data.push(self.data[i]);
        }

        let new_end = self.data.len();
        ListIdx::make(new_start, new_end)
    }

    /// Set the element at the given index within the list.
    ///
    /// Returns a new index for the modified list.
    /// The original list is untouched.
    pub fn set(&mut self, idx: ListIdx, index: usize, value: Value) -> ListIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Copy all elements, replacing the one at `index`
        for i in start..end {
            if i - start == index {
                self.data.push(value);
            } else {
                self.data.push(self.data[i]);
            }
        }

        let new_end = self.data.len();
        ListIdx::make(new_start, new_end)
    }

    /// Fold over the elements of the list with a function.
    pub fn fold<B, F>(&mut self, idx: ListIdx, init: B, mut f: F) -> B
    where
        F: FnMut(B, &Value) -> B,
    {
        let mut acc = init;
        for v in self.get(idx) {
            acc = f(acc, v);
        }
        acc
    }

    /// Try to fold over the elements of the list with a function that can fail.
    pub fn try_fold<B, F, E>(&mut self, idx: ListIdx, init: B, mut f: F) -> Result<B, E>
    where
        F: FnMut(B, &Value) -> Result<B, E>,
    {
        let mut acc = init;
        for v in self.get(idx) {
            acc = f(acc, v)?;
        }
        Ok(acc)
    }
}

impl Default for ListArena {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ListArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ListArena({} values)", self.data.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_and_get() {
        let mut arena = ListArena::new();
        let empty = arena.alloc_empty();
        assert!(arena.is_empty(empty));
        assert_eq!(arena.len(empty), 0);

        let single = arena.alloc_unit(Value::Num(42.0));
        assert_eq!(arena.len(single), 1);
        assert_eq!(arena.get(single), &[Value::Num(42.0)]);
    }

    #[test]
    fn test_alloc_from_slice() {
        let mut arena = ListArena::new();
        let values = vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)];
        let idx = arena.alloc(&values);

        assert_eq!(arena.len(idx), 3);
        assert_eq!(arena.get(idx), &values);
    }

    #[test]
    fn test_copy() {
        let mut arena = ListArena::new();
        let original = arena.alloc(&[Value::Num(1.0), Value::Num(2.0)]);
        let copy = arena.copy(original);

        assert_eq!(arena.get(original), arena.get(copy));
        // Different indices, same content
        assert_ne!(original, copy);
    }

    #[test]
    fn test_push_front() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(2.0), Value::Num(3.0)]);

        let new = arena.push_front(idx, Value::Num(1.0));
        assert_eq!(arena.len(new), 3);
        assert_eq!(
            arena.get(new),
            &[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
        // Original is untouched
        assert_eq!(arena.len(idx), 2);
    }

    #[test]
    fn test_push_back() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0)]);

        let new = arena.push_back(idx, Value::Num(3.0));
        assert_eq!(arena.len(new), 3);
        assert_eq!(
            arena.get(new),
            &[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
        assert_eq!(arena.len(idx), 2);
    }

    #[test]
    fn test_concat() {
        let mut arena = ListArena::new();
        let left = arena.alloc(&[Value::Num(1.0), Value::Num(2.0)]);
        let right = arena.alloc(&[Value::Num(3.0), Value::Num(4.0)]);

        let result = arena.concat(left, right);
        assert_eq!(arena.len(result), 4);
        assert_eq!(
            arena.get(result),
            &[
                Value::Num(1.0),
                Value::Num(2.0),
                Value::Num(3.0),
                Value::Num(4.0)
            ]
        );
    }

    #[test]
    fn test_split_first() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (first, tail) = arena.split_front(idx).unwrap();
        assert_eq!(first, Value::Num(1.0));
        assert_eq!(arena.get(tail), &[Value::Num(2.0), Value::Num(3.0)]);
        // Original is untouched
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_last() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, last) = arena.split_back(idx).unwrap();
        assert_eq!(last, Value::Num(3.0));
        assert_eq!(arena.get(head), &[Value::Num(1.0), Value::Num(2.0)]);
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_at() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, tail) = arena.split_at(idx, 1);
        assert_eq!(arena.get(head), &[Value::Num(1.0)]);
        assert_eq!(arena.get(tail), &[Value::Num(2.0), Value::Num(3.0)]);
    }

    #[test]
    fn test_reverse() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let reversed = arena.reverse(idx);
        assert_eq!(
            arena.get(reversed),
            &[Value::Num(3.0), Value::Num(2.0), Value::Num(1.0)]
        );
        // Original is untouched
        assert_eq!(
            arena.get(idx),
            &[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
    }

    #[test]
    fn test_set() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let new = arena.set(idx, 1, Value::Num(99.0));
        assert_eq!(
            arena.get(new),
            &[Value::Num(1.0), Value::Num(99.0), Value::Num(3.0)]
        );
        // Original is untouched
        assert_eq!(
            arena.get(idx),
            &[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
    }

    #[test]
    fn test_split_first_empty() {
        let mut arena = ListArena::new();
        let idx = arena.alloc_empty();

        assert!(arena.split_front(idx).is_none());
    }

    #[test]
    fn test_split_last_empty() {
        let mut arena = ListArena::new();
        let idx = arena.alloc_empty();

        assert!(arena.split_back(idx).is_none());
    }

    #[test]
    fn test_chained_operations() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        // Push front, then split first
        let extended = arena.push_front(idx, Value::Num(0.0));
        let (first, tail) = arena.split_front(extended).unwrap();
        assert_eq!(first, Value::Num(0.0));
        assert_eq!(
            arena.get(tail),
            &[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );

        // Concat, then reverse
        let right = arena.alloc(&[Value::Num(4.0), Value::Num(5.0)]);
        let combined = arena.concat(tail, right);
        let reversed = arena.reverse(combined);
        assert_eq!(
            arena.get(reversed),
            &[
                Value::Num(5.0),
                Value::Num(4.0),
                Value::Num(3.0),
                Value::Num(2.0),
                Value::Num(1.0)
            ]
        );
    }

    #[test]
    fn test_contains() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        assert!(arena.contains(idx, Value::Num(2.0)));
        assert!(!arena.contains(idx, Value::Num(4.0)));
    }

    #[test]
    fn test_iter() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let values: Vec<Value> = arena.iter(idx).collect();
        assert_eq!(
            values,
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
    }
}
