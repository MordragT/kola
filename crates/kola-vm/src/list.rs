use std::{fmt, num::NonZeroU32};

use kola_utils::{display::DisplayWith, serde::SerializeWith};
use serde::ser::SerializeSeq;

use crate::{heap::Heap, value::Value};

/// A 4-byte pointer to a node inside the `ListArena`.
/// Uses `NonZeroU32` so that `Option<ListIdx>` (representing an empty list)
/// takes up exactly 4 bytes via niche optimization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ListIdx(NonZeroU32);

impl ListIdx {
    #[inline]
    pub fn as_usize(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[inline]
    fn make(offset: usize) -> Self {
        // Max 4 billion nodes in the arena
        let raw = (offset + 1) as u32;
        Self(NonZeroU32::new(raw).expect("ListArena offset overflow"))
    }
}

impl DisplayWith<Heap> for ListIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        f.debug_list()
            .entries(heap.lists.iter(Some(*self)))
            .finish()
    }
}

impl SerializeWith<Heap> for ListIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let list = heap.lists.iter(Some(*self)).collect::<Vec<_>>();
        let mut seq = serializer.serialize_seq(Some(list.len()))?;

        for v in list {
            seq.serialize_element(&heap.with(&v))?;
        }

        seq.end()
    }
}

/// A standard functional immutable list node (Cons cell).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ListNode {
    head: Value,
    tail: Option<ListIdx>,
}

/// An arena that enforces true structural sharing for immutable lists.
///
/// Prepend operations (`push_front`) and head/tail breakdowns (`split_front`)
/// run in O(1) time and space, completely eliminating the O(N^2) memory explosions.
#[derive(Debug, Clone)]
pub struct ListArena {
    data: Vec<ListNode>,
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

    /// Get the length of the list by walking the spine.
    /// O(N) time complexity.
    pub fn len(&self, idx: Option<ListIdx>) -> usize {
        let mut count = 0;
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { tail, .. } = self.data[id.as_usize()];

            count += 1;
            curr = tail;
        }
        count
    }

    /// Check if the list is empty.
    /// O(1) time complexity.
    #[inline]
    pub fn is_empty(&self, idx: Option<ListIdx>) -> bool {
        idx.is_none()
    }

    /// Get the element at the given index within the list.
    /// O(N) time complexity.
    pub fn get_element(&self, idx: Option<ListIdx>, mut index: usize) -> Option<Value> {
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];

            if index == 0 {
                return Some(head);
            }
            index -= 1;
            curr = tail;
        }
        None
    }

    /// Get the first element (the head) of the list.
    /// O(1) time complexity.
    #[inline]
    pub fn first(&self, idx: Option<ListIdx>) -> Option<Value> {
        let id = idx?;
        let ListNode { head, .. } = self.data[id.as_usize()];
        Some(head)
    }

    /// Get the last element of the list.
    /// O(N) time complexity.
    pub fn last(&self, idx: Option<ListIdx>) -> Option<Value> {
        let mut curr = idx?;
        loop {
            let ListNode { head, tail } = self.data[curr.as_usize()];

            if let Some(next) = tail {
                curr = next;
            } else {
                return Some(head);
            }
        }
    }

    /// Check if the list contains the given value.
    /// O(N) time complexity.
    pub fn contains(&self, idx: Option<ListIdx>, value: Value) -> bool {
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];

            if head == value {
                return true;
            }
            curr = tail;
        }
        false
    }

    /// Iterate over the values in the list.
    #[inline]
    pub fn iter(&self, idx: Option<ListIdx>) -> ListIter<'_> {
        ListIter {
            arena: self,
            current: idx,
        }
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new list from a slice of values.
    /// Preserves slice order by building the chain backwards from the end.
    pub fn alloc(&mut self, values: &[Value]) -> Option<ListIdx> {
        let mut curr = None;
        for &val in values.iter().rev() {
            curr = Some(self.push_front(curr, val));
        }
        curr
    }

    /// Allocate an empty list.
    #[inline]
    pub fn alloc_empty(&mut self) -> Option<ListIdx> {
        None
    }

    /// Allocate a single-element list.
    #[inline]
    pub fn alloc_unit(&mut self, value: Value) -> Option<ListIdx> {
        Some(self.push_front(None, value))
    }

    // ── mutating operations (leveraging structural sharing) ───────

    /// Prepend a value to the front of the list.
    ///
    /// **O(1) Time and Space.** Zero memory copying.
    #[inline]
    pub fn push_front(&mut self, tail: Option<ListIdx>, head: Value) -> ListIdx {
        let offset = self.data.len();
        self.data.push(ListNode { head, tail });
        ListIdx::make(offset)
    }

    /// Append a value to the back of the list.
    ///
    /// O(N) Time and Space. Copies only the *spine* nodes, not the values.
    pub fn push_back(&mut self, idx: Option<ListIdx>, value: Value) -> ListIdx {
        let mut heads = Vec::new();
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];
            heads.push(head);
            curr = tail;
        }

        // Build backwards, starting with the new last element node
        let mut new_list = self.push_front(None, value);
        for head in heads.into_iter().rev() {
            new_list = self.push_front(Some(new_list), head);
        }
        new_list
    }

    /// Concatenate two lists together.
    ///
    /// O(Left N) Complexity. Copies the spine of the left list, linking it directly to the shared right list.
    pub fn concat(&mut self, left: Option<ListIdx>, right: Option<ListIdx>) -> Option<ListIdx> {
        let mut heads = Vec::new();
        let mut curr = left;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];

            heads.push(head);
            curr = tail;
        }

        let mut new_list = right;
        for head in heads.into_iter().rev() {
            new_list = Some(self.push_front(new_list, head));
        }
        new_list
    }

    /// Split off the first element.
    ///
    /// **O(1) Time and Space.** Instantly yields the head and the shared tail.
    pub fn split_front(&self, idx: Option<ListIdx>) -> Option<(Value, Option<ListIdx>)> {
        let id = idx?;
        let ListNode { head, tail } = self.data[id.as_usize()];

        Some((head, tail))
    }

    /// Split off the last element.
    ///
    /// O(N) Complexity. Rebuilds the spine without the terminal node.
    pub fn split_back(&mut self, idx: Option<ListIdx>) -> Option<(Option<ListIdx>, Value)> {
        let id = idx?;
        let mut heads = Vec::new();
        let mut curr = Some(id);
        while let Some(current_id) = curr {
            let ListNode { head, tail } = self.data[current_id.as_usize()];
            heads.push(head);
            curr = tail;
        }

        let last = heads.pop()?;
        let mut new_list = None;
        for head in heads.into_iter().rev() {
            new_list = Some(self.push_front(new_list, head));
        }
        Some((new_list, last))
    }

    /// Split the list at a given position.
    ///
    /// Rebuilds the prefix spine up to `index`, and connects it to the shared tail suffix.
    pub fn split_at(
        &mut self,
        idx: Option<ListIdx>,
        index: usize,
    ) -> (Option<ListIdx>, Option<ListIdx>) {
        let mut heads = Vec::new();
        let mut curr = idx;
        let mut i = 0;

        while i < index {
            if let Some(id) = curr {
                let ListNode { head, tail } = self.data[id.as_usize()];
                heads.push(head);
                curr = tail;
                i += 1;
            } else {
                break;
            }
        }

        let mut head_list = None;
        for head in heads.into_iter().rev() {
            head_list = Some(self.push_front(head_list, head));
        }

        (head_list, curr)
    }

    /// Reverse the list.
    ///
    /// O(N) Time and Space. Iterates forward, accumulating nodes backward.
    pub fn reverse(&mut self, idx: Option<ListIdx>) -> Option<ListIdx> {
        let mut curr = idx;
        let mut rev_list = None;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];
            rev_list = Some(self.push_front(rev_list, head));
            curr = tail;
        }
        rev_list
    }

    /// Replaces an item at a specific index.
    ///
    /// **Structural Sharing Win:** Only copies the spine nodes *up to* the modification index.
    /// The entire rest of the list after the modification index is directly shared with the old list.
    pub fn set(&mut self, idx: Option<ListIdx>, index: usize, value: Value) -> Option<ListIdx> {
        let mut heads = Vec::new();
        let mut curr = idx;
        let mut i = 0;

        while i < index {
            let id = curr?;
            let ListNode { head, tail } = self.data[id.as_usize()];

            heads.push(head);
            curr = tail;
            i += 1;
        }

        let target_id = curr?;
        let shared_tail = self.data[target_id.as_usize()].tail;

        let mut new_list = Some(self.push_front(shared_tail, value));
        for head in heads.into_iter().rev() {
            new_list = Some(self.push_front(new_list, head));
        }
        new_list
    }

    /// Fold over the elements of the list.
    pub fn fold<B, F>(&self, idx: Option<ListIdx>, init: B, mut f: F) -> B
    where
        F: FnMut(B, Value) -> B,
    {
        let mut acc = init;
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];

            acc = f(acc, head);
            curr = tail;
        }
        acc
    }

    /// Try to fold over the elements of the list with a failing function.
    pub fn try_fold<B, F, E>(&self, idx: Option<ListIdx>, init: B, mut f: F) -> Result<B, E>
    where
        F: FnMut(B, Value) -> Result<B, E>,
    {
        let mut acc = init;
        let mut curr = idx;
        while let Some(id) = curr {
            let ListNode { head, tail } = self.data[id.as_usize()];
            acc = f(acc, head)?;
            curr = tail;
        }
        Ok(acc)
    }
}

// ── Iterator Support ──────────────────────────────────────────

pub struct ListIter<'a> {
    arena: &'a ListArena,
    current: Option<ListIdx>,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.current?;
        let ListNode { head, tail } = self.arena.data[id.as_usize()];
        self.current = tail;
        Some(head)
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

    /// Test helper to drain the linked list iterator into a flat Vec
    /// so we can use readable array-style equality assertions.
    fn to_vec(arena: &ListArena, idx: Option<ListIdx>) -> Vec<Value> {
        arena.iter(idx).collect()
    }

    #[test]
    fn test_alloc_and_get() {
        let mut arena = ListArena::new();
        let empty = arena.alloc_empty(); // returns None
        assert!(arena.is_empty(empty));
        assert_eq!(arena.len(empty), 0);

        let single = arena.alloc_unit(Value::Num(42.0));
        assert_eq!(arena.len(single), 1);
        assert_eq!(to_vec(&arena, single), vec![Value::Num(42.0)]);
    }

    #[test]
    fn test_alloc_from_slice() {
        let mut arena = ListArena::new();
        let values = vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)];
        let idx = arena.alloc(&values);

        assert_eq!(arena.len(idx), 3);
        assert_eq!(to_vec(&arena, idx), values);
    }

    #[test]
    fn test_push_front() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(2.0), Value::Num(3.0)]);

        let new = Some(arena.push_front(idx, Value::Num(1.0)));
        assert_eq!(arena.len(new), 3);
        assert_eq!(
            to_vec(&arena, new),
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
        // Original is perfectly preserved and untouched
        assert_eq!(arena.len(idx), 2);
    }

    #[test]
    fn test_push_back() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0)]);

        let new = Some(arena.push_back(idx, Value::Num(3.0)));
        assert_eq!(arena.len(new), 3);
        assert_eq!(
            to_vec(&arena, new),
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
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
            to_vec(&arena, result),
            vec![
                Value::Num(1.0),
                Value::Num(2.0),
                Value::Num(3.0),
                Value::Num(4.0)
            ]
        );
    }

    #[test]
    fn test_split_front() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (first, tail) = arena.split_front(idx).unwrap();
        assert_eq!(first, Value::Num(1.0));
        assert_eq!(to_vec(&arena, tail), vec![Value::Num(2.0), Value::Num(3.0)]);
        // Original is untouched
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_back() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, last) = arena.split_back(idx).unwrap();
        assert_eq!(last, Value::Num(3.0));
        assert_eq!(to_vec(&arena, head), vec![Value::Num(1.0), Value::Num(2.0)]);
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_at() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, tail) = arena.split_at(idx, 1);
        assert_eq!(to_vec(&arena, head), vec![Value::Num(1.0)]);
        assert_eq!(to_vec(&arena, tail), vec![Value::Num(2.0), Value::Num(3.0)]);
    }

    #[test]
    fn test_reverse() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let reversed = arena.reverse(idx);
        assert_eq!(
            to_vec(&arena, reversed),
            vec![Value::Num(3.0), Value::Num(2.0), Value::Num(1.0)]
        );
        // Original is untouched
        assert_eq!(
            to_vec(&arena, idx),
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );
    }

    #[test]
    fn test_set() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let new = arena.set(idx, 1, Value::Num(99.0));
        assert_eq!(
            to_vec(&arena, new),
            vec![Value::Num(1.0), Value::Num(99.0), Value::Num(3.0)]
        );
        // Original is untouched
        assert_eq!(
            to_vec(&arena, idx),
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
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
        let extended = Some(arena.push_front(idx, Value::Num(0.0)));
        let (first, tail) = arena.split_front(extended).unwrap();
        assert_eq!(first, Value::Num(0.0));
        assert_eq!(
            to_vec(&arena, tail),
            vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]
        );

        // Concat, then reverse
        let right = arena.alloc(&[Value::Num(4.0), Value::Num(5.0)]);
        let combined = arena.concat(tail, right);
        let reversed = arena.reverse(combined);
        assert_eq!(
            to_vec(&arena, reversed),
            vec![
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
