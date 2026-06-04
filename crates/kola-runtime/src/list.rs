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
/// run in O(1) time and space.
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

    /// Allocates a structurally shared list directly from a forward-moving iterator.
    /// Preserves natural iterator order via forward backpatching.
    /// Instantly rolls back all structural changes if an error is encountered.
    pub fn try_alloc_from_iter<I, E>(&mut self, iter: I) -> Result<Option<ListIdx>, E>
    where
        I: IntoIterator<Item = Result<Value, E>>,
    {
        let start_len = self.data.len();
        let iter = iter.into_iter();

        // Size hint optimization to avoid intermediate vector resizing
        if let Some(upper_bound) = iter.size_hint().1 {
            self.data.reserve(upper_bound);
        }

        let mut prev_offset: Option<usize> = None;

        for item_result in iter {
            let val = match item_result {
                Ok(v) => v,
                Err(err) => {
                    // Transactional Rollback: Wipe out all partially written nodes
                    self.data.truncate(start_len);
                    return Err(err);
                }
            };

            let curr_offset = self.data.len();
            self.data.push(ListNode {
                head: val,
                tail: None,
            });

            // Backpatching: Update the previous node's tail to point to this new node
            if let Some(prev) = prev_offset {
                self.data[prev].tail = Some(ListIdx::make(curr_offset));
            }

            prev_offset = Some(curr_offset);
        }

        if self.data.len() == start_len {
            Ok(None)
        } else {
            // The head of the entire list is the very first node we pushed
            Ok(Some(ListIdx::make(start_len)))
        }
    }

    /// Allocates a structurally shared list directly from a non-failing forward iterator.
    /// Preserves natural iterator order via forward backpatching.
    pub fn alloc_from_iter<I>(&mut self, iter: I) -> Option<ListIdx>
    where
        I: IntoIterator<Item = Value>,
    {
        let start_len = self.data.len();
        let iter = iter.into_iter();

        // Size hint optimization to avoid intermediate vector resizing
        if let Some(upper_bound) = iter.size_hint().1 {
            self.data.reserve(upper_bound);
        }

        let mut prev_offset: Option<usize> = None;

        for val in iter {
            let curr_offset = self.data.len();
            self.data.push(ListNode {
                head: val,
                tail: None,
            });

            // Backpatching: Update the previous node's tail to point to this new node
            if let Some(prev) = prev_offset {
                self.data[prev].tail = Some(ListIdx::make(curr_offset));
            }

            prev_offset = Some(curr_offset);
        }

        if self.data.len() == start_len {
            None
        } else {
            // The head of the list is the very first node we pushed
            Some(ListIdx::make(start_len))
        }
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
    /// O(N) Time Complexity. Copies only the *spine* nodes in a single forward pass.
    /// Auxiliary Space Complexity: O(1) (Zero temporary heap allocations).
    pub fn push_back(&mut self, idx: Option<ListIdx>, value: Value) -> ListIdx {
        // Step 1: Allocate the terminal node containing the new value right away.
        let new_tail_idx = self.push_front(None, value);

        // Base case: If the original list is empty, our new tail is the entire list.
        let mut curr = match idx {
            Some(start_idx) => start_idx,
            None => return new_tail_idx,
        };

        let mut new_head = None;
        let mut prev_new_id: Option<ListIdx> = None;

        // Step 2: Traverse and link forward
        loop {
            let ListNode { head, tail } = self.data[curr.as_usize()];

            match tail {
                Some(next_idx) => {
                    // `curr` has a successor; allocate a copy for the new spine.
                    let new_id = self.push_front(None, head);

                    if new_head.is_none() {
                        new_head = Some(new_id);
                    }

                    // Link the previous node's tail to this new node
                    if let Some(prev_id) = prev_new_id {
                        self.data[prev_id.as_usize()].tail = Some(new_id);
                    }

                    prev_new_id = Some(new_id);
                    curr = next_idx;
                }
                None => {
                    // `curr` is the terminal node of the original list.
                    // Link the final node of the new spine directly to our pre-allocated `new_tail_idx`.
                    let final_new_id = self.push_front(Some(new_tail_idx), head);

                    if let Some(prev_id) = prev_new_id {
                        self.data[prev_id.as_usize()].tail = Some(final_new_id);
                        return new_head.unwrap(); // Infallible, since new_head was set on loop 1
                    } else {
                        // The original list had exactly one element.
                        return final_new_id;
                    }
                }
            }
        }
    }

    /// Concatenate two lists together.
    ///
    /// O(Left N) Complexity. Copies the spine of the left list in a single forward pass,
    /// linking its terminal node directly to the shared right list.
    /// Auxiliary Space Complexity: O(1) (Zero temporary allocations).
    pub fn concat(&mut self, left: Option<ListIdx>, right: Option<ListIdx>) -> Option<ListIdx> {
        // Base case: If the left list is empty, we don't need to copy anything.
        let mut curr = match left {
            Some(idx) => idx,
            None => return right,
        };

        let mut new_head = None;
        let mut prev_new_id: Option<ListIdx> = None;

        loop {
            let ListNode { head, tail } = self.data[curr.as_usize()];

            match tail {
                Some(next_idx) => {
                    // `curr` has a successor; allocate a new node copy.
                    // We temporarily set its tail to None.
                    let new_id = self.push_front(None, head);

                    if new_head.is_none() {
                        new_head = Some(new_id);
                    }

                    // Forward-link the previous node in the new spine to this one
                    if let Some(prev_id) = prev_new_id {
                        self.data[prev_id.as_usize()].tail = Some(new_id);
                    }

                    prev_new_id = Some(new_id);
                    curr = next_idx;
                }
                None => {
                    // `curr` is the terminal node of the left list.
                    // Allocate the final node of the new spine, pointing its tail directly to `right`.
                    let final_new_id = self.push_front(right, head);

                    if let Some(prev_id) = prev_new_id {
                        self.data[prev_id.as_usize()].tail = Some(final_new_id);
                        return new_head;
                    } else {
                        // The left list had exactly one element.
                        return Some(final_new_id);
                    }
                }
            }
        }
    }

    /// Split off the first element.
    ///
    /// **O(1) Time and Space.** Instantly yields the head and the shared tail.
    pub fn split_front(&self, idx: ListIdx) -> (Value, Option<ListIdx>) {
        let ListNode { head, tail } = self.data[idx.as_usize()];

        (head, tail)
    }

    /// Split off the last element.
    ///
    /// O(N) Complexity. Rebuilds the spine without the terminal node in a single pass.
    /// Auxiliary Space Complexity: O(1) (No temporary heap allocations).
    pub fn split_back(&mut self, idx: ListIdx) -> (Option<ListIdx>, Value) {
        let mut curr = idx;
        let mut new_head = None;
        let mut prev_new_id: Option<ListIdx> = None;

        loop {
            // Destructure by value to immediately release any read-borrow on self.data
            let ListNode { head, tail } = self.data[curr.as_usize()];

            match tail {
                Some(next_idx) => {
                    // `curr` is not the terminal node, so copy its head to the new spine.
                    // We temporarily initialize its tail as None.
                    let new_id = self.push_front(None, head);

                    if new_head.is_none() {
                        new_head = Some(new_id);
                    }

                    // Forward-link the previous node's tail to this freshly allocated node
                    if let Some(prev_id) = prev_new_id {
                        self.data[prev_id.as_usize()].tail = Some(new_id);
                    }

                    prev_new_id = Some(new_id);
                    curr = next_idx;
                }
                None => {
                    // `curr` has no tail, meaning it IS the terminal node!
                    // We stop here, leaving the new spine gracefully terminated with None.
                    return (new_head, head);
                }
            }
        }
    }

    /// Split the list at a given position.
    ///
    /// Rebuilds the prefix spine up to `index` in a single pass, and connects
    /// its end to the shared tail suffix.
    /// Auxiliary Space Complexity: O(1) (Zero temporary heap allocations).
    pub fn split_at(&mut self, idx: ListIdx, index: usize) -> (Option<ListIdx>, Option<ListIdx>) {
        // Base case: splitting at index 0 means the prefix is empty, and the entire list is the suffix.
        if index == 0 {
            return (None, Some(idx));
        }

        let mut curr = idx;
        let mut new_head = None;
        let mut prev_new_id: Option<ListIdx> = None;
        let mut i = 0;

        loop {
            let ListNode { head, tail } = self.data[curr.as_usize()];

            // Check if this node is the last node of the new prefix spine
            if i == index - 1 {
                let final_prefix_id = self.push_front(None, head);

                if let Some(prev_id) = prev_new_id {
                    self.data[prev_id.as_usize()].tail = Some(final_prefix_id);
                    return (new_head, tail);
                } else {
                    // Split at index 1 on a single-element traversal
                    return (Some(final_prefix_id), tail);
                }
            }

            // Otherwise, clone this prefix node and link forward
            let new_id = self.push_front(None, head);
            if new_head.is_none() {
                new_head = Some(new_id);
            }

            if let Some(prev_id) = prev_new_id {
                self.data[prev_id.as_usize()].tail = Some(new_id);
            }

            prev_new_id = Some(new_id);
            i += 1;

            // Out-of-bounds safety: if the original list ends before reaching `index`
            match tail {
                Some(next_idx) => curr = next_idx,
                None => return (new_head, None),
            }
        }
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

        let (first, tail) = arena.split_front(idx.unwrap());
        assert_eq!(first, Value::Num(1.0));
        assert_eq!(to_vec(&arena, tail), vec![Value::Num(2.0), Value::Num(3.0)]);
        // Original is untouched
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_back() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, last) = arena.split_back(idx.unwrap());
        assert_eq!(last, Value::Num(3.0));
        assert_eq!(to_vec(&arena, head), vec![Value::Num(1.0), Value::Num(2.0)]);
        assert_eq!(arena.len(idx), 3);
    }

    #[test]
    fn test_split_at() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        let (head, tail) = arena.split_at(idx.unwrap(), 1);
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
    fn test_chained_operations() {
        let mut arena = ListArena::new();
        let idx = arena.alloc(&[Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)]);

        // Push front, then split first
        let extended = arena.push_front(idx, Value::Num(0.0));
        let (first, tail) = arena.split_front(extended);
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
