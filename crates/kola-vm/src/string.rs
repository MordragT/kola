use std::{fmt, num::NonZeroU32, range::Range, str::Chars};

use kola_utils::{display::DisplayWith, serde::SerializeWith};

use crate::heap::Heap;

/// A `Copy`-friendly index into a `StringArena`.
///
/// Represents a contiguous byte range of the arena's backing storage.
/// All arena operations take this index rather than `&str`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringIdx(Range<NonZeroU32>);

impl StringIdx {
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
        StringIdx(Range {
            start: NonZeroU32::new((start + 1) as u32).expect("string arena overflow"),
            end: NonZeroU32::new((end + 1) as u32).expect("string arena overflow"),
        })
    }
}

impl DisplayWith<Heap> for StringIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        let s = heap.strings.get(*self);
        write!(f, "{}", s)
    }
}

impl SerializeWith<Heap> for StringIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s = heap.strings.get(*self);
        serializer.serialize_str(s)
    }
}

/// An append-only arena for strings.
///
/// All strings are stored as raw bytes in a single `Vec<u8>`.
/// `StringIdx` values point into this backing storage and are `Copy`.
///
/// Mutating operations (`pop_front`, `push_back`, etc.) use
/// `extend_from_within` to copy bytes directly within the arena.
/// No heap allocation occurs for any operation.
#[derive(Debug, Clone)]
pub struct StringArena {
    data: Vec<u8>,
}

impl StringArena {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    // ── read-only operations ──────────────────────────────────────

    /// Get the string slice for the given index.
    ///
    /// # Safety
    /// We only ever push valid UTF-8 into the arena, so this is safe.
    #[inline]
    pub fn get(&self, idx: StringIdx) -> &str {
        // SAFETY: we only push valid UTF-8 bytes via alloc(), copy(), etc.
        unsafe { std::str::from_utf8_unchecked(&self.data[idx.start()..idx.end()]) }
    }

    /// Get an owned `String` clone (heap allocation).
    #[inline]
    pub fn get_cloned(&self, idx: StringIdx) -> String {
        self.get(idx).to_owned()
    }

    /// Check if the string contains the given character.
    #[inline]
    pub fn contains(&self, idx: StringIdx, pat: char) -> bool {
        self.get(idx).contains(pat)
    }

    /// Iterate over the characters of the string.
    #[inline]
    pub fn chars(&self, idx: StringIdx) -> Chars<'_> {
        self.get(idx).chars()
    }

    /// Get the byte offset of the start of this string within the arena.
    #[inline]
    pub fn byte_offset(&self, idx: StringIdx) -> usize {
        idx.start()
    }

    // ── allocation ────────────────────────────────────────────────

    /// Allocate a new string in the arena, returning its index.
    #[inline]
    pub fn alloc(&mut self, s: &str) -> StringIdx {
        let start = self.data.len();
        self.data.extend_from_slice(s.as_bytes());
        let end = self.data.len();
        StringIdx::make(start, end)
    }

    /// Allocate an empty string in the arena, returning its index.
    #[inline]
    pub fn alloc_empty(&mut self) -> StringIdx {
        let start = self.data.len();
        let end = self.data.len();
        StringIdx::make(start, end)
    }

    /// Allocate a single character as a string in the arena, returning its index.
    #[inline]
    pub fn alloc_unit(&mut self, c: char) -> StringIdx {
        let start = self.data.len();
        let mut buf = [0u8; 4];
        let char_bytes = c.encode_utf8(&mut buf);
        self.data.extend_from_slice(char_bytes.as_bytes());
        let end = self.data.len();
        StringIdx::make(start, end)
    }

    /// Copy an existing string within the arena.
    /// Returns a new index pointing to an identical copy.
    /// Zero heap allocation — copies bytes directly within the vec.
    #[inline]
    pub fn copy(&mut self, idx: StringIdx) -> StringIdx {
        let start = self.data.len();
        self.data.extend_from_within(idx.start()..idx.end());
        let end = self.data.len();
        StringIdx::make(start, end)
    }

    // ── mutating operations (copy bytes within arena) ─────────────

    /// Pop the first character from the string.
    ///
    /// Returns the character and a new index for the remaining tail.
    /// The original string is untouched.
    pub fn pop_front(&mut self, idx: StringIdx) -> Option<(char, StringIdx)> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let first = self.get(idx).chars().next()?;
        let tail_start = start + first.len_utf8();

        let new_start = self.data.len();
        self.data.extend_from_within(tail_start..end);
        let new_end = self.data.len();

        Some((first, StringIdx::make(new_start, new_end)))
    }

    /// Pop the last character from the string.
    ///
    /// Returns the new index (without the last char) and the character.
    /// The original string is untouched.
    pub fn pop_back(&mut self, idx: StringIdx) -> Option<(StringIdx, char)> {
        let start = idx.start();
        let end = idx.end();
        if start == end {
            return None;
        }

        let last = self.get(idx).chars().next_back()?;
        let head_end = end - last.len_utf8();

        let new_start = self.data.len();
        self.data.extend_from_within(start..head_end);
        let new_end = self.data.len();

        Some((StringIdx::make(new_start, new_end), last))
    }

    /// Prepend a character to the string.
    ///
    /// Returns a new index for the resulting string.
    pub fn push_front(&mut self, idx: StringIdx, c: char) -> StringIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Write the char bytes first
        let mut buf = [0u8; 4];
        let char_bytes = c.encode_utf8(&mut buf);
        self.data.extend_from_slice(char_bytes.as_bytes());

        // Then copy the original string
        self.data.extend_from_within(start..end);

        let new_end = self.data.len();
        StringIdx::make(new_start, new_end)
    }

    /// Append a character to the string.
    ///
    /// Returns a new index for the resulting string.
    pub fn push_back(&mut self, idx: StringIdx, c: char) -> StringIdx {
        let start = idx.start();
        let end = idx.end();

        let new_start = self.data.len();

        // Copy the original string first
        self.data.extend_from_within(start..end);

        // Then write the char bytes
        let mut buf = [0u8; 4];
        let char_bytes = c.encode_utf8(&mut buf);
        self.data.extend_from_slice(char_bytes.as_bytes());

        let new_end = self.data.len();
        StringIdx::make(new_start, new_end)
    }

    /// Concatenate two strings.
    ///
    /// Returns a new index for the resulting string.
    pub fn concat(&mut self, left: StringIdx, right: StringIdx) -> StringIdx {
        let new_start = self.data.len();
        self.data.extend_from_within(left.start()..left.end());
        self.data.extend_from_within(right.start()..right.end());
        let new_end = self.data.len();
        StringIdx::make(new_start, new_end)
    }

    /// Reverse the string.
    ///
    /// Returns a new index for the reversed string.
    pub fn reverse(&mut self, idx: StringIdx) -> StringIdx {
        todo!()
    }

    /// Split off the first character from the string.
    #[inline]
    pub fn split_front(&mut self, idx: StringIdx) -> Option<(StringIdx, StringIdx)> {
        self.pop_front(idx).map(|(c, tail)| {
            let head = self.alloc_unit(c);
            (head, tail)
        })
    }

    /// Split off the last character from the string.
    #[inline]
    pub fn split_back(&mut self, idx: StringIdx) -> Option<(StringIdx, StringIdx)> {
        self.pop_back(idx).map(|(head, c)| {
            let tail = self.alloc_unit(c);
            (head, tail)
        })
    }

    /// Split the string at the given byte offset.
    ///
    /// Returns `(head_idx, tail_idx)`. The original string is untouched.
    pub fn split_at(&mut self, idx: StringIdx, byte_offset: usize) -> (StringIdx, StringIdx) {
        let start = idx.start();
        let end = idx.end();

        let head_start = self.data.len();
        self.data.extend_from_within(start..start + byte_offset);
        let head_end = self.data.len();

        let tail_start = self.data.len();
        self.data.extend_from_within(start + byte_offset..end);
        let tail_end = self.data.len();

        (
            StringIdx::make(head_start, head_end),
            StringIdx::make(tail_start, tail_end),
        )
    }

    /// Split the string at the first occurrence of the delimiter.
    ///
    /// Returns `Some((before, after))` if found, `None` otherwise.
    /// The original string is untouched.
    pub fn split_once(
        &mut self,
        idx: StringIdx,
        delimiter: char,
    ) -> Option<(StringIdx, StringIdx)> {
        let start = idx.start();
        let end = idx.end();

        // Find the delimiter position (borrow is temporary)
        let pos = {
            let s = self.get(idx);
            s.find(delimiter)?
        };
        let delim_len = delimiter.len_utf8();

        // Copy the part before the delimiter
        let before_start = self.data.len();
        self.data.extend_from_within(start..start + pos);
        let before_end = self.data.len();

        // Copy the part after the delimiter
        let after_start = self.data.len();
        self.data.extend_from_within(start + pos + delim_len..end);
        let after_end = self.data.len();

        Some((
            StringIdx::make(before_start, before_end),
            StringIdx::make(after_start, after_end),
        ))
    }

    /// Get the first character without mutating the arena.
    #[inline]
    pub fn first_char(&self, idx: StringIdx) -> Option<char> {
        self.get(idx).chars().next()
    }

    /// Get the last character without mutating the arena.
    #[inline]
    pub fn last_char(&self, idx: StringIdx) -> Option<char> {
        self.get(idx).chars().next_back()
    }

    /// Check if the string starts with the given prefix.
    #[inline]
    pub fn starts_with(&self, idx: StringIdx, prefix: &str) -> bool {
        self.get(idx).starts_with(prefix)
    }

    /// Check if the string ends with the given suffix.
    #[inline]
    pub fn ends_with(&self, idx: StringIdx, suffix: &str) -> bool {
        self.get(idx).ends_with(suffix)
    }

    /// Find a substring within the string.
    #[inline]
    pub fn find(&self, idx: StringIdx, pattern: &str) -> Option<usize> {
        self.get(idx).find(pattern)
    }
}

impl Default for StringArena {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for StringArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StringArena({} bytes)", self.data.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_and_get() {
        let mut arena = StringArena::new();
        let hello = arena.alloc("hello");
        let world = arena.alloc("world");

        assert_eq!(arena.get(hello), "hello");
        assert_eq!(arena.get(world), "world");
        assert_eq!(hello.len(), 5);
        assert_eq!(world.len(), 5);
    }

    #[test]
    fn test_empty_string() {
        let mut arena = StringArena::new();
        let empty = arena.alloc("");

        assert!(empty.is_empty());
        assert_eq!(empty.len(), 0);
        assert_eq!(arena.get(empty), "");
    }

    #[test]
    fn test_copy() {
        let mut arena = StringArena::new();
        let original = arena.alloc("hello");
        let copy = arena.copy(original);

        assert_eq!(arena.get(original), "hello");
        assert_eq!(arena.get(copy), "hello");
        assert_ne!(original, copy);
    }

    #[test]
    fn test_pop_front() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hello");

        let (c, tail) = arena.pop_front(idx).unwrap();
        assert_eq!(c, 'h');
        assert_eq!(arena.get(tail), "ello");

        let (c, tail) = arena.pop_front(tail).unwrap();
        assert_eq!(c, 'e');
        assert_eq!(arena.get(tail), "llo");
    }

    #[test]
    fn test_pop_back() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hello");

        let (head, c) = arena.pop_back(idx).unwrap();
        assert_eq!(c, 'o');
        assert_eq!(arena.get(head), "hell");

        let (head, c) = arena.pop_back(head).unwrap();
        assert_eq!(c, 'l');
        assert_eq!(arena.get(head), "hel");
    }

    #[test]
    fn test_push_front() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("ello");

        let new = arena.push_front(idx, 'h');
        assert_eq!(arena.get(new), "hello");
        assert_eq!(arena.get(idx), "ello");
    }

    #[test]
    fn test_push_back() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hell");

        let new = arena.push_back(idx, 'o');
        assert_eq!(arena.get(new), "hello");
        assert_eq!(arena.get(idx), "hell");
    }

    #[test]
    fn test_concat() {
        let mut arena = StringArena::new();
        let left = arena.alloc("hello");
        let right = arena.alloc(" world");

        let result = arena.concat(left, right);
        assert_eq!(arena.get(result), "hello world");
    }

    #[test]
    fn test_split_at() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hello");

        let (head, tail) = arena.split_at(idx, 3);
        assert_eq!(arena.get(head), "hel");
        assert_eq!(arena.get(tail), "lo");
    }

    #[test]
    fn test_split_once() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("key=value");

        let (key, value) = arena.split_once(idx, '=').unwrap();
        assert_eq!(arena.get(key), "key");
        assert_eq!(arena.get(value), "value");
    }

    #[test]
    fn test_contains() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hello");

        assert!(arena.contains(idx, 'e'));
        assert!(!arena.contains(idx, 'x'));
    }

    #[test]
    fn test_chars() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("abc");

        let chars: Vec<char> = arena.chars(idx).collect();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn test_unicode() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("héllo wörld");

        assert_eq!(arena.get(idx), "héllo wörld");

        let (c, tail) = arena.pop_front(idx).unwrap();
        assert_eq!(c, 'h');
        assert_eq!(arena.get(tail), "éllo wörld");
    }

    #[test]
    fn test_copy_preserves_original() {
        let mut arena = StringArena::new();
        let original = arena.alloc("hello");

        let copy = arena.copy(original);
        let modified = arena.push_back(copy, '!');

        assert_eq!(arena.get(original), "hello");
        assert_eq!(arena.get(copy), "hello");
        assert_eq!(arena.get(modified), "hello!");
    }

    #[test]
    fn test_chained_operations() {
        let mut arena = StringArena::new();
        let s = arena.alloc("hello world");

        let (head, tail) = arena.split_at(s, 5);
        assert_eq!(arena.get(head), "hello");
        assert_eq!(arena.get(tail), " world");

        let (_, word) = arena.pop_front(tail).unwrap();
        assert_eq!(arena.get(word), "world");

        let (w, c) = arena.pop_back(word).unwrap();
        assert_eq!(c, 'd');
        assert_eq!(arena.get(w), "worl");
    }

    #[test]
    fn test_pop_front_empty() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("");

        assert!(arena.pop_front(idx).is_none());
    }

    #[test]
    fn test_pop_back_empty() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("");

        assert!(arena.pop_back(idx).is_none());
    }

    #[test]
    fn test_pop_front_single_char() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("a");

        let (c, tail) = arena.pop_front(idx).unwrap();
        assert_eq!(c, 'a');
        assert!(tail.is_empty());
    }

    #[test]
    fn test_pop_back_single_char() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("a");

        let (head, c) = arena.pop_back(idx).unwrap();
        assert_eq!(c, 'a');
        assert!(head.is_empty());
    }

    #[test]
    fn test_split_once_not_found() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("hello");

        assert!(arena.split_once(idx, '=').is_none());
    }

    #[test]
    fn test_unicode_multibyte() {
        let mut arena = StringArena::new();
        let idx = arena.alloc("🦀🦀🦀");

        let (c, tail) = arena.pop_front(idx).unwrap();
        assert_eq!(c, '🦀');
        assert_eq!(arena.get(tail), "🦀🦀");

        let (head, c) = arena.pop_back(tail).unwrap();
        assert_eq!(c, '🦀');
        assert_eq!(arena.get(head), "🦀");
    }
}
