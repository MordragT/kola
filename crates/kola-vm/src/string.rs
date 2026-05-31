use std::fmt;

use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
    serde::SerializeWith,
};

/// A compact, 16-byte unified string reference.
/// It fits entirely in CPU registers and requires zero heap management.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StringIdx {
    /// Reference to a compile-time string inside the global string interner.
    Static(StrKey),

    /// Small String Optimization (SSO): Holds short text directly inline.
    /// No arena allocations, maximum cache locality. Fits up to 11 bytes.
    Inline { len: u8, bytes: [u8; 11] },

    /// A contiguous chunk of runtime-allocated text inside the dynamic buffer.
    Slice { start: u32, len: u32 },

    /// A structural string concatenation point pointing into the dedicated concat arena.
    Concat(u32),
}

impl StringIdx {
    pub fn unit(c: char) -> Self {
        let mut bytes = [0u8; 11];
        c.encode_utf8(&mut bytes);
        StringIdx::Inline {
            len: c.len_utf8() as u8,
            bytes,
        }
    }

    pub fn tuple(left: char, right: char) -> Self {
        let mut bytes = [0u8; 11];
        let left_len = left.encode_utf8(&mut bytes).len() as u8;
        let right_len = right.encode_utf8(&mut bytes[left_len as usize..]).len() as u8;
        StringIdx::Inline {
            len: left_len + right_len,
            bytes,
        }
    }
}

impl DisplayWith<StringArena> for StringIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, arena: &StringArena) -> fmt::Result {
        let s = arena.try_get(self).unwrap();
        write!(f, "{}", s)
    }
}

impl SerializeWith<StringArena> for StringIdx {
    fn serialize<S>(&self, serializer: S, arena: &StringArena) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s = arena.try_get(self).unwrap();
        serializer.serialize_str(s)
    }
}

/// A node in the dedicated internal structural string arena.
#[derive(Debug, Clone, Copy)]
pub struct ConcatNode {
    pub left: StringIdx,
    pub right: StringIdx,
}

/// An efficient string allocation engine tailored for functional runtimes.
#[derive(Debug, Clone)]
pub struct StringArena {
    /// Backing store for contiguous dynamic runtime slices.
    data: Vec<u8>,

    /// Isolated scratch arena for structural string concatenation nodes.
    concat_nodes: Vec<ConcatNode>,

    /// The global string interner
    pub interner: StrInterner,
}

impl StringArena {
    pub fn new(interner: StrInterner) -> Self {
        Self {
            data: Vec::new(),
            concat_nodes: Vec::new(),
            interner,
        }
    }

    pub fn with_capacity(bytes: usize, nodes: usize, interner: StrInterner) -> Self {
        Self {
            data: Vec::with_capacity(bytes),
            concat_nodes: Vec::with_capacity(nodes),
            interner,
        }
    }

    // ── Allocation & Generation Primitives ───────────────────────────

    /// Allocates a string slice into the arena, automatically electing
    /// either an SSO inline value or a dynamic flat slice based on length.
    pub fn alloc(&mut self, s: &str) -> StringIdx {
        let len = s.len();

        // SSO Fast-Path
        if len <= 11 {
            let mut bytes = [0u8; 11];
            bytes[..len].copy_from_slice(s.as_bytes());
            return StringIdx::Inline {
                len: len as u8,
                bytes,
            };
        }

        // Dynamic Slice Path
        let start = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        StringIdx::Slice {
            start,
            len: len as u32,
        }
    }

    /// Allocates an empty string index.
    #[inline]
    pub fn alloc_empty(&self) -> StringIdx {
        StringIdx::Inline {
            len: 0,
            bytes: [0u8; 11],
        }
    }

    /// Concatenates two string indices in O(1) time without copying bytes.
    pub fn concat(&mut self, left: StringIdx, right: StringIdx) -> StringIdx {
        // Optimize out empty identities
        if self.is_empty(left) {
            return right;
        }
        if self.is_empty(right) {
            return left;
        }

        // Attempt to merge if both are inline strings and fit within SSO limits
        if let (
            StringIdx::Inline {
                len: l_len,
                bytes: l_bytes,
            },
            StringIdx::Inline {
                len: r_len,
                bytes: r_bytes,
            },
        ) = (left, right)
        {
            let combined_len = l_len as usize + r_len as usize;
            if combined_len <= 11 {
                let mut bytes = [0u8; 11];
                bytes[..l_len as usize].copy_from_slice(&l_bytes[..l_len as usize]);
                bytes[l_len as usize..combined_len].copy_from_slice(&r_bytes[..r_len as usize]);
                return StringIdx::Inline {
                    len: combined_len as u8,
                    bytes,
                };
            }
        }

        // Fallback to structural concatenation node
        let idx = self.concat_nodes.len() as u32;
        self.concat_nodes.push(ConcatNode { left, right });
        StringIdx::Concat(idx)
    }

    // ── Introspection Primitives ─────────────────────────────────────

    /// Computes the total logical byte length of a string index.
    pub fn len(&self, idx: StringIdx) -> usize {
        match idx {
            StringIdx::Static(key) => self.interner[key].len(),
            StringIdx::Inline { len, .. } => len as usize,
            StringIdx::Slice { len, .. } => len as usize,
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                self.len(node.left) + self.len(node.right)
            }
        }
    }

    /// Quick check to see if a string index points to an empty sequence.
    #[inline]
    pub fn is_empty(&self, idx: StringIdx) -> bool {
        match idx {
            StringIdx::Static(_) => false, // Intered strings are typically non-empty
            StringIdx::Inline { len, .. } => len == 0,
            StringIdx::Slice { len, .. } => len == 0,
            StringIdx::Concat(_) => false, // Concat prunes empty structures on creation
        }
    }

    // ── Contiguous Resolution (Flattening) ───────────────────────────

    /// Resolves a contiguous string slice if the variant allows it.
    ///
    /// We pass `idx` by reference so that inline stack bytes can be borrowed
    /// safely for the duration of the reference lifetime `'b`.
    pub fn try_get<'a, 'b>(&'a self, idx: &'b StringIdx) -> Option<&'b str>
    where
        'a: 'b, // The arena data must outlive the returned view window
    {
        match idx {
            StringIdx::Static(key) => Some(self.interner[*key].as_str()),
            StringIdx::Inline { len, bytes } => {
                let slice = &bytes[..*len as usize];
                // SAFETY: We only insert valid UTF-8 sequences into our inline variant
                Some(unsafe { std::str::from_utf8_unchecked(slice) })
            }
            StringIdx::Slice { start, len } => {
                let slice = &self.data[*start as usize..(*start + *len) as usize];
                Some(unsafe { std::str::from_utf8_unchecked(slice) })
            }
            StringIdx::Concat(_) => None,
        }
    }

    /// Traverses a structural string, flattens its fragments, and writes them
    /// contiguously into the dynamic buffer.
    pub fn flatten(&mut self, idx: StringIdx) -> StringIdx {
        // Passing &idx means try_get only borrows it locally within this block.
        // self is downgraded to a shared borrow temporarily and released immediately after.
        if let Some(_) = self.try_get(&idx) {
            return idx;
        }

        let start = self.data.len() as u32;
        self.flatten_rec(idx);
        let end = self.data.len() as u32;

        StringIdx::Slice {
            start,
            len: end - start,
        }
    }

    /// Direct matching eliminates the need for complex outer lifetimes during recursion.
    fn flatten_rec(&mut self, idx: StringIdx) {
        match idx {
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                // No long-lived lifetime constraints on &mut self, recursion compiles cleanly!
                self.flatten_rec(node.left);
                self.flatten_rec(node.right);
            }
            StringIdx::Static(key) => {
                let s = self.interner[key].as_str();
                self.data.extend_from_slice(s.as_bytes());
            }
            StringIdx::Inline { len, bytes } => {
                let slice = &bytes[..len as usize];
                self.data.extend_from_slice(slice);
            }
            StringIdx::Slice { start, len } => {
                // Critical optimization: use extend_from_within to bypass
                // simultaneous mutable/immutable borrow rules on self.data
                let start = start as usize;
                let len = len as usize;
                self.data.extend_from_within(start..start + len);
            }
        }
    }

    // ── Allocation-Free Streaming Traversal ──────────────────────────

    /// Streams through all internal string fragments of a `StringIdx` sequentially.
    ///
    /// By matching directly instead of calling `try_get`, the transient stack slices
    /// of Inline strings are safely exposed to the closure `f` for the brief duration
    /// of the closure call frame, completely satisfying Higher-Rank Trait Bounds.
    pub fn for_each_fragment<'str>(&self, idx: StringIdx, f: &mut impl FnMut(&str)) {
        match idx {
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                self.for_each_fragment(node.left, f);
                self.for_each_fragment(node.right, f);
            }
            StringIdx::Static(key) => {
                f(self.interner[key].as_str());
            }
            StringIdx::Inline { len, bytes } => {
                let slice = &bytes[..len as usize];
                let s = unsafe { std::str::from_utf8_unchecked(slice) };
                f(s);
            }
            StringIdx::Slice { start, len } => {
                let slice = &self.data[start as usize..(start + len) as usize];
                let s = unsafe { std::str::from_utf8_unchecked(slice) };
                f(s);
            }
        }
    }

    // ── Additional utility methods ─────────────────────────────────

    /// Prepend a string slice to the given index, returning a new index for the result.
    pub fn push_front(&mut self, idx: StringIdx, s: &str) -> StringIdx {
        let front = self.alloc(s);
        self.concat(front, idx)
    }

    /// Append a string slice to the given index, returning a new index for the result.
    pub fn push_back(&mut self, idx: StringIdx, s: &str) -> StringIdx {
        let back = self.alloc(s);
        self.concat(idx, back)
    }

    /// Reverse the string represented by `idx`, returning a new index for the result.
    pub fn reverse<'str>(&mut self, idx: StringIdx) -> StringIdx {
        match idx {
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                // Structural inversion: swap left and right branches recursively
                let rev_left = self.reverse(node.left);
                let rev_right = self.reverse(node.right);
                self.concat(rev_right, rev_left)
            }
            // For flat leaves, reverse the characters and write them as a new flat element
            other => {
                let s = self.try_get(&other).unwrap();
                let rev_s: String = s.chars().rev().collect();
                self.alloc(&rev_s)
            }
        }
    }

    /// Get the first character of the string, if it exists, without flattening.
    pub fn first(&self, idx: StringIdx) -> Option<char> {
        match idx {
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                self.first(node.left)
            }
            other => self.try_get(&other)?.chars().next(),
        }
    }

    /// Get the last character of the string, if it exists, without flattening.
    pub fn last(&self, idx: StringIdx) -> Option<char> {
        match idx {
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                self.last(node.right)
            }
            other => self.try_get(&other)?.chars().next_back(),
        }
    }

    /// Removes and returns the first character of the string along with the remaining string index, if it exists, without flattening.
    pub fn pop_front(&mut self, idx: StringIdx) -> Option<(char, StringIdx)> {
        match idx {
            StringIdx::Slice { start, len } => {
                let s = unsafe {
                    std::str::from_utf8_unchecked(
                        &self.data[start as usize..(start + len) as usize],
                    )
                };
                let ch = s.chars().next()?;
                let delta = ch.len_utf8() as u32;
                Some((
                    ch,
                    StringIdx::Slice {
                        start: start + delta,
                        len: len - delta,
                    },
                ))
            }
            StringIdx::Inline { len, mut bytes } => {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[..len as usize]) };
                let ch = s.chars().next()?;
                let delta = ch.len_utf8();
                // Zero-allocation stack shift
                bytes.copy_within(delta..len as usize, 0);
                Some((
                    ch,
                    StringIdx::Inline {
                        len: len - delta as u8,
                        bytes,
                    },
                ))
            }
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                let (ch, rest_left) = self.pop_front(node.left)?;
                Some((ch, self.concat(rest_left, node.right)))
            }
            StringIdx::Static(key) => {
                let s = self.interner[key].to_owned();
                let ch = s.chars().next()?;
                Some((ch, self.alloc(&s[ch.len_utf8()..])))
            }
        }
    }

    /// Removes and returns the last character of the string along with the remaining string index, if it exists, without flattening.
    pub fn pop_back(&mut self, idx: StringIdx) -> Option<(StringIdx, char)> {
        match idx {
            StringIdx::Slice { start, len } => {
                let s = unsafe {
                    std::str::from_utf8_unchecked(
                        &self.data[start as usize..(start + len) as usize],
                    )
                };
                let ch = s.chars().next_back()?;
                let delta = ch.len_utf8() as u32;
                Some((
                    StringIdx::Slice {
                        start,
                        len: len - delta,
                    },
                    ch,
                ))
            }
            StringIdx::Inline { len, bytes } => {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[..len as usize]) };
                let ch = s.chars().next_back()?;
                let delta = ch.len_utf8() as u8;
                Some((
                    StringIdx::Inline {
                        len: len - delta,
                        bytes,
                    },
                    ch,
                ))
            }
            StringIdx::Concat(node_idx) => {
                let node = self.concat_nodes[node_idx as usize];
                let (rest_right, ch) = self.pop_back(node.right)?;
                Some((self.concat(node.left, rest_right), ch))
            }
            StringIdx::Static(key) => {
                let s = self.interner[key].to_owned();
                let ch = s.chars().next_back()?;
                Some((self.alloc(&s[..s.len() - ch.len_utf8()]), ch))
            }
        }
    }

    /// Splits the string at the front, returning the first character and the remaining string index, if it exists, without flattening.
    pub fn split_front(&mut self, idx: StringIdx) -> Option<(StringIdx, StringIdx)> {
        let (head, tail) = self.pop_front(idx)?;
        Some((StringIdx::unit(head), tail))
    }

    /// Splits the string at the back, returning the last character and the remaining string index, if it exists, without flattening.
    pub fn split_back(&mut self, idx: StringIdx) -> Option<(StringIdx, StringIdx)> {
        let (head, last) = self.pop_back(idx)?;
        Some((head, StringIdx::unit(last)))
    }

    /// Checks if the string contains a substring.
    /// Automatically flattens structural strings in-place to guarantee fast SIMD execution.
    pub fn contains_mut(&mut self, idx: &mut StringIdx, needle: &str) -> bool {
        if let StringIdx::Concat(_) = idx {
            // Flatten the string and update the caller's variable register in-place!
            *idx = self.flatten(*idx);
        }

        self.try_get(idx)
            .map(|s| s.contains(needle))
            .unwrap_or(false)
    }

    /// Returns the character at a logical index, modifying the underlying
    /// registry to a flat layout if it was previously structural.
    pub fn at_mut<'str>(&mut self, idx: &mut StringIdx, char_index: usize) -> Option<char> {
        // If it's a complex tree, flatten it into a contiguous slice immediately
        if let StringIdx::Concat(_) = idx {
            *idx = self.flatten(*idx);
        }

        // Now guaranteed to be a fast, contiguous leaf node read
        self.try_get(idx)?.chars().nth(char_index)
    }
}

// ── Modularity Testing ───────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sso_inline_allocation() {
        let mut arena = StringArena::new(StrInterner::new());
        let short_str = "hello";

        let idx = arena.alloc(short_str);

        assert!(matches!(idx, StringIdx::Inline { len: 5, .. }));
        assert_eq!(arena.try_get(&idx), Some("hello"));
    }

    #[test]
    fn test_dynamic_slice_allocation() {
        let mut arena = StringArena::new(StrInterner::new());
        let long_str = "this_string_is_longer_than_eleven_bytes";

        let idx = arena.alloc(long_str);

        assert!(matches!(idx, StringIdx::Slice { .. }));
        assert_eq!(arena.try_get(&idx), Some(long_str));
    }

    #[test]
    fn test_structural_concat_and_flattening() {
        let mut arena = StringArena::new(StrInterner::new());

        let left = arena.alloc("abc"); // Inline
        let right = arena.alloc("defghijklmnopqrstuvw"); // Slice

        // Structural O(1) union
        let structural_idx = arena.concat(left, right);
        assert!(matches!(structural_idx, StringIdx::Concat(_)));

        // Immediate get should fail because it's non-contiguous
        assert_eq!(arena.try_get(&structural_idx), None);

        // Flatten text contiguously inside the dynamic buffer
        let flat_idx = arena.flatten(structural_idx);
        assert!(matches!(flat_idx, StringIdx::Slice { .. }));
        assert_eq!(arena.try_get(&flat_idx), Some("abcdefghijklmnopqrstuvw"));
    }

    #[test]
    fn test_sso_concatenation_merging() {
        let mut arena = StringArena::new(StrInterner::new());
        let left = arena.alloc("foo");
        let right = arena.alloc("bar");

        // "foo" (3) + "bar" (3) = 6 bytes. Should bypass the concat arena entirely and merge inline.
        let merged = arena.concat(left, right);
        assert!(matches!(merged, StringIdx::Inline { len: 6, .. }));
        assert_eq!(arena.try_get(&merged), Some("foobar"));
    }

    #[test]
    fn test_fragment_streaming() {
        let mut interner = StrInterner::new();
        let t1 = interner.intern("static_prefix_");
        let t2 = interner.intern("_static_suffix");

        let mut arena = StringArena::new(interner);
        let s1 = StringIdx::Static(t1);
        let s2 = arena.alloc("runtime_");
        let s3 = StringIdx::Static(t2);

        let c1 = arena.concat(s1, s2);
        let root = arena.concat(c1, s3);

        let mut output = String::new();
        arena.for_each_fragment(root, &mut |frag| {
            output.push_str(frag);
        });

        assert_eq!(output, "static_prefix_runtime__static_suffix");
    }
}
