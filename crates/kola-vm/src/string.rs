use std::{
    fmt,
    num::{NonZeroU8, NonZeroU32},
    ops::ControlFlow,
};

use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
    serde::SerializeWith,
};

pub const INLINE_CAPACITY: usize = 14;

/// A compact, 16-byte unified string reference.
/// It fits entirely in CPU registers and requires zero heap management.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StringIdx {
    /// Reference to a compile-time string inside the global string interner.
    Static(StrKey),

    /// A slice of a static string, defined by a source key and byte offsets.
    StaticSlice {
        source: StrKey,
        start: u32,
        len: NonZeroU32,
    },

    /// Small String Optimization (SSO): Holds short text directly inline.
    /// No arena allocations, maximum cache locality. Fits up to 14 bytes.
    Inline {
        len: NonZeroU8,
        bytes: [u8; INLINE_CAPACITY],
    },

    /// A contiguous chunk of runtime-allocated text inside the dynamic buffer.
    Buffer { start: u32, len: NonZeroU32 },

    /// A structural string representation in the node arena.
    Node(NonZeroU32),
}

const _: () = assert!(StringIdx::BYTES <= 16);

impl StringIdx {
    pub const BYTES: usize = std::mem::size_of::<Self>();

    pub fn unit(c: char) -> Self {
        let mut bytes = [0u8; INLINE_CAPACITY];
        c.encode_utf8(&mut bytes);

        let len = NonZeroU8::new(c.len_utf8() as u8).expect("char is never empty");

        StringIdx::Inline { len, bytes }
    }

    pub fn tuple(left: char, right: char) -> Self {
        let mut bytes = [0u8; INLINE_CAPACITY];
        let left_len = left.encode_utf8(&mut bytes).len() as u8;
        let right_len = right.encode_utf8(&mut bytes[left_len as usize..]).len() as u8;

        let len = NonZeroU8::new(left_len + right_len).expect("tuple of two chars is never empty");

        StringIdx::Inline { len, bytes }
    }
}

impl DisplayWith<StringArena> for StringIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, arena: &StringArena) -> fmt::Result {
        let s = arena.as_str(self).unwrap(); // TODO fix
        write!(f, "{}", s)
    }
}

impl SerializeWith<StringArena> for StringIdx {
    fn serialize<S>(&self, serializer: S, arena: &StringArena) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s = arena.as_str(self).unwrap(); // TODO fix
        serializer.serialize_str(s)
    }
}

/// A node in the dedicated internal structural string arena.
#[derive(Debug, Clone, Copy)]
pub enum StringNode {
    Concat {
        left: StringIdx,
        right: StringIdx,
        len: NonZeroU32,
    },
    Flat {
        start: u32,
        len: NonZeroU32,
    },
}

impl StringNode {
    pub fn is_concat(&self) -> bool {
        matches!(self, StringNode::Concat { .. })
    }

    pub fn is_flat(&self) -> bool {
        matches!(self, StringNode::Flat { .. })
    }
}

/// A lightweight, deferred handle representing a string inside the arena.
/// Because it holds a shared reference, multiple handles can coexist and
/// yield their underlying string slices simultaneously.
pub struct StringHandle<'a, 'idx> {
    arena: &'a StringArena,
    idx: &'idx Option<StringIdx>,
}

impl<'a, 'idx> StringHandle<'a, 'idx> {
    /// Returns the contiguous string slice.
    /// Note that this returns a `&'a str` tied to the stable arena backing storage,
    /// meaning the string slice can outlive the handle itself!
    pub fn get(&self) -> &'idx str
    where
        'a: 'idx, // Ensure the arena outlives the index reference
    {
        let Some(idx) = self.idx else {
            return "";
        };

        // String handles can only be created from already flattened indices, so this should always succeed without mutating the arena.
        self.arena.as_str(idx).unwrap()
    }
}

/// An efficient string allocation engine tailored for functional runtimes.
#[derive(Debug, Clone)]
pub struct StringArena {
    /// Backing store for contiguous dynamic runtime slices.
    data: Vec<u8>,

    /// Isolated scratch arena for structural string concatenation nodes.
    nodes: Vec<StringNode>,

    /// The global string interner
    pub interner: StrInterner,
}

impl StringArena {
    #[inline]
    unsafe fn view(&self, start: u32, len: NonZeroU32) -> &str {
        let slice = &self.data[start as usize..(start + len.get()) as usize];

        unsafe { std::str::from_utf8_unchecked(slice) }
    }

    /// Internal recursive flattener matching straight against node descriptors
    fn flatten_rec_node(&mut self, node: StringNode) {
        match node {
            StringNode::Concat { left, right, .. } => {
                self.flatten_rec(left);
                self.flatten_rec(right);
            }
            StringNode::Flat { start, len } => {
                let start = start as usize;
                let end = start + len.get() as usize;
                self.data.extend_from_within(start..end);
            }
        }
    }

    /// Entry point for index-based recursion
    fn flatten_rec(&mut self, idx: StringIdx) {
        match idx {
            StringIdx::Static(key) => {
                let s = &self.interner[key];
                self.data.extend_from_slice(s.as_bytes());
            }
            StringIdx::StaticSlice { source, start, len } => {
                let s = &self.interner[source];
                let start = start as usize;
                let end = start + len.get() as usize;
                self.data.extend_from_slice(&s.as_bytes()[start..end]);
            }
            StringIdx::Inline { len, bytes } => {
                self.data.extend_from_slice(&bytes[..len.get() as usize]);
            }
            StringIdx::Buffer { start, len } => {
                let start = start as usize;
                self.data
                    .extend_from_within(start..start + len.get() as usize);
            }
            StringIdx::Node(node_idx) => {
                let node = self.nodes[node_idx.get() as usize];
                self.flatten_rec_node(node);
            }
        }
    }
}

impl StringArena {
    pub fn new(interner: StrInterner) -> Self {
        let mut nodes = Vec::new();
        // Reserve the first node
        nodes.push(StringNode::Flat {
            start: 0,
            len: NonZeroU32::new(1).unwrap(),
        });

        Self {
            data: Vec::new(),
            nodes,
            interner,
        }
    }

    pub fn with_capacity(bytes: usize, nodes: usize, interner: StrInterner) -> Self {
        let mut nodes = Vec::with_capacity(nodes);
        // Reserve the first node
        nodes.push(StringNode::Flat {
            start: 0,
            len: NonZeroU32::new(1).unwrap(),
        });

        Self {
            data: Vec::with_capacity(bytes),
            nodes,
            interner,
        }
    }

    // ── Allocation & Generation Primitives ───────────────────────────

    /// Allocates a string slice into the arena, automatically electing
    /// either an SSO inline value or a dynamic flat buffer slice based on length.
    /// Returns `None` if the input string is empty.
    pub fn alloc(&mut self, s: &str) -> Option<StringIdx> {
        let len = s.len();

        // An empty string is represented uniformly as `None`
        if len == 0 {
            return None;
        }

        // SSO Fast-Path (Guaranteed non-zero length here)
        if len <= INLINE_CAPACITY {
            let mut bytes = [0u8; INLINE_CAPACITY];
            bytes[..len].copy_from_slice(s.as_bytes());
            return Some(StringIdx::Inline {
                len: NonZeroU8::new(len as u8).expect("length is guaranteed to be non-zero"),
                bytes,
            });
        }

        // Dynamic Slice Path
        let start = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        Some(StringIdx::Buffer {
            start,
            len: NonZeroU32::new(len as u32).expect("length is guaranteed to be non-zero"),
        })
    }

    /// Allocates an empty string index.
    /// Uniformly returns `None` across the entire runtime pipeline.
    #[inline]
    pub fn alloc_empty(&self) -> Option<StringIdx> {
        None
    }

    /// Allocates a new string index representing a slice of an existing index.
    pub fn slice(&mut self, idx: StringIdx, start: u32, len: NonZeroU32) -> StringIdx {
        let owned_len = start + len.get();

        match idx {
            StringIdx::Static(source) => {
                debug_assert!(owned_len <= self.interner[source].len() as u32);
                StringIdx::StaticSlice { source, start, len }
            }

            StringIdx::StaticSlice {
                source,
                start: s_start,
                len: s_len,
            } => {
                debug_assert!(owned_len <= s_len.get());
                StringIdx::StaticSlice {
                    source,
                    start: s_start + start,
                    len,
                }
            }

            StringIdx::Inline {
                len: inline_len,
                bytes,
            } => {
                debug_assert!(owned_len <= inline_len.get() as u32,);

                let mut new_bytes = [0u8; INLINE_CAPACITY];
                let s = start as usize;
                let e = s + len.get() as usize;
                new_bytes[..len.get() as usize].copy_from_slice(&bytes[s..e]);

                StringIdx::Inline {
                    len: NonZeroU8::new(len.get() as u8).unwrap(),
                    bytes: new_bytes,
                }
            }

            StringIdx::Buffer {
                start: buf_start,
                len: buf_len,
            } => {
                debug_assert!(owned_len <= buf_len.get());
                StringIdx::Buffer {
                    start: buf_start + start,
                    len,
                }
            }

            StringIdx::Node(node_idx) => {
                match self.nodes[node_idx.get() as usize] {
                    StringNode::Flat {
                        start: f_start,
                        len: f_len,
                    } => {
                        debug_assert!(owned_len <= f_len.get());
                        StringIdx::Buffer {
                            start: f_start + start,
                            len,
                        }
                    }

                    StringNode::Concat {
                        left,
                        right,
                        len: total_len,
                    } => {
                        debug_assert!(owned_len <= total_len.get());
                        let len_left = self.len(left);

                        if owned_len <= len_left {
                            self.slice(left, start, len)
                        } else if start >= len_left {
                            self.slice(right, start - len_left, len)
                        } else {
                            // Straddles both branches: slice both and merge structurally
                            let left_len = len_left - start;
                            let right_len = len.get() - left_len;

                            let l_part =
                                self.slice(left, start, NonZeroU32::new(left_len).unwrap());
                            let r_part = self.slice(right, 0, NonZeroU32::new(right_len).unwrap());
                            self.concat(l_part, r_part)
                        }
                    }
                }
            }
        }
    }

    /// Concatenates two string indices in O(1) time without copying bytes.
    pub fn concat(&mut self, left: StringIdx, right: StringIdx) -> StringIdx {
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
            let combined_len = l_len.get() as usize + r_len.get() as usize;
            if combined_len <= INLINE_CAPACITY {
                let mut bytes = [0u8; INLINE_CAPACITY];
                let l_idx = l_len.get() as usize;

                bytes[..l_idx].copy_from_slice(&l_bytes[..l_idx]);
                bytes[l_idx..combined_len].copy_from_slice(&r_bytes[..r_len.get() as usize]);

                return StringIdx::Inline {
                    len: NonZeroU8::new(combined_len as u8)
                        .expect("combined length is guaranteed to be non-zero"),
                    bytes,
                };
            }
        }

        // Fallback to structural concatenation node
        // (l and r are guaranteed non-empty, pure StringIdx values)
        let next_idx = self.nodes.len() as u32;
        let len = NonZeroU32::new(self.len(left) + self.len(right)).unwrap();
        self.nodes.push(StringNode::Concat { left, right, len });

        StringIdx::Node(NonZeroU32::new(next_idx).expect("node index cannot be zero"))
    }

    // ── Introspection Primitives ─────────────────────────────────────

    /// Computes the total logical byte length of a string index.
    pub fn len(&self, idx: StringIdx) -> u32 {
        match idx {
            StringIdx::Static(key) => self.interner[key].len() as u32,
            StringIdx::StaticSlice { len, .. } => len.get(),
            StringIdx::Inline { len, .. } => len.get() as u32,
            StringIdx::Buffer { len, .. } => len.get(),
            StringIdx::Node(node_idx) => {
                // Read from our 1-based index arena safely
                match self.nodes[node_idx.get() as usize] {
                    StringNode::Concat { len, .. } => len.get(),
                    StringNode::Flat { len, .. } => len.get(),
                }
            }
        }
    }

    // ── Contiguous Resolution (Flattening) ───────────────────────────

    /// Borrows a contiguous view of the text if the variant is already flat.
    ///
    /// Lifetimes: The returned view lasts for the intersection of the arena's
    /// life `'a` and the index reference's life `'idx`.
    pub fn as_str<'a, 'idx>(&'a self, idx: &'idx StringIdx) -> Option<&'idx str>
    where
        'a: 'idx,
    {
        match *idx {
            StringIdx::Static(key) => Some(&self.interner[key]),
            StringIdx::StaticSlice { source, start, len } => {
                let s = &self.interner[source];
                let start = start as usize;
                let end = start + len.get() as usize;
                Some(&s[start..end])
            }
            StringIdx::Inline { len, ref bytes } => {
                let slice = &bytes[..len.get() as usize];
                // SAFETY: Internally managed strings are guaranteed valid UTF-8.
                Some(unsafe { std::str::from_utf8_unchecked(slice) })
            }
            StringIdx::Buffer { start, len } => Some(unsafe { self.view(start, len) }),
            StringIdx::Node(_) => None,
        }
    }

    /// Batch resolves an arbitrary N-sized array of optional indices.
    /// It sequentializes the mutable path compression phase, then packages
    /// them into independent, concurrent handles.
    pub fn resolve_many<'a, 'idx, const N: usize>(
        &'a mut self,
        indices: [&'idx Option<StringIdx>; N],
    ) -> [StringHandle<'a, 'idx>; N]
    where
        'a: 'idx, // Ensure the arena outlives the index reference
    {
        // Phase 1: Run path-compression sequentially on every item
        for idx in indices {
            let Some(StringIdx::Node(node_idx)) = idx else {
                continue; // Skip non-node indices, they are already flat
            };

            let n_idx = node_idx.get() as usize;
            let node = self.nodes[n_idx];

            if node.is_flat() {
                continue;
            }
            // Reuse the slow-path flattening optimization logic
            let start = self.data.len() as u32;
            self.flatten_rec_node(node);
            let len = NonZeroU32::new(self.data.len() as u32 - start).unwrap();

            // Path Compression: Mutate the arena storage cached state
            self.nodes[n_idx] = StringNode::Flat { start, len };
        }

        // Phase 2: Coerce our exclusive borrow down into a shared reference
        // so we can initialize the array of handles safely.
        let arena_ref: &'a StringArena = self;
        indices.map(|idx| StringHandle {
            arena: arena_ref,
            idx,
        })
    }

    /// Resolves any string index into a contiguous `&str` using shared index references.
    ///
    /// If the target is a structural `Node` (and not already flat), it flattens its
    /// children into the dynamic buffer and mutates the **internal arena node** /// into a `StringNode::Flat` variant. The source `StringIdx` remains untouched.
    pub fn get<'a, 'idx>(&'a mut self, idx: &'idx StringIdx) -> &'idx str
    where
        'a: 'idx, // Arena data must outlive the returned string view window
    {
        if let StringIdx::Node(node_idx) = idx {
            let n_idx = node_idx.get() as usize;
            let node = self.nodes[n_idx];

            if !node.is_flat() {
                let start = self.data.len() as u32;
                self.flatten_rec_node(node);
                let len = NonZeroU32::new(self.data.len() as u32 - start).unwrap();

                // Path Compression: Mutate the arena storage cached state
                self.nodes[n_idx] = StringNode::Flat { start, len };
            }
        }

        // Now that all nodes are guaranteed flat, we can borrow a contiguous view without mutating the arena further.
        self.as_str(idx).unwrap()
    }

    /// Convenience method for optional indices, returning an empty string for `None` and flattening `Some` values as needed.
    #[inline]
    pub fn get_or_default<'a, 'idx>(&'a mut self, idx: &'a Option<StringIdx>) -> &'idx str
    where
        'a: 'idx,
    {
        match idx {
            Some(idx) => self.get(idx),
            None => "",
        }
    }

    // ── Allocation-Free Streaming Traversal ──────────────────────────

    /// Streams through all internal string fragments of a `StringIdx` sequentially.
    pub fn walk_fragments<B>(
        &self,
        idx: StringIdx,
        f: &mut dyn FnMut(&str) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        self.walk_fragments_impl::<false, B>(idx, f)
    }

    /// Streams backwards through all internal string fragments of a `StringIdx` sequentially.
    pub fn walk_fragments_back<B>(
        &self,
        idx: StringIdx,
        f: &mut dyn FnMut(&str) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        self.walk_fragments_impl::<true, B>(idx, f)
    }

    /// Internal generic engine driven by a built-in compile-time boolean.
    pub fn walk_fragments_impl<const BACKWARD: bool, B>(
        &self,
        idx: StringIdx,
        f: &mut dyn FnMut(&str) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match idx {
            StringIdx::Static(key) => f(&self.interner[key]),
            StringIdx::StaticSlice { source, start, len } => {
                let s = &self.interner[source];
                let start = start as usize;
                let end = start + len.get() as usize;
                f(&s[start..end])
            }
            StringIdx::Inline { len, bytes } => {
                let slice = &bytes[..len.get() as usize];
                f(unsafe { std::str::from_utf8_unchecked(slice) })
            }
            StringIdx::Buffer { start, len } => f(unsafe { self.view(start, len) }),
            StringIdx::Node(node_idx) => match self.nodes[node_idx.get() as usize] {
                StringNode::Concat { left, right, .. } => {
                    let (left, right) = if BACKWARD {
                        (right, left)
                    } else {
                        (left, right)
                    };
                    self.walk_fragments_impl::<BACKWARD, B>(left, f)?;
                    self.walk_fragments_impl::<BACKWARD, B>(right, f)
                }
                StringNode::Flat { start, len } => f(unsafe { self.view(start, len) }),
            },
        }
    }

    // ── Additional utility methods ─────────────────────────────────

    /// Prepends a string slice to the given index, returning a new consolidated index.
    pub fn push_front(&mut self, idx: StringIdx, s: &str) -> StringIdx {
        if let Some(front) = self.alloc(s) {
            self.concat(front, idx)
        } else {
            idx
        }
    }

    /// Appends a string slice to the given index, returning a new consolidated index.
    pub fn push_back(&mut self, idx: StringIdx, s: &str) -> StringIdx {
        if let Some(back) = self.alloc(s) {
            self.concat(idx, back)
        } else {
            idx
        }
    }

    /// Reverses the string represented by `idx`, returning a new index for the result.
    ///
    /// For structural `Concat` nodes, this executes an O(1) tree inversion.
    pub fn reverse(&mut self, idx: StringIdx) -> StringIdx {
        if let StringIdx::Node(node_idx) = idx
            && let StringNode::Concat { left, right, .. } = self.nodes[node_idx.get() as usize]
        {
            // O(1) Structural inversion: swap and recursively reverse branches
            let rev_left = self.reverse(left);
            let rev_right = self.reverse(right);
            return self.concat(rev_right, rev_left);
        }

        let mut rev_s = String::with_capacity(self.len(idx) as usize);

        // Walk fragments backwards, reversing characters inside each fragment
        let _ = self.walk_fragments_back(idx, &mut |frag| {
            for ch in frag.chars().rev() {
                rev_s.push(ch);
            }
            ControlFlow::<()>::Continue(())
        });

        self.alloc(&rev_s).unwrap()
    }

    /// Gets the first character of the string if it exists, without flattening.
    /// Fast-paths down left branches in O(log N) structural steps.
    pub fn first(&self, idx: StringIdx) -> char {
        let ControlFlow::Break(first) = self.walk_fragments(idx, &mut |frag| {
            let first = frag.chars().next().unwrap();
            ControlFlow::Break(first)
        }) else {
            unreachable!()
        };

        first
    }

    /// Gets the last character of the string if it exists, without flattening.
    /// Fast-paths down right branches in O(log N) structural steps.
    pub fn last(&self, idx: StringIdx) -> char {
        let ControlFlow::Break(last) = self.walk_fragments(idx, &mut |frag| {
            let last = frag.chars().next_back().unwrap();
            ControlFlow::Break(last)
        }) else {
            unreachable!()
        };

        last
    }

    /// Removes and returns the first character along with the remaining string index, if it exists..
    pub fn pop_front(&mut self, idx: StringIdx) -> (char, Option<StringIdx>) {
        let ch = self.first(idx);
        let delta = ch.len_utf8() as u32;
        let total_len = self.len(idx);

        let rem_len = total_len - delta;
        let rest = if rem_len > 0 {
            Some(self.slice(idx, delta, NonZeroU32::new(rem_len).unwrap()))
        } else {
            None
        };

        (ch, rest)
    }

    /// Removes and returns the last character along with the remaining string index, if it exists.
    pub fn pop_back(&mut self, idx: StringIdx) -> (Option<StringIdx>, char) {
        let ch = self.last(idx);
        let delta = ch.len_utf8() as u32;
        let total_len = self.len(idx);

        let rem_len = total_len - delta;
        let rest = if rem_len > 0 {
            Some(self.slice(idx, 0, NonZeroU32::new(rem_len).unwrap()))
        } else {
            None
        };

        (rest, ch)
    }

    /// Splits the string at the front, returning a unit token for the character and the remainder.
    #[inline]
    pub fn split_front(&mut self, idx: StringIdx) -> (StringIdx, Option<StringIdx>) {
        let (head, tail) = self.pop_front(idx);
        (StringIdx::unit(head), tail)
    }

    /// Splits the string at the back, returning the remainder and a unit token for the last character.
    #[inline]
    pub fn split_back(&mut self, idx: StringIdx) -> (Option<StringIdx>, StringIdx) {
        let (tail, last) = self.pop_back(idx);
        (tail, StringIdx::unit(last))
    }

    /// Checks if the string contains a substring.
    ///
    /// Automatically flattens structural strings internally inside the arena
    /// to guarantee fast, contiguous SIMD execution—without mutating the caller's index.
    pub fn contains(&mut self, idx: StringIdx, needle: &str) -> bool {
        self.get(&idx).contains(needle)
    }

    /// Returns the character at a logical index.
    ///
    /// Transparently performs path-compression inside the arena registry on first
    /// access, ensuring subsequent reads bypass tree traversal entirely.
    pub fn at(&mut self, idx: StringIdx, char_index: usize) -> Option<char> {
        self.get(&idx).chars().nth(char_index)
    }

    pub fn eq(&mut self, a: StringIdx, b: StringIdx) -> bool {
        if a == b {
            return true;
        }

        if let (Some(slice_a), Some(slice_b)) = (self.as_str(&a), self.as_str(&b)) {
            return slice_a == slice_b;
        }

        // Slow Path: At least one is an un-flattened structural Node.
        let a_opt = Some(a);
        let b_opt = Some(b);
        let [a, b] = self.resolve_many([&a_opt, &b_opt]);
        a.get() == b.get()
    }
}

// ── Modularity Testing ───────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sso_inline_allocation() {
        let mut arena = StringArena::new(StrInterner::default());
        let short_str = "hello";

        // alloc returns Option if input can be empty; unwrap the guaranteed non-empty handle
        let idx = arena.alloc(short_str).expect("allocation failed");

        assert!(matches!(idx, StringIdx::Inline { .. }));
        assert_eq!(arena.get(&idx), "hello");
    }

    #[test]
    fn test_dynamic_slice_allocation() {
        let mut arena = StringArena::new(StrInterner::default());
        let long_str = "this_string_is_longer_than_eleven_bytes";

        let idx = arena.alloc(long_str).expect("allocation failed");

        assert!(matches!(idx, StringIdx::Buffer { .. }));
        assert_eq!(arena.get(&idx), long_str);
    }

    #[test]
    fn test_structural_concat_and_flattening() {
        let mut arena = StringArena::new(StrInterner::default());

        let left = arena.alloc("abc").expect("allocation failed"); // Inline
        let right = arena
            .alloc("defghijklmnopqrstuvw")
            .expect("allocation failed"); // Slice

        // Structural O(1) union now accepts and returns raw StringIdx values directly
        let structural_idx = arena.concat(left, right);

        assert!(matches!(structural_idx, StringIdx::Node(_)));
        assert_eq!(arena.get(&structural_idx), "abcdefghijklmnopqrstuvw");
    }

    #[test]
    fn test_sso_concatenation_merging() {
        let mut arena = StringArena::new(StrInterner::default());
        let left = arena.alloc("foo").expect("allocation failed");
        let right = arena.alloc("bar").expect("allocation failed");

        // "foo" (3) + "bar" (3) = 6 bytes. Bypasses the concat node array entirely.
        let merged = arena.concat(left, right);

        assert!(matches!(merged, StringIdx::Inline { .. }));
        assert_eq!(arena.get(&merged), "foobar");
    }

    #[test]
    fn test_fragment_streaming() {
        let mut interner = StrInterner::default();
        let t1 = interner.intern("static_prefix_");
        let t2 = interner.intern("_static_suffix");

        let mut arena = StringArena::new(interner);
        let s1 = StringIdx::Static(t1);
        let s2 = arena.alloc("runtime_").expect("allocation failed");
        let s3 = StringIdx::Static(t2);

        // Chain bare indices sequentially without structural option wrapping overhead
        let c1 = arena.concat(s1, s2);
        let root = arena.concat(c1, s3);

        assert!(matches!(root, StringIdx::Node(_)));

        let mut output = String::new();
        let _ = arena.walk_fragments(root, &mut |frag| {
            output.push_str(frag);
            ControlFlow::<()>::Continue(())
        });

        assert_eq!(output, "static_prefix_runtime__static_suffix");
    }
}
