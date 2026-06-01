use std::{
    fmt,
    num::{NonZeroU8, NonZeroU32},
};

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
    Inline { len: NonZeroU8, bytes: [u8; 11] },

    /// A contiguous chunk of runtime-allocated text inside the dynamic buffer.
    Buffer { start: u32, len: NonZeroU32 },

    /// A structural string representation in the node arena.
    Node(NonZeroU32),
}

impl StringIdx {
    pub fn unit(c: char) -> Self {
        let mut bytes = [0u8; 11];
        c.encode_utf8(&mut bytes);

        let len = NonZeroU8::new(c.len_utf8() as u8).expect("char is never empty");

        StringIdx::Inline { len, bytes }
    }

    pub fn tuple(left: char, right: char) -> Self {
        let mut bytes = [0u8; 11];
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
    },
    Slice {
        source: StringIdx,
        start: usize,
        len: usize,
    },
    Flat {
        start: u32,
        len: u32,
    },
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
    fn alloc_node(&mut self, node: StringNode) -> StringIdx {
        let next_idx = self.nodes.len() as u32;
        self.nodes.push(node);
        StringIdx::Node(NonZeroU32::new(next_idx).expect("node index cannot be zero"))
    }

    #[inline]
    unsafe fn unchecked_slice(&self, start: u32, len: u32) -> &str {
        let slice = &self.data[start as usize..(start + len) as usize];

        unsafe { std::str::from_utf8_unchecked(slice) }
    }

    /// Internal recursive flattener matching straight against node descriptors
    fn flatten_rec_node(&mut self, node: StringNode) {
        match node {
            StringNode::Concat { left, right } => {
                self.flatten_rec(left);
                self.flatten_rec(right);
            }
            StringNode::Slice { source, start, len } => {
                let scratch_start = self.data.len();
                self.flatten_rec(source);

                let slice_start = scratch_start + start;
                self.data
                    .copy_within(slice_start..slice_start + len, scratch_start);
                self.data.truncate(scratch_start + len);
            }
            StringNode::Flat { start, len } => {
                let start = start as usize;
                self.data.extend_from_within(start..start + len as usize);
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
        nodes.push(StringNode::Flat { start: 0, len: 0 });

        Self {
            data: Vec::new(),
            nodes,
            interner,
        }
    }

    pub fn with_capacity(bytes: usize, nodes: usize, interner: StrInterner) -> Self {
        let mut nodes = Vec::with_capacity(nodes);
        // Reserve the first node
        nodes.push(StringNode::Flat { start: 0, len: 0 });

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
        if len <= 11 {
            let mut bytes = [0u8; 11];
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

    /// Concatenates two string indices in O(1) time without copying bytes.
    pub fn concat(
        &mut self,
        left: Option<StringIdx>,
        right: Option<StringIdx>,
    ) -> Option<StringIdx> {
        // Optimize out empty identities
        let Some(l) = left else {
            return right;
        };

        let Some(r) = right else {
            return left;
        };

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
        ) = (l, r)
        {
            let combined_len = l_len.get() as usize + r_len.get() as usize;
            if combined_len <= 11 {
                let mut bytes = [0u8; 11];
                let l_idx = l_len.get() as usize;

                bytes[..l_idx].copy_from_slice(&l_bytes[..l_idx]);
                bytes[l_idx..combined_len].copy_from_slice(&r_bytes[..r_len.get() as usize]);

                return Some(StringIdx::Inline {
                    len: NonZeroU8::new(combined_len as u8)
                        .expect("combined length is guaranteed to be non-zero"),
                    bytes,
                });
            }
        }

        // Fallback to structural concatenation node
        // (l and r are guaranteed non-empty, pure StringIdx values)
        let next_idx = self.nodes.len() as u32;
        self.nodes.push(StringNode::Concat { left: l, right: r });

        Some(StringIdx::Node(
            NonZeroU32::new(next_idx).expect("node index cannot be zero"),
        ))
    }

    // ── Introspection Primitives ─────────────────────────────────────

    /// Computes the total logical byte length of a string index.
    pub fn len(&self, idx: Option<StringIdx>) -> usize {
        let idx = match idx {
            Some(i) => i,
            None => return 0, // Empty string length is 0
        };

        match idx {
            StringIdx::Static(key) => self.interner[key].len(),
            StringIdx::Inline { len, .. } => len.get() as usize,
            StringIdx::Buffer { len, .. } => len.get() as usize,
            StringIdx::Node(node_idx) => {
                // Read from our 1-based index arena safely
                match self.nodes[node_idx.get() as usize] {
                    StringNode::Concat { left, right } => {
                        self.len(Some(left)) + self.len(Some(right))
                    }
                    StringNode::Slice { len, .. } => len,
                    StringNode::Flat { len, .. } => len as usize,
                }
            }
        }
    }

    /// Quick check to see if a string index points to an empty sequence.
    /// Since we use `None` for empty strings, this collapses into a trivial O(1) instruction.
    #[inline]
    pub fn is_empty(&self, idx: Option<StringIdx>) -> bool {
        idx.is_none()
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
            StringIdx::Inline { len, ref bytes } => {
                let slice = &bytes[..len.get() as usize];
                // SAFETY: Internally managed strings are guaranteed valid UTF-8.
                Some(unsafe { std::str::from_utf8_unchecked(slice) })
            }
            StringIdx::Buffer { start, len } => {
                Some(unsafe { self.unchecked_slice(start, len.get()) })
            }
            StringIdx::Node(_) => None,
        }
    }

    /// Resolves a single optional string index into a `StringHandle`.
    ///
    /// If the target is a structural node, this performs path compression
    /// by flattening it into the contiguous buffer and caching the result.
    pub fn resolve<'a, 'idx>(&'a mut self, idx: &'a Option<StringIdx>) -> StringHandle<'a, 'idx>
    where
        'a: 'idx, // Ensure the arena outlives the index reference
    {
        if let Some(StringIdx::Node(node_idx)) = idx {
            let n_idx = node_idx.get() as usize;

            // 1. Fast Path: Already flattened by a previous lookup
            if let StringNode::Flat { .. } = self.nodes[n_idx] {
                // Do nothing, ready to read
            } else {
                // 2. Slow Path: First time resolving this node. Flatten it.
                let start = self.data.len() as u32;

                // Copy the node descriptor out to unlock the mutable borrow on `self.nodes`
                let node = self.nodes[n_idx];
                self.flatten_rec_node(node);

                let len = self.data.len() as u32 - start;

                // 3. Path Compression: Mutate the arena storage cached state
                self.nodes[n_idx] = StringNode::Flat { start, len };
            }
        }

        // Downgrade our exclusive `&'a mut self` borrow into a shared `&'a StringArena` handle
        StringHandle { arena: self, idx }
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
        for idx in &indices {
            if let Some(StringIdx::Node(node_idx)) = *idx {
                let n_idx = node_idx.get() as usize;
                if let StringNode::Flat { .. } = self.nodes[n_idx] {
                    continue;
                }
                // Reuse the slow-path flattening optimization logic
                let start = self.data.len() as u32;
                let node = self.nodes[n_idx];
                self.flatten_rec_node(node);
                let len = self.data.len() as u32 - start;
                self.nodes[n_idx] = StringNode::Flat { start, len };
            }
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
    pub fn get<'a, 'idx>(&'a mut self, idx: &'idx Option<StringIdx>) -> &'idx str
    where
        'a: 'idx, // Arena data must outlive the returned string view window
    {
        let handle = self.resolve(idx);
        handle.get()
    }

    // ── Allocation-Free Streaming Traversal ──────────────────────────

    /// Streams through all internal string fragments of a `StringIdx` sequentially.
    pub fn for_each_fragment(&self, idx: StringIdx, f: &mut dyn FnMut(&str)) {
        match idx {
            StringIdx::Node(node_idx) => {
                match self.nodes[node_idx.get() as usize] {
                    StringNode::Concat { left, right } => {
                        self.for_each_fragment(left, f);
                        self.for_each_fragment(right, f);
                    }
                    StringNode::Slice { source, start, len } => {
                        // Tracks global byte intervals across disparate streaming fragments
                        let mut skipped = 0;
                        let mut written = 0;

                        self.for_each_fragment(source, &mut |fragment| {
                            if written >= len {
                                return;
                            }
                            let frag_len = fragment.len();

                            if skipped + frag_len <= start {
                                skipped += frag_len;
                                return;
                            }

                            let frag_start = if skipped < start { start - skipped } else { 0 };
                            let frag_end = std::cmp::min(frag_len, frag_start + (len - written));

                            f(&fragment[frag_start..frag_end]);
                            written += frag_end - frag_start;
                            skipped += frag_len;
                        });
                    }
                    StringNode::Flat { start, len } => {
                        f(unsafe { self.unchecked_slice(start, len) });
                    }
                }
            }
            StringIdx::Static(key) => {
                f(&self.interner[key]);
            }
            StringIdx::Inline { len, bytes } => {
                let slice = &bytes[..len.get() as usize];
                f(unsafe { std::str::from_utf8_unchecked(slice) });
            }
            StringIdx::Buffer { start, len } => {
                f(unsafe { self.unchecked_slice(start, len.get()) });
            }
        }
    }

    // ── Additional utility methods ─────────────────────────────────

    /// Prepends a string slice to the given index, returning a new consolidated index.
    pub fn push_front(&mut self, idx: Option<StringIdx>, s: &str) -> Option<StringIdx> {
        let front = self.alloc(s);
        self.concat(front, idx)
    }

    /// Appends a string slice to the given index, returning a new consolidated index.
    pub fn push_back(&mut self, idx: Option<StringIdx>, s: &str) -> Option<StringIdx> {
        let back = self.alloc(s);
        self.concat(idx, back)
    }

    /// Reverses the string represented by `idx`, returning a new index for the result.
    ///
    /// For structural `Concat` nodes, this executes an O(1) tree inversion.
    pub fn reverse(&mut self, idx: Option<StringIdx>) -> Option<StringIdx> {
        let i = idx?;

        if let StringIdx::Node(node_idx) = i
            && let StringNode::Concat { left, right } = self.nodes[node_idx.get() as usize]
        {
            // Structural inversion: swap left and right branches recursively
            let rev_left = self.reverse(Some(left));
            let rev_right = self.reverse(Some(right));
            return self.concat(rev_right, rev_left);
        }

        // Leaf, Slice, or Flat node optimization fallback:
        // Stream the fragments, reverse characters, and allocate a fresh flat sequence.
        let mut s = String::new();
        self.for_each_fragment(i, &mut |frag| s.push_str(frag));

        let rev_s: String = s.chars().rev().collect();
        self.alloc(&rev_s)
    }

    /// Gets the first character of the string if it exists, without flattening.
    /// Fast-paths down left branches in O(log N) structural steps.
    pub fn first(&self, idx: Option<StringIdx>) -> Option<char> {
        let i = idx?;

        if let StringIdx::Node(node_idx) = i
            && let StringNode::Concat { left, .. } = self.nodes[node_idx.get() as usize]
        {
            return self.first(Some(left));
        }

        // Slices and Flat elements stream safely via fragment limits
        let mut res = None;
        self.for_each_fragment(i, &mut |frag| {
            if res.is_none() {
                res = frag.chars().next();
            }
        });
        res
    }

    /// Gets the last character of the string if it exists, without flattening.
    /// Fast-paths down right branches in O(log N) structural steps.
    pub fn last(&self, idx: Option<StringIdx>) -> Option<char> {
        let i = idx?;

        if let StringIdx::Node(node_idx) = i
            && let StringNode::Concat { right, .. } = self.nodes[node_idx.get() as usize]
        {
            return self.last(Some(right));
        }

        // Slices and Flat elements stream safely via fragment limits
        let mut res = None;
        self.for_each_fragment(i, &mut |frag| {
            if let Some(c) = frag.chars().next_back() {
                res = Some(c);
            }
        });
        res
    }

    /// Removes and returns the first character along with the remaining string index, if it exists.
    ///
    /// This runs completely allocation-free by constructing structural `Slice` nodes.
    pub fn pop_front(&mut self, idx: Option<StringIdx>) -> Option<(char, Option<StringIdx>)> {
        match idx? {
            StringIdx::Static(key) => {
                let s = &self.interner[key];
                let ch = s.chars().next()?;
                let delta = ch.len_utf8();
                let rem_len = s.len() - delta;

                let rest = (rem_len > 0).then(|| {
                    self.alloc_node(StringNode::Slice {
                        source: StringIdx::Static(key),
                        start: delta,
                        len: rem_len,
                    })
                });
                Some((ch, rest))
            }

            StringIdx::Inline { len, mut bytes } => {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[..len.get() as usize]) };
                let ch = s.chars().next()?;
                let delta = ch.len_utf8();
                let rem_len = len.get() as usize - delta;

                let rest = (rem_len > 0).then(|| {
                    bytes.copy_within(delta..len.get() as usize, 0);
                    StringIdx::Inline {
                        len: unsafe { NonZeroU8::new_unchecked(rem_len as u8) },
                        bytes,
                    }
                });
                Some((ch, rest))
            }

            StringIdx::Buffer { start, len } => {
                let s = unsafe {
                    std::str::from_utf8_unchecked(
                        &self.data[start as usize..(start + len.get()) as usize],
                    )
                };
                let ch = s.chars().next()?;
                let delta = ch.len_utf8() as u32;
                let rem_len = len.get() - delta;

                let rest = (rem_len > 0).then(|| StringIdx::Buffer {
                    start: start + delta,
                    len: unsafe { NonZeroU32::new_unchecked(rem_len) },
                });

                Some((ch, rest))
            }

            StringIdx::Node(node_idx) => match self.nodes[node_idx.get() as usize] {
                StringNode::Concat { left, right } => {
                    let (ch, rest_left) = self.pop_front(Some(left))?;
                    Some((ch, self.concat(rest_left, Some(right))))
                }
                StringNode::Slice { source, start, len } => {
                    let ch = self.first(idx)?;
                    let delta = ch.len_utf8();
                    let rem_len = len - delta;

                    let rest = (rem_len > 0).then(|| {
                        self.alloc_node(StringNode::Slice {
                            source,
                            start: start + delta,
                            len: rem_len,
                        })
                    });

                    Some((ch, rest))
                }
                StringNode::Flat { start, len } => {
                    let s = unsafe {
                        std::str::from_utf8_unchecked(
                            &self.data[start as usize..(start + len) as usize],
                        )
                    };
                    let ch = s.chars().next()?;
                    let delta = ch.len_utf8() as u32;
                    let rem_len = len - delta;

                    let rest = (rem_len > 0).then(|| {
                        self.alloc_node(StringNode::Flat {
                            start: start + delta,
                            len: rem_len,
                        })
                    });

                    Some((ch, rest))
                }
            },
        }
    }

    /// Removes and returns the last character along with the remaining string index, if it exists.
    pub fn pop_back(&mut self, idx: Option<StringIdx>) -> Option<(Option<StringIdx>, char)> {
        match idx? {
            StringIdx::Static(key) => {
                let s = &self.interner[key];
                let ch = s.chars().next_back()?;
                let rem_len = s.len() - ch.len_utf8();

                let rest = (rem_len > 0).then(|| {
                    self.alloc_node(StringNode::Slice {
                        source: StringIdx::Static(key),
                        start: 0,
                        len: rem_len,
                    })
                });
                Some((rest, ch))
            }

            StringIdx::Inline { len, bytes } => {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[..len.get() as usize]) };
                let ch = s.chars().next_back()?;
                let rem_len = len.get() - ch.len_utf8() as u8;

                let rest = (rem_len > 0).then(|| StringIdx::Inline {
                    len: unsafe { NonZeroU8::new_unchecked(rem_len) },
                    bytes,
                });
                Some((rest, ch))
            }

            StringIdx::Buffer { start, len } => {
                let s = unsafe {
                    std::str::from_utf8_unchecked(
                        &self.data[start as usize..(start + len.get()) as usize],
                    )
                };
                let ch = s.chars().next_back()?;
                let rem_len = len.get() - ch.len_utf8() as u32;

                let rest = (rem_len > 0).then(|| StringIdx::Buffer {
                    start,
                    len: unsafe { NonZeroU32::new_unchecked(rem_len) },
                });
                Some((rest, ch))
            }

            StringIdx::Node(node_idx) => match self.nodes[node_idx.get() as usize] {
                StringNode::Concat { left, right } => {
                    let (rest_right, ch) = self.pop_back(Some(right))?;
                    Some((self.concat(Some(left), rest_right), ch))
                }
                StringNode::Slice { source, start, len } => {
                    let ch = self.last(idx)?;
                    let rem_len = len - ch.len_utf8();

                    let rest = (rem_len > 0).then(|| {
                        self.alloc_node(StringNode::Slice {
                            source,
                            start,
                            len: rem_len,
                        })
                    });

                    Some((rest, ch))
                }
                StringNode::Flat { start, len } => {
                    let s = unsafe {
                        std::str::from_utf8_unchecked(
                            &self.data[start as usize..(start + len) as usize],
                        )
                    };
                    let ch = s.chars().next_back()?;
                    let rem_len = len - ch.len_utf8() as u32;

                    let rest = (rem_len > 0).then(|| {
                        self.alloc_node(StringNode::Flat {
                            start,
                            len: rem_len,
                        })
                    });

                    Some((rest, ch))
                }
            },
        }
    }

    /// Splits the string at the front, returning a unit token for the character and the remainder.
    pub fn split_front(
        &mut self,
        idx: Option<StringIdx>,
    ) -> Option<(StringIdx, Option<StringIdx>)> {
        let (head, tail) = self.pop_front(idx)?;
        Some((StringIdx::unit(head), tail))
    }

    /// Splits the string at the back, returning the remainder and a unit token for the last character.
    pub fn split_back(&mut self, idx: Option<StringIdx>) -> Option<(Option<StringIdx>, StringIdx)> {
        let (tail, last) = self.pop_back(idx)?;
        Some((tail, StringIdx::unit(last)))
    }

    /// Checks if the string contains a substring.
    ///
    /// Automatically flattens structural strings internally inside the arena
    /// to guarantee fast, contiguous SIMD execution—without mutating the caller's index.
    pub fn contains(&mut self, idx: Option<StringIdx>, needle: &str) -> bool {
        self.get(&idx).contains(needle)
    }

    /// Returns the character at a logical index.
    ///
    /// Transparently performs path-compression inside the arena registry on first
    /// access, ensuring subsequent reads bypass tree traversal entirely.
    pub fn at(&mut self, idx: Option<StringIdx>, char_index: usize) -> Option<char> {
        self.get(&idx).chars().nth(char_index)
    }

    pub fn eq(&mut self, left: Option<StringIdx>, right: Option<StringIdx>) -> bool {
        match (left, right) {
            (None, None) => true,
            (Some(_), None) | (None, Some(_)) => false,
            (Some(a), Some(b)) => {
                if a == b {
                    return true;
                }

                if let (Some(slice_a), Some(slice_b)) = (self.as_str(&a), self.as_str(&b)) {
                    return slice_a == slice_b;
                }

                // Slow Path: At least one is an un-flattened structural Node.
                let a_idx = &Some(a);
                let b_idx = &Some(b);
                let [a, b] = self.resolve_many([a_idx, b_idx]);
                a.get() == b.get()
            }
        }
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

        let idx = arena.alloc(short_str);

        assert!(matches!(idx, Some(StringIdx::Inline { .. })));
        assert_eq!(arena.get(&idx), "hello");
    }

    #[test]
    fn test_dynamic_slice_allocation() {
        let mut arena = StringArena::new(StrInterner::default());
        let long_str = "this_string_is_longer_than_eleven_bytes";

        let idx = arena.alloc(long_str);

        assert!(matches!(idx, Some(StringIdx::Buffer { .. })));
        assert_eq!(arena.get(&idx), long_str);
    }

    #[test]
    fn test_structural_concat_and_flattening() {
        let mut arena = StringArena::new(StrInterner::default());

        let left = arena.alloc("abc"); // Inline
        let right = arena.alloc("defghijklmnopqrstuvw"); // Slice

        // Structural O(1) union
        let structural_idx = arena.concat(left, right);
        assert!(matches!(structural_idx, Some(StringIdx::Node(_))));
        assert_eq!(arena.get(&structural_idx), "abcdefghijklmnopqrstuvw");
    }

    #[test]
    fn test_sso_concatenation_merging() {
        let mut arena = StringArena::new(StrInterner::default());
        let left = arena.alloc("foo");
        let right = arena.alloc("bar");

        // "foo" (3) + "bar" (3) = 6 bytes. Should bypass the concat arena entirely and merge inline.
        let merged = arena.concat(left, right);
        assert!(matches!(merged, Some(StringIdx::Inline { .. })));
        assert_eq!(arena.get(&merged), "foobar");
    }

    #[test]
    fn test_fragment_streaming() {
        let mut interner = StrInterner::default();
        let t1 = interner.intern("static_prefix_");
        let t2 = interner.intern("_static_suffix");

        let mut arena = StringArena::new(interner);
        let s1 = Some(StringIdx::Static(t1));
        let s2 = arena.alloc("runtime_");
        let s3 = Some(StringIdx::Static(t2));

        let c1 = arena.concat(s1, s2);
        let root = arena.concat(c1, s3);

        assert!(matches!(root, Some(StringIdx::Node(_))));

        let mut output = String::new();
        arena.for_each_fragment(root.unwrap(), &mut |frag| {
            output.push_str(frag);
        });

        assert_eq!(output, "static_prefix_runtime__static_suffix");
    }
}
