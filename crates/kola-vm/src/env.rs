use std::{num::NonZeroU32, ops::Index, range::Range};

use kola_ir::instr::Symbol;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnvIdx(Range<NonZeroU32>);

impl EnvIdx {
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
        Self(Range {
            start: NonZeroU32::new((start + 1) as u32).expect("string arena overflow"),
            end: NonZeroU32::new((end + 1) as u32).expect("string arena overflow"),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnvArena {
    data: Vec<(Symbol, Value)>,
}

impl EnvArena {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn alloc(&mut self, entries: &[(Symbol, Value)]) -> EnvIdx {
        let start = self.data.len();
        self.data.extend_from_slice(entries);
        let end = self.data.len();

        self.data[start..end].sort_by_key(|(sym, _)| *sym);

        EnvIdx::make(start, end)
    }

    /// Copy an existing env within the arena.
    #[inline]
    pub fn copy(&mut self, idx: EnvIdx) -> EnvIdx {
        let start = self.data.len();
        self.data.extend_from_within(idx.start()..idx.end());
        let end = self.data.len();
        EnvIdx::make(start, end)
    }

    #[inline]
    pub fn get(&self, idx: EnvIdx, name: Symbol) -> Option<Value> {
        let pos =
            match self.data[idx.start()..idx.end()].binary_search_by_key(&name, |(sym, _)| *sym) {
                Ok(pos) => pos,
                Err(_) => return None,
            };

        Some(self.data[idx.start() + pos].1)
    }

    pub fn insert(&mut self, idx: EnvIdx, name: Symbol, value: impl Into<Value>) -> EnvIdx {
        let start = idx.start();
        let end = idx.end();

        // Find the insertion point (before any existing entry with the same key)
        let insert_pos = match self.data[start..end].binary_search_by_key(&name, |(sym, _)| *sym) {
            Ok(pos) => pos, // insert at existing key position → shadows it
            Err(pos) => pos,
        };

        let new_start = self.data.len();

        // Copy entries before insertion point
        if insert_pos > 0 {
            self.data.extend_from_within(start..start + insert_pos);
        }

        // Insert new entry (this will be the visible one)
        self.data.push((name, value.into()));

        // Copy the old entry at insert_pos (now shadowed) plus everything after it
        let remaining_start = start + insert_pos;
        if remaining_start < end {
            self.data.extend_from_within(remaining_start..end);
        }

        let new_end = self.data.len();
        EnvIdx::make(new_start, new_end)
    }

    pub fn remove(&mut self, idx: EnvIdx, name: Symbol) -> Option<(Value, EnvIdx)> {
        let start = idx.start();
        let end = idx.end();

        let pos = match self.data[start..end].binary_search_by_key(&name, |(sym, _)| *sym) {
            Ok(pos) => pos,
            Err(_) => return None,
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
        Some((removed_value, EnvIdx::make(new_start, new_end)))
    }
}

impl Index<EnvIdx> for EnvArena {
    type Output = [(Symbol, Value)];

    fn index(&self, index: EnvIdx) -> &Self::Output {
        &self.data[index.start()..index.end()]
    }
}
