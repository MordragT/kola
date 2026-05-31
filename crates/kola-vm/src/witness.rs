use kola_protocol::TypeProtocol;
use kola_utils::display::DisplayWith;
use std::{fmt, num::NonZeroU32};

use crate::heap::Heap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WitnessIdx(NonZeroU32);

impl WitnessIdx {
    #[inline]
    fn index(self) -> usize {
        self.0.get() as usize - 1
    }

    #[inline]
    fn make(index: usize) -> Self {
        Self(NonZeroU32::new((index + 1) as u32).expect("witness arena overflow"))
    }
}

impl DisplayWith<Heap> for WitnessIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        let s = heap.witnesses.get(*self);
        write!(f, "{}", s.to_json().unwrap())
    }
}

#[derive(Debug, Clone)]
pub struct WitnessArena {
    data: Vec<TypeProtocol>,
}

impl WitnessArena {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn alloc(&mut self, proto: TypeProtocol) -> WitnessIdx {
        let idx = self.data.len();
        self.data.push(proto);
        WitnessIdx::make(idx)
    }

    pub fn get(&self, idx: WitnessIdx) -> &TypeProtocol {
        &self.data[idx.index()]
    }
}

impl Default for WitnessArena {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl fmt::Display for WitnessArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WitnessArena({} bytes)", self.data.len())
    }
}
