use kola_utils::convert::TryAsRef;
use std::{hash::Hash, marker::PhantomData, mem, num::NonZeroU32};

use crate::{
    instr::Instr,
    ir::{Ir, IrView},
};

#[derive(Debug)]
pub struct Id<T> {
    id: NonZeroU32,
    t: PhantomData<T>,
}

const _: () = {
    // Ensure that Id<T> is always 4 bytes in size
    // and using nieche optimization for Option<Id<T>>.
    assert!(mem::size_of::<Id<()>>() == 4 * mem::size_of::<u8>());
    assert!(mem::size_of::<Option<Id<()>>>() == 4 * mem::size_of::<u8>());
};

impl<T> Id<T> {
    pub(crate) fn new(id: u32) -> Self {
        let id = NonZeroU32::new(id).expect("Id cannot be zero");

        Self { id, t: PhantomData }
    }

    pub fn as_u32(&self) -> u32 {
        self.id.get()
    }

    pub fn as_usize(&self) -> usize {
        self.id.get() as usize
    }

    pub fn get(self, ir: &Ir) -> T
    where
        T: Copy,
        Instr: TryAsRef<T>,
    {
        ir.instr(self)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
