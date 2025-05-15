use kola_print::prelude::*;
use kola_utils::convert::TryAsRef;
use std::{hash::Hash, marker::PhantomData};

use crate::{instr::Instr, ir::Ir};

#[derive(Debug)]
pub struct InstrId<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> InstrId<T> {
    pub(crate) fn new(id: u32) -> Self {
        Self { id, t: PhantomData }
    }

    pub fn as_usize(&self) -> usize {
        self.id as usize
    }

    pub fn get(self, ir: &Ir) -> &T
    where
        Instr: TryAsRef<T>,
    {
        ir.get(self)
    }
}

impl<T> Clone for InstrId<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for InstrId<T> {}

impl<T> PartialEq for InstrId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> Eq for InstrId<T> {}

impl<T> PartialOrd for InstrId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for InstrId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T> Hash for InstrId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> Printable<Ir> for InstrId<T>
where
    Instr: TryAsRef<T>,
    T: Printable<Ir>,
{
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        with.get(*self).notate(with, arena)
    }
}
