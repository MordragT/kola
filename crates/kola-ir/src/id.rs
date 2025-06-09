use kola_print::prelude::*;
use kola_utils::convert::TryAsRef;
use std::{hash::Hash, marker::PhantomData};

use crate::{instr::Instr, ir::Ir, print::IrPrinter};

#[derive(Debug)]
pub struct Id<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> Id<T> {
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
