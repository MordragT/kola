use kola_print::prelude::*;
use kola_utils::convert::TryAsRef;

use crate::{
    id::Id,
    instr::{Expr, Instr},
};

#[derive(Debug, Clone)]
pub struct IrBuilder {
    instructions: Vec<Instr>,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn add<T>(&mut self, instr: T) -> Id<T>
    where
        T: Into<Instr>,
    {
        let id = self.instructions.len() as u32;

        let instr = instr.into();
        self.instructions.push(instr);

        Id::new(id)
    }

    pub fn finish(self, root: Id<Expr>) -> Ir {
        let Self { instructions } = self;
        Ir { instructions, root }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    instructions: Vec<Instr>,
    root: Id<Expr>,
}

impl Ir {
    pub fn get(&self, id: usize) -> Instr {
        self.instructions[id]
    }

    pub fn instr<T>(&self, id: Id<T>) -> &T
    where
        Instr: TryAsRef<T>,
    {
        let instr = &self.instructions[id.as_usize()];
        instr.try_as_ref().unwrap()
    }

    pub fn root(&self) -> Id<Expr> {
        self.root
    }
}

impl Printable<()> for Ir {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        self.root.notate(self, arena)
    }
}
