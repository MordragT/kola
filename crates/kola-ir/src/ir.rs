use kola_print::prelude::*;
use kola_utils::convert::TryAsRef;

use crate::{
    id::InstrId,
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

    pub fn push<T>(&mut self, instr: T) -> InstrId<T>
    where
        T: Into<Instr>,
    {
        let id = self.instructions.len() as u32;

        let instr = instr.into();
        self.instructions.push(instr);

        InstrId::new(id)
    }

    pub fn finish(self, root: InstrId<Expr>) -> Ir {
        let Self { instructions } = self;
        Ir { instructions, root }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    instructions: Vec<Instr>,
    root: InstrId<Expr>,
}

impl Ir {
    pub fn get<T>(&self, id: InstrId<T>) -> &T
    where
        Instr: TryAsRef<T>,
    {
        let instr = &self.instructions[id.as_usize()];
        instr.try_as_ref().unwrap()
    }
}

impl Printable<()> for Ir {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        self.root.notate(self, arena)
    }
}
