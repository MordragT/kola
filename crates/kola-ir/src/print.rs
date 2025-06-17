use kola_utils::interner::StrInterner;

use crate::ir::Ir;

#[derive(Debug, Clone, Copy)]
pub struct IrPrinter<'a, T> {
    pub ir: &'a Ir,
    pub interner: &'a StrInterner,
    pub node: T,
}

impl<'a, T> IrPrinter<'a, T> {
    pub fn new(ir: &'a Ir, interner: &'a StrInterner, node: T) -> Self {
        Self { ir, interner, node }
    }

    pub fn map<U>(self, f: impl FnOnce(T, &'a Ir) -> U) -> IrPrinter<'a, U> {
        IrPrinter {
            node: f(self.node, self.ir),
            ir: self.ir,
            interner: self.interner,
        }
    }

    pub fn to<U>(self, node: U) -> IrPrinter<'a, U> {
        IrPrinter {
            node,
            ir: self.ir,
            interner: self.interner,
        }
    }
}
