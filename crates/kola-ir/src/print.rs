use crate::ir::Ir;

#[derive(Debug, Clone, Copy)]
pub struct IrPrinter<'ir, T> {
    pub ir: &'ir Ir,
    pub node: T,
}

impl<'ir, T> IrPrinter<'ir, T> {
    pub fn new(ir: &'ir Ir, node: T) -> Self {
        Self { ir, node }
    }

    pub fn map<U>(self, f: impl FnOnce(T, &'ir Ir) -> U) -> IrPrinter<'ir, U> {
        IrPrinter {
            node: f(self.node, self.ir),
            ir: self.ir,
        }
    }

    pub fn to<U>(self, node: U) -> IrPrinter<'ir, U> {
        IrPrinter { node, ir: self.ir }
    }
}
