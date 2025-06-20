use std::u32;

use kola_ir::instr as ir;
use kola_resolver::{
    phase::{ResolvePhase, ResolvedNodes, ResolvedValue},
    symbol::{Sym, ValueSym},
};
use kola_tree::{
    id::Id as TreeId,
    meta::{MetaCast, MetaView},
    node::{self, Namespace},
};

#[derive(Debug, Clone, Copy)]
pub struct SymbolEnv<'a> {
    resolved: &'a ResolvedNodes,
}

impl<'a> SymbolEnv<'a> {
    pub fn new(resolved: &'a ResolvedNodes) -> Self {
        Self { resolved }
    }

    pub fn next(&mut self) -> ir::Symbol {
        let sym = ValueSym::new();
        let symbol = ir::Symbol(sym.id());
        symbol
    }

    pub fn symbol_of<T, N>(&self, id: TreeId<T>) -> ir::Symbol
    where
        N: Namespace,
        T: MetaCast<ResolvePhase, Meta = Sym<N>>,
    {
        let sym = self.resolved.meta(id);
        ir::Symbol(sym.id())
    }

    pub fn atom_of(&self, id: TreeId<node::QualifiedExpr>) -> ir::Atom {
        match *self.resolved.meta(id) {
            ResolvedValue::Defined(sym) => ir::Atom::Symbol(ir::Symbol(sym.id())),
            ResolvedValue::Builtin(b) => ir::Atom::Builtin(b),
        }
    }
}

impl<'a> From<&'a ResolvedNodes> for SymbolEnv<'a> {
    fn from(resolved: &'a ResolvedNodes) -> Self {
        Self::new(resolved)
    }
}
