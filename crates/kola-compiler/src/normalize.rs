use kola_tree::prelude::*;
use std::collections::HashMap;

use crate::{
    CompilePhase,
    ir::{self, Instr, IrBuilder},
};

pub struct Normalizer {
    builder: IrBuilder,
    cache: Vec<Meta<CompilePhase>>,
    symbols: HashMap<Symbol, ir::Symbol>,
}

impl Normalizer {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new(),
            cache: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    fn fresh_symbol(&mut self, symbol: &Symbol) -> ir::Symbol {
        let next = self.symbols.len() as u32;

        *self
            .symbols
            .entry(symbol.clone())
            .or_insert(ir::Symbol(next))
    }

    pub fn normalize(mut self, tree: &Tree) -> ir::Ir {
        for node in tree.iter_nodes() {
            match node {
                Node::Name(n) => {
                    let symbol = self.fresh_symbol(&n.0);
                    let id = self.builder.push(symbol);
                    self.cache.push(Meta::Name(id));
                }
                Node::Ident(i) => {
                    let symbol = self.fresh_symbol(&i.0);
                    let id = self.builder.push(symbol);
                    self.cache.push(Meta::Ident(id));
                }
                Node::Literal(l) => {
                    let id = self.builder.push(l.clone());
                    self.cache.push(Meta::Literal(id));
                }
                Node::List(_) => todo!(),
                Node::Property(_) => todo!(),
                Node::Record(_) => todo!(),
                Node::RecordSelect(_) => todo!(),
                Node::RecordExtend(_) => todo!(),
                Node::RecordRestrict(_) => todo!(),
                Node::RecordUpdate(_) => todo!(),
                Node::UnaryOp(_) => todo!(),
                Node::Unary(_) => todo!(),
                Node::BinaryOp(_) => todo!(),
                Node::Binary(_) => todo!(),
                Node::Let(node::Let {
                    name,
                    value,
                    inside,
                }) => {
                    let symbol = *self.cache.meta(*name);
                    todo!()
                }
                Node::PatError(_) => todo!(),
                Node::Wildcard(_) => todo!(),
                Node::LiteralPat(_) => todo!(),
                Node::IdentPat(_) => todo!(),
                Node::PropertyPat(_) => todo!(),
                Node::RecordPat(_) => todo!(),
                Node::Pat(_) => todo!(),
                Node::Branch(_) => todo!(),
                Node::Case(_) => todo!(),
                Node::If(_) => todo!(),
                Node::Func(_) => todo!(),
                Node::Call(_) => todo!(),
                Node::ExprError(_) => todo!(),
                Node::Expr(_) => todo!(),
            }
        }

        todo!()
    }
}
