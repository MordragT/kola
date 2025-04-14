use kola_tree::prelude::*;
use std::collections::HashMap;

use crate::ir::{self, Instr, InstrId, IrBuilder};

// TODO scoping
#[derive(Debug, Clone, Default)]
pub struct SymbolEnv {
    generator: u32,
    table: HashMap<node::Symbol, ir::Symbol>,
}

impl SymbolEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> ir::Symbol {
        let symbol = ir::Symbol(self.generator);
        self.generator += 1;
        symbol
    }

    pub fn lookup(&mut self, symbol: &node::Symbol) -> ir::Symbol {
        if let Some(sym) = self.table.get(symbol) {
            *sym
        } else {
            let sym = self.next();
            self.table.insert(symbol.clone(), sym);
            sym
        }
    }
}

// subst(expr,hole,atom) ~= let hole = atom; expr
pub fn subst(expr: InstrId<ir::Expr>, hole: ir::Symbol, atom: InstrId<ir::Atom>) -> ir::Expr {
    ir::Expr::Let {
        bind: hole,
        value: atom,
        next: expr,
    }
}

// https://github.com/AntonPing/norem-draft-1/blob/e5a5d677b17cf966f9a19d6a88c094980cc6f6d4/src/backend/normalize.rs
pub struct Normalizer {
    builder: IrBuilder,
    // cache: Vec<Meta<CompilePhase>>,
    symbols: SymbolEnv,
}

impl Normalizer {
    pub fn new() -> Self {
        Self {
            builder: IrBuilder::new(),
            // cache: Vec::new(),
            symbols: SymbolEnv::new(),
        }
    }

    // fn fresh_symbol(&mut self, symbol: &Symbol) -> ir::Symbol {
    //     let next = self.symbols.len() as u32;

    //     *self
    //         .symbols
    //         .entry(symbol.clone())
    //         .or_insert(ir::Symbol(next))
    // }

    pub fn normalize(tree: &Tree) -> ir::Ir {
        let mut normalizer = Self::new();

        let root = tree.root_id().get(tree);
        let root = normalizer.normalize_with(root, tree);

        normalizer.builder.finish(root)
    }

    // TODO better name that a function body is normalized here
    fn normalize_with(&mut self, expr: &node::Expr, tree: &Tree) -> InstrId<ir::Expr> {
        let bind = self.symbols.next();
        let arg = self.builder.push(ir::Atom::Symbol(bind));
        let ret = self.builder.push(ir::Expr::Ret { arg });

        self.normalize_expr(expr, bind, ret, tree)
    }

    fn normalize_expr(
        &mut self,
        expr: &node::Expr,
        hole: ir::Symbol,
        ctx: InstrId<ir::Expr>,
        tree: &Tree,
    ) -> InstrId<ir::Expr> {
        use node::Expr::*;

        match expr {
            Error(_) => panic!(),
            Literal(id) => {
                let lit = id.get(tree);
                let atom = self.builder.push(ir::Atom::from(lit.clone()));
                self.builder.push(subst(ctx, hole, atom))
            }
            Ident(id) => {
                let ident = id.get(tree);
                let symbol = self.symbols.lookup(&ident.0);
                let atom = self.builder.push(ir::Atom::from(symbol));
                self.builder.push(subst(ctx, hole, atom))
            }

            List(id) => todo!(),
            Record(id) => {
                let node::RecordExpr { fields } = id.get(tree);

                todo!()
            }
            RecordSelect(id) => todo!(),
            RecordExtend(id) => todo!(),
            RecordRestrict(id) => todo!(),
            RecordUpdate(id) => todo!(),
            Unary(id) => todo!(),
            Binary(id) => todo!(),
            // normalize(let x = e1 in e2, hole, ctx) =
            // normalize(e1,x,normalize(e2,hole,ctx))
            Let(id) => {
                let node::LetExpr {
                    name,
                    value,
                    inside,
                } = id.get(tree);

                let res = self.normalize_expr(inside.get(tree), hole, ctx, tree);

                let bind = self.symbols.lookup(&name.get(tree).0);
                let res = self.normalize_expr(value.get(tree), bind, res, tree);
                res
            }
            If(id) => {
                let if_ = id.get(tree);
                todo!()
            }
            Case(id) => todo!(),
            // normalize(\x => e, hole, ctx) =
            // let f = \x => normalize_with(e) in ctx[hole:=f]
            Lambda(id) => {
                let node::LambdaExpr { param, body } = id.get(tree);

                let bind = self.symbols.next();
                let atom = self.builder.push(ir::Atom::from(bind));

                // TODO scope
                let param = self.symbols.lookup(&param.get(tree).0);
                let body = self.normalize_with(body.get(tree), tree);
                let func = self.builder.push(ir::Atom::Func { param, body });

                let next = self.builder.push(subst(ctx, hole, atom));

                self.builder.push(ir::Expr::Let {
                    bind,
                    value: func,
                    next,
                })
            }
            // normalize((e0 e1), hole, ctx) =
            // normalize(e1,x, normalize(e0,f, let hole = (f x) in ctx))
            Call(id) => {
                let node::CallExpr { func, arg } = id.get(tree);

                let f = self.symbols.next();
                let f_atom = self.builder.push(ir::Atom::from(f));
                let x = self.symbols.next();
                let x_atom = self.builder.push(ir::Atom::from(x));

                let res = self.builder.push(ir::Expr::Call {
                    bind: hole,
                    func: f_atom,
                    arg: x_atom,
                    next: ctx,
                });

                let res = self.normalize_expr(func.get(tree), f, res, tree);
                self.normalize_expr(arg.get(tree), x, res, tree)
            }
        }
    }
}

// from typing import Callable, Union

// def is_value(m):
//     """Checks if the given expression is a value."""
//     return isinstance(m, (int, float, bool, str))

// def normalize_term(m, k):
//     """Normalizes a given expression."""
//     if isinstance(m, list):
//         if m[0] == 'lambda':
//             params, body = m[1:]
//             return normalize_term(body, lambda n: ['lambda', params, n])
//         elif m[0] == 'let':
//             var, m1, m2 = m[1:]
//             return normalize_term(m1, lambda n1: ['let', [var, n1], normalize_term(m2, k)])
//         elif m[0] == 'if':
//             m1, m2, m3 = m[1:]
//             return normalize_term(m1, lambda t: ['if', t, normalize_term(m2, k), normalize_term(m3, k)])
//         elif m[0] in ['+', '-', '*', '/', '=']:
//             return normalize_name(m, k)
//         else:
//             return normalize_name(m, k)
//     elif is_value(m):
//         return k(m)
//     else:
//         raise ValueError("Invalid expression")

// def normalize_name(m, k):
//     """Normalizes a name or generates a fresh symbol for it."""
//     if is_value(m):
//         return k(m)
//     else:
//         # Generate a fresh symbol (implementation depends on your environment)
//         fresh_symbol = generate_fresh_symbol()
//         return ['let', [fresh_symbol, m], k(fresh_symbol)]

// def normalize_name_list(m_list, k):
//     """Normalizes a list of expressions."""
//     if not m_list:
//         return k([])
//     else:
//         head = m_list[0]
//         tail = m_list[1:]
//         return normalize_name(head, lambda t: normalize_name_list(tail, lambda t_tail: k([t] + t_tail)))

// def generate_fresh_symbol():
//     """Generates a unique symbol (e.g., 'x', 'y', 'z', ...)."""
//     # Implementation depends on your environment
//     # (e.g., using a counter)
//     pass
