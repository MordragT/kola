use std::ops::ControlFlow;

use kola_ir::prelude::{Id as InstrId, instr as ir, *};
use kola_resolver::{
    phase::{ResolvePhase, ResolvedNodes},
    symbol::Sym,
};
use kola_tree::{
    node::Namespace,
    prelude::{Id as TreeId, *},
};

use crate::symbol::SymbolEnv;

#[derive(Debug)]
pub struct Normalizer<'a, Node> {
    root_id: TreeId<Node>,
    next: InstrId<ir::Expr>,
    hole: ir::Symbol,
    builder: &'a mut IrBuilder,
    symbols: SymbolEnv<'a>,
}

impl<'a, Node> Normalizer<'a, Node> {
    // pub fn new(root_id: TreeId<Node>, resolved: &'a ResolvedNodes) -> Self {
    //     let mut symbols = SymbolEnv::new(resolved);
    //     let mut builder = IrBuilder::new();

    //     let hole = symbols.next();
    //     let arg = builder.add(ir::Atom::Symbol(hole));
    //     let next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

    //     Self {
    //         root_id,
    //         builder,
    //         next,
    //         hole,
    //         symbols,
    //     }
    // }

    pub fn new(
        root_id: TreeId<Node>,
        next: InstrId<ir::Expr>,
        hole: ir::Symbol,
        builder: &'a mut IrBuilder,
        symbols: SymbolEnv<'a>,
    ) -> Self {
        Self {
            root_id,
            next,
            hole,
            builder,
            symbols,
        }
    }

    pub fn next_symbol(&mut self) -> ir::Symbol {
        self.symbols.next()
    }

    pub fn symbol_of<T, N>(&self, id: TreeId<T>) -> ir::Symbol
    where
        N: Namespace,
        T: MetaCast<ResolvePhase, Meta = Sym<N>>, // TODO maybe ValueSym ??
    {
        self.symbols.symbol_of(id)
    }

    /// emit(atom) ~= let self.hole = atom; self.next
    pub fn emit(&mut self, atom: InstrId<ir::Atom>) {
        let next = ir::Expr::Let(ir::LetExpr {
            bind: self.hole,
            value: atom,
            next: self.next,
        });
        self.next = self.builder.add(next);
    }

    /// Run a nested normalization with fresh state, then restore original state
    /// Returns the normalized expression
    fn with_fresh_context<T, F>(&mut self, tree: &T, f: F) -> InstrId<ir::Expr>
    where
        T: TreeView,
        F: FnOnce(&mut Self, &T) -> ControlFlow<()>,
    {
        // Create fresh symbols for the nested context
        let fresh_hole = self.next_symbol();
        let fresh_arg = self.builder.add(ir::Atom::Symbol(fresh_hole));
        let fresh_ret = self
            .builder
            .add(ir::Expr::Ret(ir::RetExpr { arg: fresh_arg }));

        // Save current state
        let saved_next = self.next;
        let saved_hole = self.hole;

        // Set up fresh state
        self.next = fresh_ret;
        self.hole = fresh_hole;

        // Run the nested normalization
        let _ = f(self, tree);

        // Get the result
        let result = self.next;

        // Restore original state
        self.next = saved_next;
        self.hole = saved_hole;

        result
    }

    pub fn run<T>(mut self, tree: &T) -> InstrId<ir::Expr>
    where
        T: TreeView,
        TreeId<Node>: Visitable<T>,
    {
        let root = self.root_id;

        match root.visit_by(&mut self, tree) {
            ControlFlow::Continue(_) => (),
            ControlFlow::Break(_) => (),
        }

        self.next
    }
}

impl<'a, T, Node> Visitor<T> for Normalizer<'a, Node>
where
    T: TreeView,
{
    type BreakValue = ();

    fn visit_literal_expr(
        &mut self,
        id: TreeId<node::LiteralExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let atom = match *id.get(tree) {
            node::LiteralExpr::Num(n) => ir::Atom::Num(n),
            node::LiteralExpr::Str(s) => ir::Atom::Str(s),
            node::LiteralExpr::Bool(b) => ir::Atom::Bool(b),
            node::LiteralExpr::Char(c) => ir::Atom::Char(c),
        };

        let atom = self.builder.add(atom);
        self.emit(atom);
        ControlFlow::Continue(())
    }

    fn visit_path_expr(
        &mut self,
        id: TreeId<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::PathExpr {
            path,
            binding,
            select,
        } = id.get(tree);

        if !select.is_empty() {
            todo!()
        }

        if let Some(path) = path {
            todo!()
        } else {
            // This is a bit weird, I should definitely split the PathExpr so that I get a SelectExpr,
            // but essentially this is currently the symbol of the value bind in the current module,
            // if the module path is empty.
            // Otherwise this would be the symbol of a value bind in another module, which is not yet supported.
            let symbol = self.symbol_of(id);
            let atom = self.builder.add(ir::Atom::Symbol(symbol));

            self.emit(atom)
        }

        ControlFlow::Continue(())
    }

    /// normalize(let x = e1 in e2, hole, ctx) =
    /// normalize(e1,x,normalize(e2,hole,ctx))
    fn visit_let_expr(
        &mut self,
        id: TreeId<node::LetExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value,
            inside,
        } = *id.get(tree);

        self.visit_expr(inside, tree)?;
        self.hole = self.symbol_of(id);
        self.visit_expr(value, tree)
    }

    // normalize(\x => e, hole, ctx) =
    // let f = \x => normalize_with(e) in ctx[hole:=f]
    fn visit_lambda_expr(
        &mut self,
        id: TreeId<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr { param, body } = *id.get(tree);

        let param = self.symbol_of(id);
        let body = self.with_fresh_context(tree, |this, tree| this.visit_expr(body, tree));
        let func = self.builder.add(ir::Atom::Func(ir::Func { param, body }));

        self.emit(func);
        ControlFlow::Continue(())
    }

    // normalize((e0 e1), hole, ctx) =
    // normalize(e1,x, normalize(e0,f, let hole = (f x) in ctx))
    fn visit_call_expr(
        &mut self,
        id: TreeId<node::CallExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CallExpr { func, arg } = *id.get(tree);

        // Create fresh symbols for function and argument
        let f_sym = self.next_symbol();
        let x_sym = self.next_symbol();

        // Create atoms for the symbols
        let f_atom = self.builder.add(ir::Atom::Symbol(f_sym));
        let x_atom = self.builder.add(ir::Atom::Symbol(x_sym));

        // Create the call expression that will be the "context" for our normalizations
        let call_expr = self.builder.add(ir::Expr::Call(ir::CallExpr {
            bind: self.hole, // Result goes into current hole
            func: f_atom,    // Function symbol
            arg: x_atom,     // Argument symbol
            next: self.next, // Current continuation
        }));

        // Now normalize in reverse order (CPS style):
        // First, set up context for function normalization
        self.next = call_expr;
        self.hole = f_sym;

        // Normalize the function expression
        self.visit_expr(func, tree)?;

        // Then, normalize the argument into x_sym
        self.hole = x_sym;
        self.visit_expr(arg, tree)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kola_tree::prelude::*;

    // #[test]
    // fn test_normalizer() {
    //     // Example usage of the Normalizer
    //     let tree = Tree::new();
    //     let root_id = tree.root_id();

    //     let mut builder = IrBuilder::new();
    //     let symbols = SymbolEnv::new(&tree);

    //     let mut normalizer = Normalizer::new(
    //         root_id,
    //         builder.next(),
    //         symbols.next(),
    //         &mut builder,
    //         symbols,
    //     );
    //     let result = normalizer.run(&tree);

    //     assert!(result.is_ok());
    // }
}

// impl<T> Normalizer<T> {
//     pub fn new(root_id: TreeId<T>) -> Self {
//         Self {
//             root_id,
//             builder: IrBuilder::new(),
//         }
//     }
//
//     pub fn finish(tree: &Tree) -> Ir {
//         let mut normalizer = Self::new();

//         let root = tree.root_id().get(tree);
//         let root = normalizer.normalize_with(root, tree);

//         self.builder.finish(root)
//     }

//     // TODO better name that a function body is normalized here
//     fn normalize_with(&mut self, expr: &node::Expr, tree: &Tree) -> InstrId<instr::Expr> {
//         let bind = self.symbols.next();
//         let arg = self.builder.add(instr::Atom::Symbol(bind));
//         let ret = self.builder.add(instr::Expr::Ret { arg });

//         self.normalize_expr(expr, bind, ret, tree)
//     }

//     fn normalize_expr(
//         &mut self,
//         expr: &node::Expr,
//         hole: instr::Symbol,
//         ctx: InstrId<instr::Expr>,
//         tree: &Tree,
//     ) -> InstrId<instr::Expr> {
//         use node::Expr::*;

//         match expr {
//             Error(_) => panic!(),
//             Literal(id) => {
//                 let lit = id.get(tree);
//                 let atom = self.builder.add(instr::Atom::from(lit.clone()));
//                 self.builder.add(subst(ctx, hole, atom))
//             }
//             Ident(id) => {
//                 let ident = id.get(tree);
//                 let symbol = self.symbols.lookup(&ident.0);
//                 let atom = self.builder.add(instr::Atom::from(symbol));
//                 self.builder.add(subst(ctx, hole, atom))
//             }

//             List(id) => todo!(),
//             Record(id) => {
//                 let node::RecordExpr { fields } = id.get(tree);

//                 todo!()
//             }
//             RecordSelect(id) => todo!(),
//             RecordExtend(id) => todo!(),
//             RecordRestrict(id) => todo!(),
//             RecordUpdate(id) => todo!(),
//             Unary(id) => todo!(),
//             Binary(id) => todo!(),
//             // normalize(let x = e1 in e2, hole, ctx) =
//             // normalize(e1,x,normalize(e2,hole,ctx))
//             Let(id) => {
//                 let node::LetExpr {
//                     name,
//                     value,
//                     inside,
//                 } = id.get(tree);

//                 let res = self.normalize_expr(inside.get(tree), hole, ctx, tree);

//                 let bind = self.symbols.lookup(name.get(tree));
//                 let res = self.normalize_expr(value.get(tree), bind, res, tree);
//                 res
//             }
//             If(id) => {
//                 let if_ = id.get(tree);
//                 todo!()
//             }
//             Case(id) => todo!(),
//             // normalize(\x => e, hole, ctx) =
//             // let f = \x => normalize_with(e) in ctx[hole:=f]
//             Lambda(id) => {
//                 let node::LambdaExpr { param, body } = id.get(tree);

//                 let bind = self.symbols.next();
//                 let atom = self.builder.add(instr::Atom::from(bind));

//                 // TODO scope
//                 let param = self.symbols.lookup(param.get(tree));
//                 let body = self.normalize_with(body.get(tree), tree);
//                 let func = self.builder.add(instr::Atom::Func { param, body });

//                 let next = self.builder.add(subst(ctx, hole, atom));

//                 self.builder.add(instr::Expr::Let {
//                     bind,
//                     value: func,
//                     next,
//                 })
//             }
//             // normalize((e0 e1), hole, ctx) =
//             // normalize(e1,x, normalize(e0,f, let hole = (f x) in ctx))
//             Call(id) => {
//                 let node::CallExpr { func, arg } = id.get(tree);

//                 let f = self.symbols.next();
//                 let f_atom = self.builder.add(instr::Atom::from(f));
//                 let x = self.symbols.next();
//                 let x_atom = self.builder.add(instr::Atom::from(x));

//                 let res = self.builder.add(instr::Expr::Call {
//                     bind: hole,
//                     func: f_atom,
//                     arg: x_atom,
//                     next: ctx,
//                 });

//                 let res = self.normalize_expr(func.get(tree), f, res, tree);
//                 self.normalize_expr(arg.get(tree), x, res, tree)
//             }
//         }
//     }
// }

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
