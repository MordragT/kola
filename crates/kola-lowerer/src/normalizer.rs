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

    use kola_ir::{
        instr as ir,
        ir::{Ir, IrBuilder},
    };
    use kola_resolver::{phase::ResolvedNodes, symbol::ValueSym};
    use kola_tree::prelude::*;
    use kola_utils::interner::StrInterner;
    use kola_vm::{machine::CekMachine, value::Value};

    use super::Normalizer;
    use crate::symbol::SymbolEnv;

    // TODO this wont work for path expressions using bound symbols,
    fn mock_resolved(tree: &impl TreeView) -> ResolvedNodes {
        let mut resolved = ResolvedNodes::default();

        for query in tree.query3::<node::PathExpr, node::LetExpr, node::LambdaExpr>() {
            match query {
                Query3::V0(id, _path) => resolved.insert_meta(id, ValueSym::new()),
                Query3::V1(id, _let) => resolved.insert_meta(id, ValueSym::new()),
                Query3::V2(id, _lambda) => resolved.insert_meta(id, ValueSym::new()),
            }
        }

        resolved
    }

    fn normalize<T>(tree: TreeBuilder, root_id: Id<T>, resolved: &ResolvedNodes) -> Ir
    where
        Id<T>: Visitable<TreeBuilder>,
    {
        let mut symbols = SymbolEnv::new(resolved);
        let mut builder = IrBuilder::new();

        let hole = symbols.next();
        let arg = builder.add(ir::Atom::Symbol(hole));
        let next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

        let normalizer = Normalizer::new(root_id, next, hole, &mut builder, symbols);
        let root = normalizer.run(&tree);

        builder.finish(root)
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::LiteralExpr::Num(10.0));

        let resolved = mock_resolved(&builder);
        let ir = normalize(builder, lit, &resolved);
        let mut machine = CekMachine::new(ir);
        let value = machine.run().unwrap();

        assert_eq!(value, Value::Num(10.0))
    }

    #[test]
    fn path_expr_unbound() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        let x = interner.intern("x");

        // Build a simple path expression: x (no module path, just binding)
        let binding = builder.insert(node::ValueName::new(x));
        let path_expr = builder.insert(node::PathExpr {
            path: None,
            binding,
            select: vec![],
        });

        let resolved = mock_resolved(&builder);
        let ir = normalize(builder, path_expr, &resolved);
        let mut machine = CekMachine::new(ir);

        // Expect unbound symbol error.
        let _err = machine.run().unwrap_err();
    }

    #[test]
    fn let_expr() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();
        let mut resolved = ResolvedNodes::default();

        let x = interner.intern("x");
        let x_sym = ValueSym::new();

        // Build: let x = 42 in x
        let name = builder.insert(node::ValueName::new(x));
        let value = builder.insert(node::LiteralExpr::Num(42.0));
        let value = builder.insert(node::Expr::Literal(value));
        let inside = builder.insert(node::PathExpr {
            path: None,
            binding: name,
            select: vec![],
        });
        resolved.insert_meta(inside, x_sym);
        let inside = builder.insert(node::Expr::Path(inside));

        let let_expr = builder.insert(node::LetExpr {
            name,
            value,
            inside,
        });
        resolved.insert_meta(let_expr, x_sym);

        let ir = normalize(builder, let_expr, &resolved);
        let mut machine = CekMachine::new(ir);
        let value = machine.run().unwrap();

        // Should evaluate to 42 (the value bound to x)
        assert_eq!(value, Value::Num(42.0));
    }

    #[test]
    fn lambda_call_expr() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();
        let mut resolved = ResolvedNodes::default();

        let x = interner.intern("x");
        let x_sym = ValueSym::new();

        // Build: (\x => x) 42  (identity function applied to 42)

        // Create the lambda parameter reference in body: x
        let param_name = builder.insert(node::ValueName::new(x));
        let lambda_body_path = builder.insert(node::PathExpr {
            path: None,
            binding: param_name,
            select: vec![],
        });
        resolved.insert_meta(lambda_body_path, x_sym); // Reference to parameter
        let lambda_body = builder.insert(node::Expr::Path(lambda_body_path));

        // Create the lambda: \x => x
        let lambda_expr = builder.insert(node::LambdaExpr {
            param: param_name,
            body: lambda_body,
        });
        resolved.insert_meta(lambda_expr, x_sym); // Lambda gets parameter symbol
        let lambda = builder.insert(node::Expr::Lambda(lambda_expr));

        // Create the argument: 42
        let arg_literal = builder.insert(node::LiteralExpr::Num(42.0));
        let arg = builder.insert(node::Expr::Literal(arg_literal));

        // Create the call: (\x => x) 42
        let call_expr = builder.insert(node::CallExpr { func: lambda, arg });

        let ir = normalize(builder, call_expr, &resolved);
        let mut machine = CekMachine::new(ir);
        let value = machine.run().unwrap();

        // Should evaluate to 42 (identity function returns its argument)
        assert_eq!(value, Value::Num(42.0));
    }
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
