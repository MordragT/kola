use std::ops::ControlFlow;

use kola_ir::prelude::{Id as InstrId, instr as ir, *};
use kola_resolver::{phase::ResolvePhase, symbol::Sym};
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
        let node::PathExpr { path, select } = *id.get(tree);

        if let Some(_path) = path {
            // Module path resolution is not yet supported
            todo!("Module path resolution not implemented")
        } else {
            // No module path, so this is just a select expression in the current module
            self.visit_select_expr(select, tree)
        }
    }

    fn visit_select_expr(
        &mut self,
        id: TreeId<node::SelectExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::SelectExpr { source: _, fields } = id.get(tree);

        // Create symbol and corresponding atom
        let source_sym = self.symbol_of(id);
        let mut source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        if fields.is_empty() {
            // If there are no fields, we just return the source atom
            self.emit(source_atom);
            return ControlFlow::Continue(());
        }

        for field_id in fields.iter().take(fields.len() - 1) {
            let field_label = field_id.get(tree).0;

            // Create a fresh symbol for this intermediate result
            let next_sym = self.next_symbol();
            let next_atom = self.builder.add(ir::Atom::Symbol(next_sym));

            // Create the record access expression
            let access_expr = self
                .builder
                .add(ir::Expr::RecordAccess(ir::RecordAccessExpr {
                    bind: next_sym,
                    base: source_atom,
                    label: field_label,
                    next: self.next,
                }));

            // Update for next iteration
            self.next = access_expr;
            source_atom = next_atom;
        }

        // Handle the final field access - this binds to self.hole
        if let Some(last_field_id) = fields.last() {
            let field_label = last_field_id.get(tree).0;

            let final_access = self
                .builder
                .add(ir::Expr::RecordAccess(ir::RecordAccessExpr {
                    bind: self.hole,
                    base: source_atom,
                    label: field_label,
                    next: self.next,
                }));

            self.next = final_access;
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

        // Create fresh symbols and corresponding atoms
        let f_sym = self.next_symbol();
        let x_sym = self.next_symbol();

        let f_atom = self.builder.add(ir::Atom::Symbol(f_sym));
        let x_atom = self.builder.add(ir::Atom::Symbol(x_sym));

        // Create the call expression context and set continuation
        let call_expr = self.builder.add(ir::Expr::Call(ir::CallExpr {
            bind: self.hole,
            func: f_atom,
            arg: x_atom,
            next: self.next,
        }));
        self.next = call_expr;

        // Normalize in reverse order (CPS style)
        self.hole = x_sym;
        self.visit_expr(arg, tree)?;

        self.hole = f_sym;
        self.visit_expr(func, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_if_expr(
        &mut self,
        id: TreeId<node::IfExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::IfExpr {
            predicate,
            then,
            or,
        } = *id.get(tree);

        // Create fresh symbol and corresponding atom
        let pred_sym = self.next_symbol();
        let pred_atom = self.builder.add(ir::Atom::Symbol(pred_sym));

        let then_expr = self.with_fresh_context(tree, |this, tree| this.visit_expr(then, tree));
        let or_expr = self.with_fresh_context(tree, |this, tree| this.visit_expr(or, tree));

        // Create the if expression context and set continuation
        let if_expr = self.builder.add(ir::Expr::If(ir::IfExpr {
            bind: self.hole,
            predicate: pred_atom,
            then: then_expr,
            or: or_expr,
            next: self.next,
        }));
        self.next = if_expr;

        // Normalize predicate
        self.hole = pred_sym;
        self.visit_expr(predicate, tree)
    }

    fn visit_unary_expr(
        &mut self,
        id: TreeId<node::UnaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::UnaryExpr { op, operand } = *id.get(tree);

        // Create fresh symbol and corresponding atom
        let operand_sym = self.next_symbol();
        let operand_atom = self.builder.add(ir::Atom::Symbol(operand_sym));

        let unary_op = match *op.get(tree) {
            node::UnaryOp::Neg => ir::UnaryOp::Neg,
            node::UnaryOp::Not => ir::UnaryOp::Not,
        };

        // Create the unary expression context and set continuation
        let unary_expr = self.builder.add(ir::Expr::Unary(ir::UnaryExpr {
            bind: self.hole,
            op: unary_op,
            arg: operand_atom,
            next: self.next,
        }));
        self.next = unary_expr;

        // Normalize operand
        self.hole = operand_sym;
        self.visit_expr(operand, tree)
    }

    fn visit_binary_expr(
        &mut self,
        id: TreeId<node::BinaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::BinaryExpr { left, op, right } = *id.get(tree);

        // Create fresh symbols and correpsonding atoms
        let left_sym = self.next_symbol();
        let right_sym = self.next_symbol();

        let left_atom = self.builder.add(ir::Atom::Symbol(left_sym));
        let right_atom = self.builder.add(ir::Atom::Symbol(right_sym));

        let binary_op = match *op.get(tree) {
            node::BinaryOp::Add => ir::BinaryOp::Add,
            node::BinaryOp::Sub => ir::BinaryOp::Sub,
            node::BinaryOp::Mul => ir::BinaryOp::Mul,
            node::BinaryOp::Div => ir::BinaryOp::Div,
            node::BinaryOp::Rem => ir::BinaryOp::Rem,
            node::BinaryOp::Eq => ir::BinaryOp::Eq,
            node::BinaryOp::NotEq => ir::BinaryOp::NotEq,
            node::BinaryOp::Less => ir::BinaryOp::Less,
            node::BinaryOp::LessEq => ir::BinaryOp::LessEq,
            node::BinaryOp::Greater => ir::BinaryOp::Greater,
            node::BinaryOp::GreaterEq => ir::BinaryOp::GreaterEq,
            node::BinaryOp::And => ir::BinaryOp::And,
            node::BinaryOp::Or => ir::BinaryOp::Or,
            node::BinaryOp::Xor => ir::BinaryOp::Xor,
            node::BinaryOp::Merge => ir::BinaryOp::Merge,
        };

        // Create the binary expression that will be the "context" for our normalizations
        let binary_expr = self.builder.add(ir::Expr::Binary(ir::BinaryExpr {
            bind: self.hole,
            op: binary_op,
            lhs: left_atom,
            rhs: right_atom,
            next: self.next,
        }));
        self.next = binary_expr;

        // Normalize in reverse order (CPS style):
        self.hole = right_sym;
        self.visit_expr(right, tree)?;

        self.hole = left_sym;
        self.visit_expr(left, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_list_expr(
        &mut self,
        id: TreeId<node::ListExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let items = &id.get(tree).0;

        // Create fresh symbols for each item
        let item_syms = (0..items.len())
            .map(|_| self.next_symbol())
            .collect::<Vec<_>>();

        // Create the list expression context and set continuation
        let mut list_expr = ir::ListExpr {
            bind: self.hole,
            head: None,
            tail: None,
            next: self.next,
        };
        list_expr.prepend_all(item_syms.iter().map(ir::Atom::from), self.builder);
        self.next = self.builder.add(ir::Expr::List(list_expr));

        // Normalize expressions in reverse order (CPS style)
        for (&expr, sym) in items.iter().rev().zip(item_syms) {
            self.hole = sym;
            self.visit_expr(expr, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_expr(
        &mut self,
        id: TreeId<node::RecordExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let fields = &id.get(tree).0;

        // Create fresh symbols for each field value
        let field_value_syms: Vec<_> = (0..fields.len()).map(|_| self.next_symbol()).collect();

        let field_pairs: Vec<_> = fields
            .iter()
            .zip(&field_value_syms)
            .map(|(field_id, &value_sym)| {
                let field = field_id.get(tree);
                let label = field.field.get(tree).0;
                (label, ir::Atom::Symbol(value_sym))
            })
            .collect();

        // Create the record expression context and set continuation
        let mut record_expr = ir::RecordExpr {
            bind: self.hole,
            head: None,
            next: self.next,
        };
        record_expr.extend(field_pairs, self.builder);
        self.next = self.builder.add(ir::Expr::Record(record_expr));

        // Normalize field values in reverse order (CPS style)
        for (field_id, &value_sym) in fields.iter().rev().zip(field_value_syms.iter().rev()) {
            let field = field_id.get(tree);
            self.hole = value_sym;
            self.visit_expr(field.value, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_extend_expr(
        &mut self,
        id: TreeId<node::RecordExtendExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordExtendExpr {
            source,
            field,
            value,
        } = *id.get(tree);

        // Create fresh symbols and corresponding atoms
        let value_sym = self.next_symbol();
        let source_sym = self.next_symbol();

        let value_atom = self.builder.add(ir::Atom::Symbol(value_sym));
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        // Create the record extend expression context and set continuation
        let extend_expr = self
            .builder
            .add(ir::Expr::RecordExtend(ir::RecordExtendExpr {
                bind: self.hole,
                base: source_atom,
                label: field.get(tree).0,
                value: value_atom,
                next: self.next,
            }));
        self.next = extend_expr;

        // Normalize in reverse order (CPS style)
        self.hole = value_sym;
        self.visit_expr(value, tree)?;

        self.hole = source_sym;
        self.visit_expr(source, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_restrict_expr(
        &mut self,
        id: TreeId<node::RecordRestrictExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordRestrictExpr { source, field } = *id.get(tree);

        // Create fresh symbol and corresponding atom
        let source_sym = self.next_symbol();
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        // Create the record restrict expression context and set continuation
        let restrict_expr = self
            .builder
            .add(ir::Expr::RecordRestrict(ir::RecordRestrictExpr {
                bind: self.hole,
                base: source_atom,
                label: field.get(tree).0,
                next: self.next,
            }));
        self.next = restrict_expr;

        // Normalize in reverse order (CPS style)
        self.hole = source_sym;
        self.visit_expr(source, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_update_expr(
        &mut self,
        id: TreeId<node::RecordUpdateExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordUpdateExpr {
            source,
            field,
            op,
            value,
        } = *id.get(tree);

        // Create fresh symbols and corresponding atoms
        let value_sym = self.next_symbol();
        let source_sym = self.next_symbol();

        let value_atom = self.builder.add(ir::Atom::Symbol(value_sym));
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        let op = match *op.get(tree) {
            node::RecordUpdateOp::Assign => ir::RecordUpdateOp::Assign,
            node::RecordUpdateOp::AddAssign => ir::RecordUpdateOp::AddAssign,
            node::RecordUpdateOp::SubAssign => ir::RecordUpdateOp::SubAssign,
            node::RecordUpdateOp::MulAssign => ir::RecordUpdateOp::MulAssign,
            node::RecordUpdateOp::DivAssign => ir::RecordUpdateOp::DivAssign,
            node::RecordUpdateOp::RemAssign => ir::RecordUpdateOp::RemAssign,
        };

        // Create the record update expression context and set continuation
        let update_expr = self
            .builder
            .add(ir::Expr::RecordUpdate(ir::RecordUpdateExpr {
                bind: self.hole,
                base: source_atom,
                label: field.get(tree).0,
                op,
                value: value_atom,
                next: self.next,
            }));
        self.next = update_expr;

        // Normalize in reverse order (CPS style)
        self.hole = value_sym;
        self.visit_expr(value, tree)?;

        self.hole = source_sym;
        self.visit_expr(source, tree)?;

        ControlFlow::Continue(())
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

        for query in tree.query3::<node::SelectExpr, node::LetExpr, node::LambdaExpr>() {
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
        let mut machine = CekMachine::new(ir, StrInterner::new());
        let value = machine.run().unwrap();

        assert_eq!(value, Value::Num(10.0))
    }

    #[test]
    fn path_expr_unbound() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        let x = interner.intern("x");

        // Build a simple path expression: x (no module path, just binding)
        let source = builder.insert(node::ValueName::new(x));
        let select = builder.insert(node::SelectExpr {
            source,
            fields: Vec::new(),
        });
        let path_expr = builder.insert(node::PathExpr { path: None, select });

        let resolved = mock_resolved(&builder);
        let ir = normalize(builder, path_expr, &resolved);
        let mut machine = CekMachine::new(ir, interner);

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
        let select = builder.insert(node::SelectExpr {
            source: name,
            fields: vec![],
        });
        resolved.insert_meta(select, x_sym);
        let inside = builder.insert(node::PathExpr { path: None, select });
        let inside = builder.insert(node::Expr::Path(inside));

        let let_expr = builder.insert(node::LetExpr {
            name,
            value,
            inside,
        });
        resolved.insert_meta(let_expr, x_sym);

        let ir = normalize(builder, let_expr, &resolved);
        let mut machine = CekMachine::new(ir, interner);
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
        let select = builder.insert(node::SelectExpr {
            source: param_name,
            fields: vec![],
        });
        resolved.insert_meta(select, x_sym); // Reference to parameter
        let lambda_body_path = builder.insert(node::PathExpr { path: None, select });
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
        let mut machine = CekMachine::new(ir, interner);
        let value = machine.run().unwrap();

        // Should evaluate to 42 (identity function returns its argument)
        assert_eq!(value, Value::Num(42.0));
    }
}
