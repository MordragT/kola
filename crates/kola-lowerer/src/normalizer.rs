use std::ops::ControlFlow;

use kola_ir::prelude::{Id as InstrId, instr as ir, *};
use kola_protocol::TypeInterner;
use kola_resolver::{
    phase::{ResolvePhase, ResolvedModule, ResolvedNodes, ResolvedValue},
    symbol::{Sym, ValueSym},
};
use kola_tree::{
    node::Namespace,
    prelude::{Id as TreeId, *},
};
use kola_typer::phase::TypedNodes;
use kola_utils::interner::StrInterner;

// https://matt.might.net/articles/cps-conversion/

#[derive(Debug)]
pub struct Normalizer<'a, Node> {
    root_id: TreeId<Node>,
    next: InstrId<ir::Expr>,
    hole: ir::Symbol,
    resolved: &'a ResolvedNodes,
    typed: &'a TypedNodes,
    builder: &'a mut IrBuilder,
    type_interner: &'a mut TypeInterner,
    str_interner: &'a StrInterner,
}

impl<'a, Node> Normalizer<'a, Node> {
    pub fn new(
        root_id: TreeId<Node>,
        next: InstrId<ir::Expr>,
        hole: ir::Symbol,
        resolved: &'a ResolvedNodes,
        typed: &'a TypedNodes,
        builder: &'a mut IrBuilder,
        type_interner: &'a mut TypeInterner,
        str_interner: &'a StrInterner,
    ) -> Self {
        Self {
            root_id,
            next,
            hole,
            resolved,
            typed,
            builder,
            type_interner,
            str_interner,
        }
    }

    pub fn next_symbol(&mut self) -> ir::Symbol {
        let sym = ValueSym::new();
        let symbol = ir::Symbol(sym.id());
        symbol
    }

    pub fn symbol_of<T, N>(&self, id: TreeId<T>) -> ir::Symbol
    where
        N: Namespace,
        T: MetaCast<ResolvePhase, Meta = Sym<N>>, // TODO maybe ValueSym ??
    {
        let sym = self.resolved.meta(id);
        ir::Symbol(sym.id())
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
        F: FnOnce(&mut Self, &T) -> ControlFlow<!>,
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

        ControlFlow::Continue(()) = root.visit_by(&mut self, tree);

        self.next
    }

    // Helper method to build FieldPath from AST field path
    fn build_field_path<T>(
        &mut self,
        field_path: &[TreeId<node::ValueName>],
        tree: &T,
    ) -> InstrId<ir::FieldPath>
    where
        T: TreeView,
    {
        field_path
            .iter()
            .rfold(None, |path_id, &field_id| {
                let label = field_id.get(tree).0;
                Some(self.builder.add(ir::FieldPath::new(label, path_id)))
            })
            .expect("Field path should not be empty")
    }
}

impl<'a, T, Node> Visitor<T> for Normalizer<'a, Node>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_literal_expr(
        &mut self,
        id: TreeId<node::LiteralExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let atom = match *id.get(tree) {
            node::LiteralExpr::Unit => ir::Atom::Noop,
            node::LiteralExpr::Num(n) => ir::Atom::Num(n),
            node::LiteralExpr::Str(s) => ir::Atom::Str(s),
            node::LiteralExpr::Bool(b) => ir::Atom::Bool(b),
            node::LiteralExpr::Char(c) => ir::Atom::Char(c),
        };

        let atom = self.builder.add(atom);
        self.emit(atom);
        ControlFlow::Continue(())
    }

    fn visit_qualified_expr(
        &mut self,
        id: TreeId<node::QualifiedExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            field_path,
        } = *id.get(tree);

        if let Some(path) = module_path {
            let ResolvedModule(sym) = *self.resolved.meta(path);
            let module_atom = self.builder.add(ir::Atom::Symbol(ir::Symbol(sym.id())));

            let name = source.get(tree).0;

            let access_expr = self
                .builder
                .add(ir::Expr::RecordAccess(ir::RecordAccessExpr {
                    bind: self.hole,
                    base: module_atom,
                    label: name,
                    next: self.next,
                }));
            self.next = access_expr;

            return ControlFlow::Continue(());
        }

        // Create atom
        let source_atom = match *self.resolved.meta(id) {
            ResolvedValue::Reference(sym) => ir::Atom::Symbol(ir::Symbol(sym.id())),
            ResolvedValue::Builtin(b) => ir::Atom::Builtin(b),
        }; // This is only defined if path is None so be careful about moving this
        let source_atom = self.builder.add(source_atom);

        if let Some(field_path) = field_path {
            let field_path = &field_path.get(tree).0;

            // Pre-allocate symbols for each field access
            let mut field_symbols = (0..field_path.len())
                .map(|_| self.next_symbol())
                .collect::<Vec<_>>();

            // The last field access should bind to self.hole
            if let Some(last) = field_symbols.last_mut() {
                *last = self.hole;
            }

            // Build the access chain from right to left (last field to first field)
            // This ensures proper execution order
            let mut continuation = self.next;

            for (i, field_id) in field_path.iter().enumerate().rev() {
                let field_label = field_id.get(tree).0;
                let bind_sym = field_symbols[i];

                // Determine the base atom for this access
                let base_atom = if i == 0 {
                    source_atom // First field accesses the original source
                } else {
                    self.builder.add(ir::Atom::Symbol(field_symbols[i - 1]))
                };

                // Create the record access expression
                let access_expr = self
                    .builder
                    .add(ir::Expr::RecordAccess(ir::RecordAccessExpr {
                        bind: bind_sym,
                        base: base_atom,
                        label: field_label,
                        next: continuation,
                    }));

                continuation = access_expr;
            }

            self.next = continuation;
        } else {
            // If there are no fields, we just return the source atom
            self.emit(source_atom);
        }

        ControlFlow::Continue(())
    }

    fn visit_tag_expr(
        &mut self,
        id: TreeId<node::TagExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let tag = id.get(tree).0.get(tree).0;
        let tag_atom = self.builder.add(ir::Atom::Tag(tag.into()));

        self.emit(tag_atom);
        ControlFlow::Continue(())
    }

    fn visit_symbol_expr(
        &mut self,
        id: TreeId<node::SymbolExpr>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let sym = *self.resolved.meta(id);

        let atom = self.builder.add(ir::Atom::Num(sym.id() as f64));
        self.emit(atom);

        ControlFlow::Continue(())
    }

    fn visit_type_rep_expr(
        &mut self,
        id: TreeId<node::TypeRepExpr>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let ty = self.typed.meta(id);
        let ty_proto = ty.to_protocol(self.str_interner);
        let ty_key = self.type_interner.intern(ty_proto);

        let atom = self.builder.add(ir::Atom::TypeRep(ir::TypeRep(ty_key)));
        self.emit(atom);

        ControlFlow::Continue(())
    }

    /// normalize(let x = e1 in e2, hole, ctx) =
    /// normalize(e1,x,normalize(e2,hole,ctx))
    fn visit_let_expr(
        &mut self,
        id: TreeId<node::LetExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr { value, body, .. } = *id.get(tree);

        self.visit_expr(body, tree)?;
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
        let node::LambdaExpr { body, .. } = *id.get(tree);

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

    fn visit_handle_expr(
        &mut self,
        id: TreeId<node::HandleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandleExpr { source, clauses } = id.get(tree);

        // Create a fresh symbol and atom for the source
        let source_sym = self.next_symbol();
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        let mut next = None;

        for &clause_id in clauses.iter().rev() {
            let node::HandlerClause { op, body, .. } = *clause_id.get(tree);

            // Get the operation name as StrKey
            let op_name = op.get(tree).0;

            // Get the parameter symbol
            let param_sym = self.symbol_of(clause_id);

            // Normalize the handler body with fresh context
            let body =
                self.with_fresh_context(tree, |normalizer, tree| normalizer.visit_expr(body, tree));

            // Create the handler clause
            let clause = self.builder.add(ir::HandlerClause {
                op: op_name,
                param: param_sym,
                body,
                next, // Link to the previous clause (building backwards)
            });

            next = Some(clause);
        }

        let clause = next.expect("HandleExpr should have at least one clause");

        // Create the handle expression context and set continuation
        let handle_expr = self.builder.add(ir::Expr::Handle(ir::HandleExpr {
            bind: self.hole,
            source: source_atom,
            clause,
            next: self.next,
        }));
        self.next = handle_expr;

        // Normalize in reverse order (CPS style)
        self.hole = source_sym;
        self.visit_expr(*source, tree)
    }

    fn visit_do_expr(
        &mut self,
        id: TreeId<node::DoExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::DoExpr { op, arg } = *id.get(tree);

        let op = op.get(tree).0;

        // Create fresh symbols and corresponding atoms
        let arg_sym = self.next_symbol();
        let arg_atom = self.builder.add(ir::Atom::Symbol(arg_sym));

        // Create the do expression context and set continuation
        let do_expr = self.builder.add(ir::Expr::Do(ir::DoExpr {
            bind: self.hole,
            op,
            arg: arg_atom,
            next: self.next,
        }));
        self.next = do_expr;

        // Normalize in reverse order (CPS style)
        self.hole = arg_sym;
        self.visit_expr(arg, tree)
    }

    fn visit_if_expr(
        &mut self,
        id: TreeId<node::IfExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::IfExpr {
            pred,
            then,
            or_else,
        } = *id.get(tree);

        // Create fresh symbol and corresponding atom
        let pred_sym = self.next_symbol();
        let pred_atom = self.builder.add(ir::Atom::Symbol(pred_sym));

        let then_expr = self.with_fresh_context(tree, |this, tree| this.visit_expr(then, tree));
        let or_else_expr =
            self.with_fresh_context(tree, |this, tree| this.visit_expr(or_else, tree));

        // Create the if expression context and set continuation
        let if_expr = self.builder.add(ir::Expr::If(ir::IfExpr {
            bind: self.hole,
            predicate: pred_atom,
            then: then_expr,
            or: or_else_expr,
            next: self.next,
        }));
        self.next = if_expr;

        // Normalize predicate
        self.hole = pred_sym;
        self.visit_expr(pred, tree)
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
        let node::BinaryExpr { lhs, op, rhs } = *id.get(tree);

        // Create fresh symbols and correpsonding atoms
        let lhs_sym = self.next_symbol();
        let rhs_sym = self.next_symbol();

        let lhs_atom = self.builder.add(ir::Atom::Symbol(lhs_sym));
        let rhs_atom = self.builder.add(ir::Atom::Symbol(rhs_sym));

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
            node::BinaryOp::Concat => ir::BinaryOp::Concat,
        };

        // Create the binary expression that will be the "context" for our normalizations
        let binary_expr = self.builder.add(ir::Expr::Binary(ir::BinaryExpr {
            bind: self.hole,
            op: binary_op,
            lhs: lhs_atom,
            rhs: rhs_atom,
            next: self.next,
        }));
        self.next = binary_expr;

        // Normalize in reverse order (CPS style):
        self.hole = rhs_sym;
        self.visit_expr(rhs, tree)?;

        self.hole = lhs_sym;
        self.visit_expr(lhs, tree)?;

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
                let label = field.label.get(tree).0;
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
            field_path,
            value,
            ..
        } = *id.get(tree);

        // Create fresh symbols and corresponding atoms
        let value_sym = self.next_symbol();
        let source_sym = self.next_symbol();

        let value_atom = self.builder.add(ir::Atom::Symbol(value_sym));
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        // Build FieldPath from AST field path
        let field_path = field_path.get(tree).0.as_slice();
        let path = self.build_field_path(field_path, tree);

        // Create the record extend expression context and set continuation
        let extend_expr = self
            .builder
            .add(ir::Expr::RecordExtend(ir::RecordExtendExpr {
                bind: self.hole,
                base: source_atom,
                path,
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
        let node::RecordRestrictExpr {
            source, field_path, ..
        } = *id.get(tree);

        // Create fresh symbol and corresponding atom
        let source_sym = self.next_symbol();
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        // Build FieldPath from AST field path
        let field_path = field_path.get(tree).0.as_slice();
        let path = self.build_field_path(field_path, tree);

        // Create the record restrict expression context and set continuation
        let restrict_expr = self
            .builder
            .add(ir::Expr::RecordRestrict(ir::RecordRestrictExpr {
                bind: self.hole,
                base: source_atom,
                path,
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
            field_path,
            op,
            value,
            ..
        } = *id.get(tree);

        // Create fresh symbols and corresponding atoms
        let value_sym = self.next_symbol();
        let source_sym = self.next_symbol();

        let value_atom = self.builder.add(ir::Atom::Symbol(value_sym));
        let source_atom = self.builder.add(ir::Atom::Symbol(source_sym));

        // Build FieldPath from AST field path
        let field_path = field_path.get(tree).0.as_slice();
        let path = self.build_field_path(field_path, tree);

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
                path,
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

    fn visit_record_merge_expr(
        &mut self,
        id: TreeId<node::RecordMergeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordMergeExpr { lhs, rhs } = *id.get(tree);

        // Create fresh symbols and correpsonding atoms
        let lhs_sym = self.next_symbol();
        let rhs_sym = self.next_symbol();

        let lhs_atom = self.builder.add(ir::Atom::Symbol(lhs_sym));
        let rhs_atom = self.builder.add(ir::Atom::Symbol(rhs_sym));

        // Create the binary expression that will be the "context" for our normalizations
        let binary_expr = self.builder.add(ir::Expr::Binary(ir::BinaryExpr {
            bind: self.hole,
            op: ir::BinaryOp::Merge,
            lhs: lhs_atom,
            rhs: rhs_atom,
            next: self.next,
        }));
        self.next = binary_expr;

        // Normalize in reverse order (CPS style):
        self.hole = rhs_sym;
        self.visit_expr(rhs, tree)?;

        self.hole = lhs_sym;
        self.visit_expr(lhs, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_case_expr(
        &mut self,
        id: TreeId<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseExpr { source, branches } = id.get(tree);

        // Save the current hole - this is where the case result should go
        let result_hole = self.hole;

        // Create symbol for the value being matched
        let source_sym = self.next_symbol();

        let mut on_failure = self
            .builder
            .add(ir::PatternMatcher::Failure(ir::PatternFailure));

        for branch_id in branches.iter().rev() {
            let node::CaseBranch { pat, body } = *branch_id.get(tree);

            // Save the current continuation
            let saved_next = self.next;

            // Set hole to result_hole for the branch body
            self.hole = result_hole;

            self.visit_expr(body, tree)?;
            let branch_next = self.next; // Capture the branch expression

            // Restore the continuation for the next iteration
            self.next = saved_next;

            let on_success = self
                .builder
                .add(ir::PatternMatcher::Success(ir::PatternSuccess {
                    next: branch_next, // Use the captured branch expression
                }));

            let mut pat_normalizer = PatternNormalizer::new(
                result_hole, // result binding
                source_sym,  // value being matched
                on_success,
                on_failure,
                self.resolved,
                self.builder,
            );
            pat.visit_by(&mut pat_normalizer, tree)?;

            on_failure = pat_normalizer.on_success; // This is now the root pattern matcher
        }

        // Create the case expression with the final pattern matcher
        let case_expr = ir::PatternMatchExpr {
            bind: result_hole, // Use the saved hole
            matcher: on_failure,
            next: self.next,
        };
        self.next = self.builder.add(ir::Expr::PatternMatch(case_expr));

        // Normalize the source (CPS style)
        self.hole = source_sym;
        self.visit_expr(*source, tree)?;

        ControlFlow::Continue(())
    }
}

#[derive(Debug)]
pub struct PatternNormalizer<'a> {
    hole: ir::Symbol,   // Where case result goes
    source: ir::Symbol, // What we're matching against
    on_success: InstrId<ir::PatternMatcher>,
    on_failure: InstrId<ir::PatternMatcher>,
    resolved: &'a ResolvedNodes,
    builder: &'a mut IrBuilder,
}

impl<'a> PatternNormalizer<'a> {
    pub fn new(
        hole: ir::Symbol,
        source: ir::Symbol,
        on_success: InstrId<ir::PatternMatcher>,
        on_failure: InstrId<ir::PatternMatcher>,
        resolved: &'a ResolvedNodes,
        builder: &'a mut IrBuilder,
    ) -> Self {
        Self {
            hole,
            source,
            on_success,
            on_failure,
            resolved,
            builder,
        }
    }

    pub fn next_symbol(&mut self) -> ir::Symbol {
        let sym = ValueSym::new();
        let symbol = ir::Symbol(sym.id());
        symbol
    }

    pub fn symbol_of<T, N>(&self, id: TreeId<T>) -> ir::Symbol
    where
        N: Namespace,
        T: MetaCast<ResolvePhase, Meta = Sym<N>>, // TODO maybe ValueSym ??
    {
        let sym = self.resolved.meta(id);
        ir::Symbol(sym.id())
    }
}

impl<'a, Tree> Visitor<Tree> for PatternNormalizer<'a>
where
    Tree: TreeView,
{
    type BreakValue = !;

    fn visit_any_pat(
        &mut self,
        _id: TreeId<node::AnyPat>,
        _tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        // No testing nor extraction needed
        // Not even a symbol needed so maybe just do nothing here ?

        ControlFlow::Continue(())
    }

    fn visit_bind_pat(
        &mut self,
        id: TreeId<node::BindPat>,
        _tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = self.symbol_of(id);

        // Here is the correct place to extract, probably not elsewhere
        self.on_success = self.builder.add(ir::PatternMatcher::Identity(ir::Identity {
            bind,
            source: self.source,
            next: self.on_success,
        }));

        ControlFlow::Continue(())
    }
    fn visit_literal_pat(
        &mut self,
        id: TreeId<node::LiteralPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        // I think I should actually only create ExtractIdentity in the bind pat
        // LiteralPat are only tests anyway and will never be doing any binding
        // self.on_success =
        //     self.builder
        //         .add(ir::PatternMatcher::ExtractIdentity(ir::ExtractIdentity {
        //             bind: self.hole,
        //             source: self.source,
        //             next: self.on_success,
        //         }));

        self.on_success = match *id.get(tree) {
            node::LiteralPat::Unit => self.builder.add(ir::PatternMatcher::IsUnit(ir::IsUnit {
                source: self.source,
                on_success: self.on_success,
                on_failure: self.on_failure,
            })),
            node::LiteralPat::Bool(payload) => {
                self.builder.add(ir::PatternMatcher::IsBool(ir::IsBool {
                    source: self.source,
                    payload,
                    on_success: self.on_success,
                    on_failure: self.on_failure,
                }))
            }
            node::LiteralPat::Num(payload) => {
                self.builder.add(ir::PatternMatcher::IsNum(ir::IsNum {
                    source: self.source,
                    payload,
                    on_success: self.on_success,
                    on_failure: self.on_failure,
                }))
            }
            node::LiteralPat::Char(payload) => {
                self.builder.add(ir::PatternMatcher::IsChar(ir::IsChar {
                    source: self.source,
                    payload,
                    on_success: self.on_success,
                    on_failure: self.on_failure,
                }))
            }
            node::LiteralPat::Str(payload) => {
                self.builder.add(ir::PatternMatcher::IsStr(ir::IsStr {
                    source: self.source,
                    payload,
                    on_success: self.on_success,
                    on_failure: self.on_failure,
                }))
            }
        };

        ControlFlow::Continue(())
    }

    fn visit_list_pat(
        &mut self,
        id: TreeId<node::ListPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let elements = &id.get(tree).0;

        let mut iter = elements.iter();
        let mut source = self.source;
        let mut spread_sym = None;

        struct El {
            head: ir::Symbol,
            tail: ir::Symbol,
            source: ir::Symbol,
            pat: TreeId<node::Pat>,
        }

        let mut heads = Vec::new();
        let mut tails = Vec::new();

        while let Some(el) = iter.next() {
            match *el.get(tree) {
                node::ListElPat::Pat(pat) => {
                    // Create a fresh symbols for this element
                    let head = self.next_symbol();
                    let tail_list = self.next_symbol();

                    let el = El {
                        head,
                        tail: tail_list,
                        source,
                        pat,
                    };

                    heads.push(el);
                    source = tail_list;
                }
                node::ListElPat::Spread(name) => {
                    if name.is_some() {
                        spread_sym = Some(self.symbol_of(*el));
                    } else {
                        spread_sym = Some(self.next_symbol());
                    }

                    break;
                }
            }
        }

        while let Some(el) = iter.next_back() {
            match *el.get(tree) {
                node::ListElPat::Pat(pat) => {
                    // Create a fresh symbols for this element
                    let tail = self.next_symbol();
                    let head_list = self.next_symbol();

                    let el = El {
                        head: head_list,
                        tail,
                        source,
                        pat,
                    };

                    tails.push(el);
                    source = head_list;
                }
                node::ListElPat::Spread(_) => {
                    // There should only be one spread,
                    // TODO handle gracefully somewhere else
                    panic!("Only one spread allowed in list patterns");
                }
            }
        }

        // Spread must be processed first as it happens last in execution
        if let Some(bind) = spread_sym {
            // Add extractor for the spread
            self.on_success = self.builder.add(ir::PatternMatcher::Identity(ir::Identity {
                bind,
                source,
                next: self.on_success,
            }));
        }

        for El {
            head,
            tail,
            source,
            pat,
        } in tails
        {
            // Create nested pattern normalizer for this element
            let mut nested_normalizer = PatternNormalizer::new(
                self.hole,
                tail,
                self.on_success,
                self.on_failure,
                self.resolved,
                self.builder,
            );

            // Visit the nested pattern
            pat.visit_by(&mut nested_normalizer, tree)?;
            let next = nested_normalizer.on_success;

            self.on_success =
                self.builder
                    .add(ir::PatternMatcher::ListSplitTail(ir::ListSplitTail {
                        head_list: head,
                        tail,
                        source,
                        next,
                    }));
        }

        // Add all head patterns in reverse order
        for El {
            head,
            tail,
            source,
            pat,
        } in heads.into_iter().rev()
        {
            // Create nested pattern normalizer for this element
            let mut nested_normalizer = PatternNormalizer::new(
                self.hole,
                head,
                self.on_success,
                self.on_failure,
                self.resolved,
                self.builder,
            );

            // Visit the nested pattern
            pat.visit_by(&mut nested_normalizer, tree)?;
            let next = nested_normalizer.on_success;

            self.on_success =
                self.builder
                    .add(ir::PatternMatcher::ListSplitHead(ir::ListSplitHead {
                        head,
                        tail_list: tail,
                        source,
                        next,
                    }));
        }

        let length = elements.len() as u32;

        // Determine what list tests we need and spread extraction
        if spread_sym.is_some() {
            // Has spread - need at least `regular_count` elements
            self.on_success =
                self.builder
                    .add(ir::PatternMatcher::ListIsAtLeast(ir::ListIsAtLeast {
                        source: self.source,
                        min_length: length - 1,
                        on_success: self.on_success,
                        on_failure: self.on_failure,
                    }));
        } else {
            // No spread - need exactly `regular_count` elements
            self.on_success = self
                .builder
                .add(ir::PatternMatcher::ListIsExact(ir::ListIsExact {
                    source: self.source,
                    length,
                    on_success: self.on_success,
                    on_failure: self.on_failure,
                }))
        }

        // Add IsList test
        self.on_success = self.builder.add(ir::PatternMatcher::IsList(ir::IsList {
            source: self.source,
            on_success: self.on_success,
            on_failure: self.on_failure,
        }));

        ControlFlow::Continue(())
    }

    // TODO this is a weird implementation maybe record patterns need a different design
    fn visit_record_pat(
        &mut self,
        id: TreeId<node::RecordPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordPat { fields, polymorph } = id.get(tree);

        // Build the chain backwards, starting from the final success
        let mut current_success = self.on_success;

        // Process fields in reverse order to build the chain backwards
        for &field_id in fields.iter().rev() {
            let node::RecordFieldPat { pat, field } = *field_id.get(tree);

            let field = field.get(tree).0;

            if let Some(pat_id) = pat {
                // Field has a pattern - need to extract and match
                let field_sym = self.next_symbol();

                // Create nested pattern normalizer for the field pattern
                let mut nested_normalizer = PatternNormalizer::new(
                    self.hole,
                    field_sym,
                    current_success,
                    self.on_failure,
                    self.resolved,
                    self.builder,
                );

                // Visit the nested pattern
                pat_id.visit_by(&mut nested_normalizer, tree)?;
                let next = nested_normalizer.on_success;

                // Create field extraction
                current_success =
                    self.builder
                        .add(ir::PatternMatcher::RecordGetAt(ir::RecordGetAt {
                            bind: field_sym,
                            source: self.source,
                            field,
                            next,
                        }));
            } else {
                // Field has no pattern - it's a binding like { a }
                let field_sym = self.symbol_of(field_id);

                // Create field extraction and binding
                current_success =
                    self.builder
                        .add(ir::PatternMatcher::RecordGetAt(ir::RecordGetAt {
                            bind: field_sym,
                            source: self.source,
                            field,
                            next: current_success,
                        }));
            }

            // TODO not needed ?
            // // Add field existence check
            // current_success =
            //     self.builder
            //         .add(ir::PatternMatcher::RecordHasField(ir::RecordHasField {
            //             source: self.source,
            //             field,
            //             on_success: current_success,
            //             on_failure: self.on_failure,
            //         }));
        }

        // Add IsRecord test (happens first)
        self.on_success = self.builder.add(ir::PatternMatcher::IsRecord(ir::IsRecord {
            source: self.source,
            on_success: current_success,
            on_failure: self.on_failure,
        }));

        ControlFlow::Continue(())
    }

    fn visit_variant_pat(
        &mut self,
        id: TreeId<node::VariantPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let variant_pat = id.get(tree);
        let tags = &variant_pat.0;

        if tags.is_empty() {
            // Empty variant pattern - shouldn't happen, but handle gracefully
            self.on_success = self
                .builder
                .add(ir::PatternMatcher::Failure(ir::PatternFailure));
            return ControlFlow::Continue(());
        }

        // For variant patterns, we need to try each tag in sequence
        // Build the chain from right to left (last tag to first tag)
        let mut current_failure = self.on_failure;

        for &tag_id in tags.iter().rev() {
            let tag_pat = tag_id.get(tree);
            let tag_name = tag_pat.tag.get(tree).0;

            let mut tag_success = self.on_success;

            if let Some(pat_id) = tag_pat.pat {
                let pat_sym = self.next_symbol();

                // Tag has a pattern - need to match against variant value
                // Note: The variant value extraction is handled implicitly by IsVariant
                // The pattern should match against the same source symbol
                let mut nested_normalizer = PatternNormalizer::new(
                    self.hole,
                    pat_sym, // Variant value is in here after VariantGet extraction
                    tag_success,
                    current_failure,
                    self.resolved,
                    self.builder,
                );

                // Visit the nested pattern
                pat_id.visit_by(&mut nested_normalizer, tree)?;
                tag_success = nested_normalizer.on_success;

                // Create the variant extraction
                tag_success = self
                    .builder
                    .add(ir::PatternMatcher::VariantGet(ir::VariantGet {
                        bind: pat_sym,
                        source: self.source,
                        next: tag_success,
                    }));
            }

            // Create variant tag test
            let variant_test = self
                .builder
                .add(ir::PatternMatcher::IsVariant(ir::IsVariant {
                    source: self.source,
                    tag: tag_name,
                    on_success: tag_success,
                    on_failure: current_failure,
                }));

            // This test becomes the failure continuation for the next test
            current_failure = variant_test;
        }

        // The first tag test becomes our success continuation
        self.on_success = current_failure;

        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use kola_ir::{
        instr::{self as ir, Symbol},
        ir::{Ir, IrBuilder},
    };
    use kola_protocol::TypeInterner;
    use kola_resolver::{
        phase::{ResolvedNodes, ResolvedValue},
        symbol::ValueSym,
    };
    use kola_tree::prelude::*;
    use kola_utils::interner::StrInterner;
    use kola_vm::{
        machine::{CekMachine, MachineContext},
        value::Value,
    };

    use super::Normalizer;

    // TODO this wont work for path expressions using bound symbols,
    fn mock_resolved(tree: &impl TreeView) -> ResolvedNodes {
        let mut resolved = ResolvedNodes::default();

        for query in tree.query3::<node::QualifiedExpr, node::LetExpr, node::LambdaExpr>() {
            match query {
                Query3::V0(id, _path) => {
                    resolved.insert_meta(id, ResolvedValue::Reference(ValueSym::new()))
                }
                Query3::V1(id, _let) => resolved.insert_meta(id, ValueSym::new()),
                Query3::V2(id, _lambda) => resolved.insert_meta(id, ValueSym::new()),
            }
        }

        resolved
    }

    fn normalize<T>(
        tree: TreeBuilder,
        root_id: Id<T>,
        resolved: &ResolvedNodes,
        str_interner: &StrInterner,
    ) -> Ir
    where
        Id<T>: Visitable<TreeBuilder>,
    {
        let mut type_interner = TypeInterner::new();
        let mut builder = IrBuilder::new();

        let mocked_typed = HashMap::new();

        let hole = Symbol(ValueSym::new().id());
        let arg = builder.add(ir::Atom::Symbol(hole));
        let next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

        let normalizer = Normalizer::new(
            root_id,
            next,
            hole,
            resolved,
            &mocked_typed,
            &mut builder,
            &mut type_interner,
            str_interner,
        );
        let root = normalizer.run(&tree);

        builder.finish(root)
    }

    fn new_machine(ir: Ir, interner: StrInterner) -> CekMachine {
        let context = MachineContext::new(ir, "/mocked/path", interner, TypeInterner::new());
        CekMachine::new(context)
    }

    #[test]
    fn literal() {
        let mut builder = TreeBuilder::new();
        let lit = builder.insert(node::LiteralExpr::Num(10.0));

        let resolved = mock_resolved(&builder);
        let ir = normalize(builder, lit, &resolved, &StrInterner::new());
        let mut machine = new_machine(ir, StrInterner::new());
        let value = machine.run().unwrap();

        assert_eq!(value, Value::Num(10.0))
    }

    #[test]
    fn qualified_expr_unbound() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        let x = interner.intern("x");

        // Build a simple path expression: x (no module path, just binding)
        let path_expr = node::QualifiedExpr::new_in(None, x, None, &mut builder);

        let resolved = mock_resolved(&builder);
        let ir = normalize(builder, path_expr, &resolved, &interner);
        let mut machine = new_machine(ir, interner);

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

        let inside = node::QualifiedExpr::new_in(None, x, None, &mut builder);
        resolved.insert_meta(inside, ResolvedValue::Reference(x_sym));

        let value = builder.insert(node::LiteralExpr::Num(42.0));
        let let_expr = node::LetExpr::new_in(x, None, value, inside, &mut builder);
        resolved.insert_meta(let_expr, x_sym);

        let ir = normalize(builder, let_expr, &resolved, &interner);
        let mut machine = new_machine(ir, interner);
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
        let lambda_body = node::QualifiedExpr::new_in(None, x, None, &mut builder);
        resolved.insert_meta(lambda_body, ResolvedValue::Reference(x_sym));

        // Create the lambda: \x => x
        let lambda_expr = node::LambdaExpr::new_in(x, None, lambda_body, &mut builder);
        resolved.insert_meta(lambda_expr, x_sym); // Lambda gets parameter symbol
        let lambda = builder.insert(node::Expr::Lambda(lambda_expr));

        // Create the argument: 42
        let arg_literal = builder.insert(node::LiteralExpr::Num(42.0));
        let arg = builder.insert(node::Expr::Literal(arg_literal));

        // Create the call: (\x => x) 42
        let call_expr = builder.insert(node::CallExpr { func: lambda, arg });

        let ir = normalize(builder, call_expr, &resolved, &interner);
        let mut machine = new_machine(ir, interner);
        let value = machine.run().unwrap();

        // Should evaluate to 42 (identity function returns its argument)
        assert_eq!(value, Value::Num(42.0));
    }

    #[test]
    fn record_expr() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();
        let resolved = ResolvedNodes::default();

        // Build: { a = 42, b = "hello" }

        let a = interner.intern("a");
        let b = interner.intern("b");

        // Create the field values
        let a_value = builder.insert(node::LiteralExpr::Num(42.0));
        let b_value = builder.insert(node::LiteralExpr::Str(interner.intern("hello")));

        // Create the record fields
        let a_field = node::RecordField::new_in(a, None, a_value, &mut builder);
        let b_field = node::RecordField::new_in(b, None, b_value, &mut builder);

        // Create the record expression
        let record_expr = node::RecordExpr::new_in([a_field, b_field], &mut builder);
        let record_id = builder.insert(node::Expr::Record(record_expr));

        let ir = normalize(builder, record_id, &resolved, &interner);
        let mut machine = new_machine(ir, interner);
        let value = machine.run().unwrap();

        // Should evaluate to a record with two fields
        assert!(value.is_record());
    }

    #[test]
    fn record_access_expr() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();
        let mut resolved = ResolvedNodes::default();

        // Build: let r = { a = { b = { c = "hello" } } } in r.a.b.c

        let r = interner.intern("r");
        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");

        let r_sym = ValueSym::new();

        // Create the innermost record: { c = "hello" }
        let c_value = builder.insert(node::LiteralExpr::Str(interner.intern("hello")));
        let c_field = node::RecordField::new_in(c, None, c_value, &mut builder);
        let c_record = node::RecordExpr::new_in([c_field], &mut builder);

        // Create the middle record: { b = { c = "hello" } }
        let b_field = node::RecordField::new_in(b, None, c_record, &mut builder);
        let b_record = node::RecordExpr::new_in([b_field], &mut builder);

        // Create the outer record: { a = { b = { c = "hello" } } }
        let a_field = node::RecordField::new_in(a, None, b_record, &mut builder);
        let record_expr = node::RecordExpr::new_in([a_field], &mut builder);

        // Create the let expression: let r = { a = { b = { c = "hello" } } } in r.a.b.c
        let field_path = node::FieldPath::new_in(
            [
                node::ValueName::new(a),
                node::ValueName::new(b),
                node::ValueName::new(c),
            ],
            &mut builder,
        );

        let qualified_expr = node::QualifiedExpr::new_in(None, r, Some(field_path), &mut builder);
        resolved.insert_meta(qualified_expr, ResolvedValue::Reference(r_sym));

        let let_expr = node::LetExpr::new_in(r, None, record_expr, qualified_expr, &mut builder);
        resolved.insert_meta(let_expr, r_sym);

        let ir = normalize(builder, let_expr, &resolved, &interner);
        let mut machine = new_machine(ir, interner);
        let value = machine.run().unwrap();

        // Should evaluate to "hello" (the value of r.a.b.c)
        assert_eq!(value, Value::Str("hello".to_owned()));
    }
}
