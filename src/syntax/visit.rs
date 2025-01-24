use std::ops::ControlFlow;

use crate::semantic::types::MonoType;

use super::tree;

// double dispatch
pub trait Visitable {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor;

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut;
}

impl Visitable for MonoType {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_ty(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_ty_mut(self)
    }
}

impl Visitable for tree::Name {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_name(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_name_mut(self)
    }
}

impl Visitable for tree::ExprError {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_error(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_error_mut(self)
    }
}

impl Visitable for tree::Ident {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_ident(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_ident_mut(self)
    }
}

impl Visitable for tree::Literal {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_literal(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_literal_mut(self)
    }
}

impl Visitable for tree::List {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_list(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_list_mut(self)
    }
}

impl Visitable for tree::Property {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_property(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_property_mut(self)
    }
}

impl Visitable for tree::Record {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_record(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_record_mut(self)
    }
}

impl Visitable for tree::RecordSelect {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_record_select(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_record_select_mut(self)
    }
}

impl Visitable for tree::RecordExtend {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_record_extend(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_record_extend_mut(self)
    }
}

impl Visitable for tree::RecordRestrict {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_record_restrict(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_record_restrict_mut(self)
    }
}

impl Visitable for tree::RecordUpdate {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_record_update(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_record_update_mut(self)
    }
}

impl Visitable for tree::UnaryOp {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_unary_op(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_unary_op_mut(self)
    }
}

impl Visitable for tree::Unary {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_unary(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_unary_mut(self)
    }
}

impl Visitable for tree::BinaryOp {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_binary_op(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_binary_op_mut(self)
    }
}

impl Visitable for tree::Binary {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_binary(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_binary_mut(self)
    }
}

impl Visitable for tree::Let {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_let(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_let_mut(self)
    }
}

impl Visitable for tree::If {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_if(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_if_mut(self)
    }
}

impl Visitable for tree::Case {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_case(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_case_mut(self)
    }
}

impl Visitable for tree::Call {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_call(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_call_mut(self)
    }
}

// TODO more Visitable implementations

impl Visitable for tree::Expr {
    fn visit_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: Visitor,
    {
        visitor.visit_expr(self)
    }

    fn visit_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: VisitorMut,
    {
        visitor.visit_expr_mut(self)
    }
}

pub trait Visitor: Sized {
    type BreakValue;

    fn visit_ty(&mut self, ty: &MonoType) -> ControlFlow<Self::BreakValue> {
        walk_ty(self, ty)
    }

    fn visit_name(&mut self, name: &tree::Name) -> ControlFlow<Self::BreakValue> {
        walk_name(self, name)
    }

    fn visit_error(&mut self, error: &tree::ExprError) -> ControlFlow<Self::BreakValue> {
        walk_error(self, error)
    }

    fn visit_ident(&mut self, ident: &tree::Ident) -> ControlFlow<Self::BreakValue> {
        walk_ident(self, ident)
    }

    fn visit_literal(&mut self, literal: &tree::Literal) -> ControlFlow<Self::BreakValue> {
        walk_literal(self, literal)
    }

    fn visit_list(&mut self, list: &tree::List) -> ControlFlow<Self::BreakValue> {
        walk_list(self, list)
    }

    fn visit_property(&mut self, property: &tree::Property) -> ControlFlow<Self::BreakValue> {
        walk_property(self, property)
    }

    fn visit_record(&mut self, record: &tree::Record) -> ControlFlow<Self::BreakValue> {
        walk_record(self, record)
    }

    fn visit_record_select(
        &mut self,
        select: &tree::RecordSelect,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_select(self, select)
    }

    fn visit_record_extend(
        &mut self,
        extend: &tree::RecordExtend,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_extend(self, extend)
    }

    fn visit_record_restrict(
        &mut self,
        restrict: &tree::RecordRestrict,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_restrict(self, restrict)
    }

    fn visit_record_update(
        &mut self,
        update: &tree::RecordUpdate,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_update(self, update)
    }

    fn visit_unary_op(&mut self, unary_op: &tree::UnaryOp) -> ControlFlow<Self::BreakValue> {
        walk_unary_op(self, unary_op)
    }

    fn visit_unary(&mut self, unary: &tree::Unary) -> ControlFlow<Self::BreakValue> {
        walk_unary(self, unary)
    }

    fn visit_binary_op(&mut self, binary_op: &tree::BinaryOp) -> ControlFlow<Self::BreakValue> {
        walk_binary_op(self, binary_op)
    }

    fn visit_binary(&mut self, binary: &tree::Binary) -> ControlFlow<Self::BreakValue> {
        walk_binary(self, binary)
    }

    fn visit_let(&mut self, let_: &tree::Let) -> ControlFlow<Self::BreakValue> {
        walk_let(self, let_)
    }

    fn visit_if(&mut self, if_: &tree::If) -> ControlFlow<Self::BreakValue> {
        walk_if(self, if_)
    }

    fn visit_pat(&mut self, pat: &tree::Pat) -> ControlFlow<Self::BreakValue> {
        walk_pat(self, pat)
    }

    fn visit_branch(&mut self, branch: &tree::Branch) -> ControlFlow<Self::BreakValue> {
        walk_branch(self, branch)
    }

    fn visit_case(&mut self, case: &tree::Case) -> ControlFlow<Self::BreakValue> {
        walk_case(self, case)
    }

    fn visit_func(&mut self, func: &tree::Func) -> ControlFlow<Self::BreakValue> {
        walk_func(self, func)
    }

    fn visit_call(&mut self, call: &tree::Call) -> ControlFlow<Self::BreakValue> {
        walk_call(self, call)
    }

    fn visit_expr(&mut self, expr: &tree::Expr) -> ControlFlow<Self::BreakValue> {
        walk_expr(self, expr)
    }
}

pub fn walk_ty<V>(_visitor: &mut V, _ty: &MonoType) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    ControlFlow::Continue(())
}

pub fn walk_name<V>(_visitor: &mut V, _name: &tree::Name) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    ControlFlow::Continue(())
}

pub fn walk_error<V>(_visitor: &mut V, _error: &tree::ExprError) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    ControlFlow::Continue(())
}

pub fn walk_ident<V>(visitor: &mut V, ident: &tree::Ident) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ty(ident.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_literal<V>(visitor: &mut V, literal: &tree::Literal) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ty(literal.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_list<V>(visitor: &mut V, list: &tree::List) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    for value in &list.values {
        visitor.visit_expr(value)?;
    }
    visitor.visit_ty(list.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_property<V>(visitor: &mut V, property: &tree::Property) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_name(&property.key)?;
    visitor.visit_expr(&property.value)?;
    ControlFlow::Continue(())
}

pub fn walk_record<V>(visitor: &mut V, record: &tree::Record) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    for property in &record.fields {
        visitor.visit_property(property)?;
    }
    visitor.visit_ty(record.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_record_select<V>(
    visitor: &mut V,
    select: &tree::RecordSelect,
) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&select.source)?;
    visitor.visit_name(&select.field)?;
    visitor.visit_ty(select.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_record_extend<V>(
    visitor: &mut V,
    extend: &tree::RecordExtend,
) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&extend.source)?;
    visitor.visit_name(&extend.field)?;
    visitor.visit_expr(&extend.value)?;
    visitor.visit_ty(extend.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_record_restrict<V>(
    visitor: &mut V,
    restrict: &tree::RecordRestrict,
) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&restrict.source)?;
    visitor.visit_name(&restrict.field)?;
    visitor.visit_ty(restrict.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_record_update<V>(
    visitor: &mut V,
    update: &tree::RecordUpdate,
) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&update.source)?;
    visitor.visit_name(&update.field)?;
    visitor.visit_expr(&update.value)?;
    visitor.visit_ty(update.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_unary_op<V>(visitor: &mut V, unary_op: &tree::UnaryOp) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ty(unary_op.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_unary<V>(visitor: &mut V, unary: &tree::Unary) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_unary_op(&unary.op)?;
    visitor.visit_expr(&unary.target)?;
    visitor.visit_ty(unary.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_binary_op<V>(visitor: &mut V, binary_op: &tree::BinaryOp) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ty(binary_op.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_binary<V>(visitor: &mut V, binary: &tree::Binary) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_binary_op(&binary.op)?;
    visitor.visit_expr(&binary.left)?;
    visitor.visit_expr(&binary.right)?;
    visitor.visit_ty(binary.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_let<V>(visitor: &mut V, let_: &tree::Let) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_name(&let_.name)?;
    visitor.visit_expr(&let_.value)?;
    visitor.visit_expr(&let_.inside)?;
    visitor.visit_ty(let_.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_if<V>(visitor: &mut V, if_: &tree::If) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&if_.predicate)?;
    visitor.visit_expr(&if_.then)?;
    visitor.visit_expr(&if_.or)?;
    visitor.visit_ty(if_.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_pat<V>(visitor: &mut V, pat: &tree::Pat) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    todo!()
}

pub fn walk_branch<V>(visitor: &mut V, branch: &tree::Branch) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_pat(&branch.pat)?;
    visitor.visit_expr(&branch.matches)?;
    ControlFlow::Continue(())
}

pub fn walk_case<V>(visitor: &mut V, case: &tree::Case) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ident(&case.source)?;
    for branch in &case.branches {
        visitor.visit_branch(branch)?;
    }
    visitor.visit_ty(case.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_func<V>(visitor: &mut V, func: &tree::Func) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_ident(&func.param)?;
    visitor.visit_expr(&func.body)?;
    visitor.visit_ty(func.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_call<V>(visitor: &mut V, call: &tree::Call) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    visitor.visit_expr(&call.func)?;
    visitor.visit_expr(&call.arg)?;
    visitor.visit_ty(call.ty())?;
    ControlFlow::Continue(())
}

pub fn walk_expr<V>(visitor: &mut V, expr: &tree::Expr) -> ControlFlow<V::BreakValue>
where
    V: Visitor,
{
    match expr {
        tree::Expr::Error(e) => visitor.visit_error(e),
        tree::Expr::Literal(l) => visitor.visit_literal(l),
        tree::Expr::Ident(i) => visitor.visit_ident(i),
        tree::Expr::List(l) => visitor.visit_list(l),
        tree::Expr::Record(r) => visitor.visit_record(r),
        tree::Expr::RecordSelect(r) => visitor.visit_record_select(r),
        tree::Expr::RecordExtend(r) => visitor.visit_record_extend(r),
        tree::Expr::RecordRestrict(r) => visitor.visit_record_restrict(r),
        tree::Expr::RecordUpdate(r) => visitor.visit_record_update(r),
        tree::Expr::Unary(u) => visitor.visit_unary(u),
        tree::Expr::Binary(b) => visitor.visit_binary(b),
        tree::Expr::Let(l) => visitor.visit_let(l),
        tree::Expr::If(i) => visitor.visit_if(i),
        tree::Expr::Case(c) => visitor.visit_case(c),
        tree::Expr::Func(f) => visitor.visit_func(f),
        tree::Expr::Call(c) => visitor.visit_call(c),
    }
}

pub trait VisitorMut: Sized {
    type BreakValue;

    fn visit_ty_mut(&mut self, ty: &mut MonoType) -> ControlFlow<Self::BreakValue> {
        walk_ty_mut(self, ty)
    }

    fn visit_name_mut(&mut self, name: &mut tree::Name) -> ControlFlow<Self::BreakValue> {
        walk_name_mut(self, name)
    }

    fn visit_error_mut(&mut self, error: &mut tree::ExprError) -> ControlFlow<Self::BreakValue> {
        walk_error_mut(self, error)
    }

    fn visit_ident_mut(&mut self, ident: &mut tree::Ident) -> ControlFlow<Self::BreakValue> {
        walk_ident_mut(self, ident)
    }

    fn visit_literal_mut(&mut self, literal: &mut tree::Literal) -> ControlFlow<Self::BreakValue> {
        walk_literal_mut(self, literal)
    }

    fn visit_list_mut(&mut self, list: &mut tree::List) -> ControlFlow<Self::BreakValue> {
        walk_list_mut(self, list)
    }

    fn visit_property_mut(
        &mut self,
        property: &mut tree::Property,
    ) -> ControlFlow<Self::BreakValue> {
        walk_property_mut(self, property)
    }

    fn visit_record_mut(&mut self, record: &mut tree::Record) -> ControlFlow<Self::BreakValue> {
        walk_record_mut(self, record)
    }

    fn visit_record_select_mut(
        &mut self,
        select: &mut tree::RecordSelect,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_select_mut(self, select)
    }

    fn visit_record_extend_mut(
        &mut self,
        extend: &mut tree::RecordExtend,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_extend_mut(self, extend)
    }

    fn visit_record_restrict_mut(
        &mut self,
        restrict: &mut tree::RecordRestrict,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_restrict_mut(self, restrict)
    }

    fn visit_record_update_mut(
        &mut self,
        update: &mut tree::RecordUpdate,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_update_mut(self, update)
    }

    fn visit_unary_op_mut(
        &mut self,
        unary_op: &mut tree::UnaryOp,
    ) -> ControlFlow<Self::BreakValue> {
        walk_unary_op_mut(self, unary_op)
    }

    fn visit_unary_mut(&mut self, unary: &mut tree::Unary) -> ControlFlow<Self::BreakValue> {
        walk_unary_mut(self, unary)
    }

    fn visit_binary_op_mut(
        &mut self,
        binary_op: &mut tree::BinaryOp,
    ) -> ControlFlow<Self::BreakValue> {
        walk_binary_op_mut(self, binary_op)
    }

    fn visit_binary_mut(&mut self, binary: &mut tree::Binary) -> ControlFlow<Self::BreakValue> {
        walk_binary_mut(self, binary)
    }

    fn visit_let_mut(&mut self, let_: &mut tree::Let) -> ControlFlow<Self::BreakValue> {
        walk_let_mut(self, let_)
    }

    fn visit_if_mut(&mut self, if_: &mut tree::If) -> ControlFlow<Self::BreakValue> {
        walk_if_mut(self, if_)
    }

    fn visit_pat_mut(&mut self, pat: &mut tree::Pat) -> ControlFlow<Self::BreakValue> {
        walk_pat_mut(self, pat)
    }

    fn visit_branch_mut(&mut self, branch: &mut tree::Branch) -> ControlFlow<Self::BreakValue> {
        walk_branch_mut(self, branch)
    }

    fn visit_case_mut(&mut self, case: &mut tree::Case) -> ControlFlow<Self::BreakValue> {
        walk_case_mut(self, case)
    }

    fn visit_func_mut(&mut self, func: &mut tree::Func) -> ControlFlow<Self::BreakValue> {
        walk_func_mut(self, func)
    }

    fn visit_call_mut(&mut self, call: &mut tree::Call) -> ControlFlow<Self::BreakValue> {
        walk_call_mut(self, call)
    }

    fn visit_expr_mut(&mut self, expr: &mut tree::Expr) -> ControlFlow<Self::BreakValue> {
        walk_expr_mut(self, expr)
    }
}

pub fn walk_ty_mut<V>(_visitor: &mut V, _ty: &mut MonoType) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    ControlFlow::Continue(())
}

pub fn walk_name_mut<V>(_visitor: &mut V, _name: &mut tree::Name) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    ControlFlow::Continue(())
}

pub fn walk_error_mut<V>(
    _visitor: &mut V,
    _error: &mut tree::ExprError,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    ControlFlow::Continue(())
}

pub fn walk_ident_mut<V>(visitor: &mut V, ident: &mut tree::Ident) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ty_mut(ident.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_literal_mut<V>(
    visitor: &mut V,
    literal: &mut tree::Literal,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ty_mut(literal.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_list_mut<V>(visitor: &mut V, list: &mut tree::List) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    for value in &mut list.values {
        visitor.visit_expr_mut(value)?;
    }
    visitor.visit_ty_mut(list.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_property_mut<V>(
    visitor: &mut V,
    property: &mut tree::Property,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_name_mut(&mut property.key)?;
    visitor.visit_expr_mut(&mut property.value)?;
    ControlFlow::Continue(())
}

pub fn walk_record_mut<V>(visitor: &mut V, record: &mut tree::Record) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    for property in &mut record.fields {
        visitor.visit_property_mut(property)?;
    }
    visitor.visit_ty_mut(record.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_record_select_mut<V>(
    visitor: &mut V,
    select: &mut tree::RecordSelect,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut select.source)?;
    visitor.visit_name_mut(&mut select.field)?;
    visitor.visit_ty_mut(select.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_record_extend_mut<V>(
    visitor: &mut V,
    extend: &mut tree::RecordExtend,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut extend.source)?;
    visitor.visit_name_mut(&mut extend.field)?;
    visitor.visit_expr_mut(&mut extend.value)?;
    visitor.visit_ty_mut(extend.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_record_restrict_mut<V>(
    visitor: &mut V,
    restrict: &mut tree::RecordRestrict,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut restrict.source)?;
    visitor.visit_name_mut(&mut restrict.field)?;
    visitor.visit_ty_mut(restrict.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_record_update_mut<V>(
    visitor: &mut V,
    update: &mut tree::RecordUpdate,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut update.source)?;
    visitor.visit_name_mut(&mut update.field)?;
    visitor.visit_expr_mut(&mut update.value)?;
    visitor.visit_ty_mut(update.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_unary_op_mut<V>(
    visitor: &mut V,
    unary_op: &mut tree::UnaryOp,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ty_mut(unary_op.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_unary_mut<V>(visitor: &mut V, unary: &mut tree::Unary) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_unary_op_mut(&mut unary.op)?;
    visitor.visit_expr_mut(&mut unary.target)?;
    visitor.visit_ty_mut(unary.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_binary_op_mut<V>(
    visitor: &mut V,
    binary_op: &mut tree::BinaryOp,
) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ty_mut(binary_op.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_binary_mut<V>(visitor: &mut V, binary: &mut tree::Binary) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_binary_op_mut(&mut binary.op)?;
    visitor.visit_expr_mut(&mut binary.left)?;
    visitor.visit_expr_mut(&mut binary.right)?;
    visitor.visit_ty_mut(binary.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_let_mut<V>(visitor: &mut V, let_: &mut tree::Let) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_name_mut(&mut let_.name)?;
    visitor.visit_expr_mut(&mut let_.value)?;
    visitor.visit_expr_mut(&mut let_.inside)?;
    visitor.visit_ty_mut(let_.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_if_mut<V>(visitor: &mut V, if_: &mut tree::If) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut if_.predicate)?;
    visitor.visit_expr_mut(&mut if_.then)?;
    visitor.visit_expr_mut(&mut if_.or)?;
    visitor.visit_ty_mut(if_.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_pat_mut<V>(visitor: &mut V, pat: &mut tree::Pat) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    todo!()
}

pub fn walk_branch_mut<V>(visitor: &mut V, branch: &mut tree::Branch) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_pat_mut(&mut branch.pat)?;
    visitor.visit_expr_mut(&mut branch.matches)?;
    ControlFlow::Continue(())
}

pub fn walk_case_mut<V>(visitor: &mut V, case: &mut tree::Case) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ident_mut(&mut case.source)?;
    for branch in &mut case.branches {
        visitor.visit_branch_mut(branch)?;
    }
    visitor.visit_ty_mut(case.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_func_mut<V>(visitor: &mut V, func: &mut tree::Func) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_ident_mut(&mut func.param)?;
    visitor.visit_expr_mut(&mut func.body)?;
    visitor.visit_ty_mut(func.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_call_mut<V>(visitor: &mut V, call: &mut tree::Call) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    visitor.visit_expr_mut(&mut call.func)?;
    visitor.visit_expr_mut(&mut call.arg)?;
    visitor.visit_ty_mut(call.ty_mut())?;
    ControlFlow::Continue(())
}

pub fn walk_expr_mut<V>(visitor: &mut V, expr: &mut tree::Expr) -> ControlFlow<V::BreakValue>
where
    V: VisitorMut,
{
    match expr {
        tree::Expr::Error(e) => visitor.visit_error_mut(e),
        tree::Expr::Literal(l) => visitor.visit_literal_mut(l),
        tree::Expr::Ident(i) => visitor.visit_ident_mut(i),
        tree::Expr::List(l) => visitor.visit_list_mut(l),
        tree::Expr::Record(r) => visitor.visit_record_mut(r),
        tree::Expr::RecordSelect(r) => visitor.visit_record_select_mut(r),
        tree::Expr::RecordExtend(r) => visitor.visit_record_extend_mut(r),
        tree::Expr::RecordRestrict(r) => visitor.visit_record_restrict_mut(r),
        tree::Expr::RecordUpdate(r) => visitor.visit_record_update_mut(r),
        tree::Expr::Unary(u) => visitor.visit_unary_mut(u),
        tree::Expr::Binary(b) => visitor.visit_binary_mut(b),
        tree::Expr::Let(l) => visitor.visit_let_mut(l),
        tree::Expr::If(i) => visitor.visit_if_mut(i),
        tree::Expr::Case(c) => visitor.visit_case_mut(c),
        tree::Expr::Func(f) => visitor.visit_func_mut(f),
        tree::Expr::Call(c) => visitor.visit_call_mut(c),
    }
}
