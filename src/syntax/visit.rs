use std::ops::ControlFlow;

use crate::semantic::types::MonoType;

use super::ast;

pub trait Visit: Sized {
    type BreakValue;

    fn visit_ty(&mut self, ty: &MonoType) -> ControlFlow<Self::BreakValue> {
        walk_ty(self, ty)
    }

    fn visit_name(&mut self, name: &ast::Name) -> ControlFlow<Self::BreakValue> {
        walk_name(self, name)
    }

    fn visit_error(&mut self, error: &ast::ExprError) -> ControlFlow<Self::BreakValue> {
        walk_error(self, error)
    }

    fn visit_ident(&mut self, ident: &ast::IdentExpr) -> ControlFlow<Self::BreakValue> {
        walk_ident(self, ident)
    }

    fn visit_literal(&mut self, literal: &ast::LiteralExpr) -> ControlFlow<Self::BreakValue> {
        walk_literal(self, literal)
    }

    fn visit_list(&mut self, list: &ast::ListExpr) -> ControlFlow<Self::BreakValue> {
        walk_list(self, list)
    }

    fn visit_property(&mut self, property: &ast::Property) -> ControlFlow<Self::BreakValue> {
        walk_property(self, property)
    }

    fn visit_record(&mut self, record: &ast::RecordExpr) -> ControlFlow<Self::BreakValue> {
        walk_record(self, record)
    }

    fn visit_record_select(
        &mut self,
        record_select: &ast::RecordSelectExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_select(self, record_select)
    }

    fn visit_record_extend(
        &mut self,
        record_extend: &ast::RecordExtendExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_extend(self, record_extend)
    }

    fn visit_record_restrict(
        &mut self,
        record_restrict: &ast::RecordRestrictExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_restrict(self, record_restrict)
    }

    fn visit_record_update(
        &mut self,
        record_update: &ast::RecordUpdateExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_update(self, record_update)
    }

    fn visit_unary_op(&mut self, unary_op: &ast::UnaryOp) -> ControlFlow<Self::BreakValue> {
        walk_unary_op(self, unary_op)
    }

    fn visit_unary(&mut self, unary: &ast::UnaryExpr) -> ControlFlow<Self::BreakValue> {
        walk_unary(self, unary)
    }

    fn visit_binary_op(&mut self, binary_op: &ast::BinaryOp) -> ControlFlow<Self::BreakValue> {
        walk_binary_op(self, binary_op)
    }

    fn visit_binary(&mut self, binary: &ast::BinaryExpr) -> ControlFlow<Self::BreakValue> {
        walk_binary(self, binary)
    }

    fn visit_let(&mut self, let_: &ast::LetExpr) -> ControlFlow<Self::BreakValue> {
        walk_let(self, let_)
    }

    fn visit_if(&mut self, if_: &ast::IfExpr) -> ControlFlow<Self::BreakValue> {
        walk_if(self, if_)
    }

    fn visit_pat(&mut self, pat: &ast::Pat) -> ControlFlow<Self::BreakValue> {
        walk_pat(self, pat)
    }

    fn visit_branch(&mut self, branch: &ast::Branch) -> ControlFlow<Self::BreakValue> {
        walk_branch(self, branch)
    }

    fn visit_case(&mut self, case: &ast::CaseExpr) -> ControlFlow<Self::BreakValue> {
        walk_case(self, case)
    }

    fn visit_func(&mut self, func: &ast::FuncExpr) -> ControlFlow<Self::BreakValue> {
        walk_func(self, func)
    }

    fn visit_call(&mut self, call: &ast::CallExpr) -> ControlFlow<Self::BreakValue> {
        walk_call(self, call)
    }

    fn visit_expr(&mut self, expr: &ast::Expr) -> ControlFlow<Self::BreakValue> {
        walk_expr(self, expr)
    }
}

pub fn walk_ty<V>(_visitor: &mut V, _ty: &MonoType) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    ControlFlow::Continue(())
}

pub fn walk_name<V>(_visitor: &mut V, _name: &ast::Name) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    ControlFlow::Continue(())
}

pub fn walk_error<V>(_visitor: &mut V, _error: &ast::ExprError) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    ControlFlow::Continue(())
}

pub fn walk_ident<V>(visitor: &mut V, ident: &ast::IdentExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ty(&ident.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_literal<V>(visitor: &mut V, literal: &ast::LiteralExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ty(&literal.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_list<V>(visitor: &mut V, list: &ast::ListExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    for value in &list.values {
        visitor.visit_expr(value)?;
    }
    visitor.visit_ty(&list.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_property<V>(visitor: &mut V, property: &ast::Property) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_name(&property.key)?;
    visitor.visit_expr(&property.value)?;
    ControlFlow::Continue(())
}

pub fn walk_record<V>(visitor: &mut V, record: &ast::RecordExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    for property in &record.fields {
        visitor.visit_property(property)?;
    }
    visitor.visit_ty(&record.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_select<V>(
    visitor: &mut V,
    record_select: &ast::RecordSelectExpr,
) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_expr(&record_select.source)?;
    visitor.visit_name(&record_select.field)?;
    visitor.visit_ty(&record_select.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_extend<V>(
    visitor: &mut V,
    record_extend: &ast::RecordExtendExpr,
) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_expr(&record_extend.source)?;
    visitor.visit_name(&record_extend.field)?;
    visitor.visit_expr(&record_extend.value)?;
    visitor.visit_ty(&record_extend.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_restrict<V>(
    visitor: &mut V,
    record_restrict: &ast::RecordRestrictExpr,
) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_expr(&record_restrict.source)?;
    visitor.visit_name(&record_restrict.field)?;
    visitor.visit_ty(&record_restrict.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_update<V>(
    visitor: &mut V,
    record_update: &ast::RecordUpdateExpr,
) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_expr(&record_update.source)?;
    visitor.visit_name(&record_update.field)?;
    visitor.visit_expr(&record_update.value)?;
    visitor.visit_ty(&record_update.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_unary_op<V>(visitor: &mut V, unary_op: &ast::UnaryOp) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ty(&unary_op.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_unary<V>(visitor: &mut V, unary: &ast::UnaryExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_unary_op(&unary.op)?;
    visitor.visit_expr(&unary.target)?;
    visitor.visit_ty(&unary.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_binary_op<V>(visitor: &mut V, binary_op: &ast::BinaryOp) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ty(&binary_op.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_binary<V>(visitor: &mut V, binary: &ast::BinaryExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_binary_op(&binary.op)?;
    visitor.visit_expr(&binary.left)?;
    visitor.visit_expr(&binary.right)?;
    visitor.visit_ty(&binary.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_let<V>(visitor: &mut V, let_: &ast::LetExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_name(&let_.name)?;
    visitor.visit_expr(&let_.value)?;
    visitor.visit_expr(&let_.inside)?;
    visitor.visit_ty(&let_.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_if<V>(visitor: &mut V, if_: &ast::IfExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_expr(&if_.predicate)?;
    visitor.visit_expr(&if_.then)?;
    visitor.visit_expr(&if_.or)?;
    visitor.visit_ty(&if_.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_pat<V>(visitor: &mut V, pat: &ast::Pat) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    todo!()
}

pub fn walk_branch<V>(visitor: &mut V, branch: &ast::Branch) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_pat(&branch.pat)?;
    visitor.visit_expr(&branch.matches)?;
    ControlFlow::Continue(())
}

pub fn walk_case<V>(visitor: &mut V, case: &ast::CaseExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ident(&case.source)?;
    for branch in &case.branches {
        visitor.visit_branch(branch)?;
    }
    visitor.visit_ty(&case.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_func<V>(visitor: &mut V, func: &ast::FuncExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ident(&func.param)?;
    visitor.visit_expr(&func.body)?;
    visitor.visit_ty(&func.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_call<V>(visitor: &mut V, call: &ast::CallExpr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    visitor.visit_ident(&call.func)?;
    visitor.visit_expr(&call.arg)?;
    visitor.visit_ty(&call.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_expr<V>(visitor: &mut V, expr: &ast::Expr) -> ControlFlow<V::BreakValue>
where
    V: Visit,
{
    match expr {
        ast::Expr::Error(e) => visitor.visit_error(e),
        ast::Expr::Literal(l) => visitor.visit_literal(l),
        ast::Expr::Ident(i) => visitor.visit_ident(i),
        ast::Expr::List(l) => visitor.visit_list(l),
        ast::Expr::Record(r) => visitor.visit_record(r),
        ast::Expr::RecordSelect(r) => visitor.visit_record_select(r),
        ast::Expr::RecordExtend(r) => visitor.visit_record_extend(r),
        ast::Expr::RecordRestrict(r) => visitor.visit_record_restrict(r),
        ast::Expr::RecordUpdate(r) => visitor.visit_record_update(r),
        ast::Expr::Unary(u) => visitor.visit_unary(u),
        ast::Expr::Binary(b) => visitor.visit_binary(b),
        ast::Expr::Let(l) => visitor.visit_let(l),
        ast::Expr::If(i) => visitor.visit_if(i),
        ast::Expr::Case(c) => visitor.visit_case(c),
        ast::Expr::Func(f) => visitor.visit_func(f),
        ast::Expr::Call(c) => visitor.visit_call(c),
    }
}

pub trait VisitMut: Sized {
    type BreakValue;

    fn visit_ty_mut(&mut self, ty: &mut MonoType) -> ControlFlow<Self::BreakValue> {
        walk_ty_mut(self, ty)
    }

    fn visit_name_mut(&mut self, name: &mut ast::Name) -> ControlFlow<Self::BreakValue> {
        walk_name_mut(self, name)
    }

    fn visit_error_mut(&mut self, error: &mut ast::ExprError) -> ControlFlow<Self::BreakValue> {
        walk_error_mut(self, error)
    }

    fn visit_ident_mut(&mut self, ident: &mut ast::IdentExpr) -> ControlFlow<Self::BreakValue> {
        walk_ident_mut(self, ident)
    }

    fn visit_literal_mut(
        &mut self,
        literal: &mut ast::LiteralExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_literal_mut(self, literal)
    }

    fn visit_list_mut(&mut self, list: &mut ast::ListExpr) -> ControlFlow<Self::BreakValue> {
        walk_list_mut(self, list)
    }

    fn visit_property_mut(
        &mut self,
        property: &mut ast::Property,
    ) -> ControlFlow<Self::BreakValue> {
        walk_property_mut(self, property)
    }

    fn visit_record_mut(&mut self, record: &mut ast::RecordExpr) -> ControlFlow<Self::BreakValue> {
        walk_record_mut(self, record)
    }

    fn visit_record_select_mut(
        &mut self,
        record_select: &mut ast::RecordSelectExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_select_mut(self, record_select)
    }

    fn visit_record_extend_mut(
        &mut self,
        record_extend: &mut ast::RecordExtendExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_extend_mut(self, record_extend)
    }

    fn visit_record_restrict_mut(
        &mut self,
        record_restrict: &mut ast::RecordRestrictExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_restrict_mut(self, record_restrict)
    }

    fn visit_record_update_mut(
        &mut self,
        record_update: &mut ast::RecordUpdateExpr,
    ) -> ControlFlow<Self::BreakValue> {
        walk_record_update_mut(self, record_update)
    }

    fn visit_unary_op_mut(&mut self, unary_op: &mut ast::UnaryOp) -> ControlFlow<Self::BreakValue> {
        walk_unary_op_mut(self, unary_op)
    }

    fn visit_unary_mut(&mut self, unary: &mut ast::UnaryExpr) -> ControlFlow<Self::BreakValue> {
        walk_unary_mut(self, unary)
    }

    fn visit_binary_op_mut(
        &mut self,
        binary_op: &mut ast::BinaryOp,
    ) -> ControlFlow<Self::BreakValue> {
        walk_binary_op_mut(self, binary_op)
    }

    fn visit_binary_mut(&mut self, binary: &mut ast::BinaryExpr) -> ControlFlow<Self::BreakValue> {
        walk_binary_mut(self, binary)
    }

    fn visit_let_mut(&mut self, let_: &mut ast::LetExpr) -> ControlFlow<Self::BreakValue> {
        walk_let_mut(self, let_)
    }

    fn visit_if_mut(&mut self, if_: &mut ast::IfExpr) -> ControlFlow<Self::BreakValue> {
        walk_if_mut(self, if_)
    }

    fn visit_pat_mut(&mut self, pat: &mut ast::Pat) -> ControlFlow<Self::BreakValue> {
        walk_pat_mut(self, pat)
    }

    fn visit_branch_mut(&mut self, branch: &mut ast::Branch) -> ControlFlow<Self::BreakValue> {
        walk_branch_mut(self, branch)
    }

    fn visit_case_mut(&mut self, case: &mut ast::CaseExpr) -> ControlFlow<Self::BreakValue> {
        walk_case_mut(self, case)
    }

    fn visit_func_mut(&mut self, func: &mut ast::FuncExpr) -> ControlFlow<Self::BreakValue> {
        walk_func_mut(self, func)
    }

    fn visit_call_mut(&mut self, call: &mut ast::CallExpr) -> ControlFlow<Self::BreakValue> {
        walk_call_mut(self, call)
    }

    fn visit_expr_mut(&mut self, expr: &mut ast::Expr) -> ControlFlow<Self::BreakValue> {
        walk_expr_mut(self, expr)
    }
}

pub fn walk_ty_mut<V>(_visitor: &mut V, _ty: &mut MonoType) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    ControlFlow::Continue(())
}

pub fn walk_name_mut<V>(_visitor: &mut V, _name: &mut ast::Name) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    ControlFlow::Continue(())
}

pub fn walk_error_mut<V>(
    _visitor: &mut V,
    _error: &mut ast::ExprError,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    ControlFlow::Continue(())
}

pub fn walk_ident_mut<V>(visitor: &mut V, ident: &mut ast::IdentExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ty_mut(&mut ident.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_literal_mut<V>(
    visitor: &mut V,
    literal: &mut ast::LiteralExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ty_mut(&mut literal.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_list_mut<V>(visitor: &mut V, list: &mut ast::ListExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    for value in &mut list.values {
        visitor.visit_expr_mut(value)?;
    }
    visitor.visit_ty_mut(&mut list.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_property_mut<V>(
    visitor: &mut V,
    property: &mut ast::Property,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_name_mut(&mut property.key)?;
    visitor.visit_expr_mut(&mut property.value)?;
    ControlFlow::Continue(())
}

pub fn walk_record_mut<V>(
    visitor: &mut V,
    record: &mut ast::RecordExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    for property in &mut record.fields {
        visitor.visit_property_mut(property)?;
    }
    visitor.visit_ty_mut(&mut record.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_select_mut<V>(
    visitor: &mut V,
    record_select: &mut ast::RecordSelectExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_expr_mut(&mut record_select.source)?;
    visitor.visit_name_mut(&mut record_select.field)?;
    visitor.visit_ty_mut(&mut record_select.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_extend_mut<V>(
    visitor: &mut V,
    record_extend: &mut ast::RecordExtendExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_expr_mut(&mut record_extend.source)?;
    visitor.visit_name_mut(&mut record_extend.field)?;
    visitor.visit_expr_mut(&mut record_extend.value)?;
    visitor.visit_ty_mut(&mut record_extend.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_restrict_mut<V>(
    visitor: &mut V,
    record_restrict: &mut ast::RecordRestrictExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_expr_mut(&mut record_restrict.source)?;
    visitor.visit_name_mut(&mut record_restrict.field)?;
    visitor.visit_ty_mut(&mut record_restrict.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_record_update_mut<V>(
    visitor: &mut V,
    record_update: &mut ast::RecordUpdateExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_expr_mut(&mut record_update.source)?;
    visitor.visit_name_mut(&mut record_update.field)?;
    visitor.visit_expr_mut(&mut record_update.value)?;
    visitor.visit_ty_mut(&mut record_update.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_unary_op_mut<V>(
    visitor: &mut V,
    unary_op: &mut ast::UnaryOp,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ty_mut(&mut unary_op.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_unary_mut<V>(visitor: &mut V, unary: &mut ast::UnaryExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_unary_op_mut(&mut unary.op)?;
    visitor.visit_expr_mut(&mut unary.target)?;
    visitor.visit_ty_mut(&mut unary.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_binary_op_mut<V>(
    visitor: &mut V,
    binary_op: &mut ast::BinaryOp,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ty_mut(&mut binary_op.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_binary_mut<V>(
    visitor: &mut V,
    binary: &mut ast::BinaryExpr,
) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_binary_op_mut(&mut binary.op)?;
    visitor.visit_expr_mut(&mut binary.left)?;
    visitor.visit_expr_mut(&mut binary.right)?;
    visitor.visit_ty_mut(&mut binary.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_let_mut<V>(visitor: &mut V, let_: &mut ast::LetExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_name_mut(&mut let_.name)?;
    visitor.visit_expr_mut(&mut let_.value)?;
    visitor.visit_expr_mut(&mut let_.inside)?;
    visitor.visit_ty_mut(&mut let_.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_if_mut<V>(visitor: &mut V, if_: &mut ast::IfExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_expr_mut(&mut if_.predicate)?;
    visitor.visit_expr_mut(&mut if_.then)?;
    visitor.visit_expr_mut(&mut if_.or)?;
    visitor.visit_ty_mut(&mut if_.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_pat_mut<V>(visitor: &mut V, pat: &mut ast::Pat) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    todo!()
}

pub fn walk_branch_mut<V>(visitor: &mut V, branch: &mut ast::Branch) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_pat_mut(&mut branch.pat)?;
    visitor.visit_expr_mut(&mut branch.matches)?;
    ControlFlow::Continue(())
}

pub fn walk_case_mut<V>(visitor: &mut V, case: &mut ast::CaseExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ident_mut(&mut case.source)?;
    for branch in &mut case.branches {
        visitor.visit_branch_mut(branch)?;
    }
    visitor.visit_ty_mut(&mut case.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_func_mut<V>(visitor: &mut V, func: &mut ast::FuncExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ident_mut(&mut func.param)?;
    visitor.visit_expr_mut(&mut func.body)?;
    visitor.visit_ty_mut(&mut func.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_call_mut<V>(visitor: &mut V, call: &mut ast::CallExpr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    visitor.visit_ident_mut(&mut call.func)?;
    visitor.visit_expr_mut(&mut call.arg)?;
    visitor.visit_ty_mut(&mut call.ty)?;
    ControlFlow::Continue(())
}

pub fn walk_expr_mut<V>(visitor: &mut V, expr: &mut ast::Expr) -> ControlFlow<V::BreakValue>
where
    V: VisitMut,
{
    match expr {
        ast::Expr::Error(e) => visitor.visit_error_mut(e),
        ast::Expr::Literal(l) => visitor.visit_literal_mut(l),
        ast::Expr::Ident(i) => visitor.visit_ident_mut(i),
        ast::Expr::List(l) => visitor.visit_list_mut(l),
        ast::Expr::Record(r) => visitor.visit_record_mut(r),
        ast::Expr::RecordSelect(r) => visitor.visit_record_select_mut(r),
        ast::Expr::RecordExtend(r) => visitor.visit_record_extend_mut(r),
        ast::Expr::RecordRestrict(r) => visitor.visit_record_restrict_mut(r),
        ast::Expr::RecordUpdate(r) => visitor.visit_record_update_mut(r),
        ast::Expr::Unary(u) => visitor.visit_unary_mut(u),
        ast::Expr::Binary(b) => visitor.visit_binary_mut(b),
        ast::Expr::Let(l) => visitor.visit_let_mut(l),
        ast::Expr::If(i) => visitor.visit_if_mut(i),
        ast::Expr::Case(c) => visitor.visit_case_mut(c),
        ast::Expr::Func(f) => visitor.visit_func_mut(f),
        ast::Expr::Call(c) => visitor.visit_call_mut(c),
    }
}
