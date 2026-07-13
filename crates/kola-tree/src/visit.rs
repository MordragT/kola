use pastey::paste;
use std::ops::ControlFlow;

use crate::{id::Id, node, tree::TreeView};

pub trait Visitable<T: TreeView> {
    fn visit_by<V>(&self, visitor: &mut V, storage: &T) -> ControlFlow<V::BreakValue>
    where
        V: Visitor<T>;
}

macro_rules! impl_visitable {
    ($($variant:ident),* $(,)?) => {
        paste!{
            $(
                impl<T: TreeView> Visitable<T> for Id<node::$variant> {
                    fn visit_by<V>(&self, visitor: &mut V, storage: &T) -> ControlFlow<V::BreakValue>
                    where
                        V: Visitor<T>,
                    {
                        visitor.[<visit_ $variant:snake:lower>](*self, storage)
                    }
                }
            )*
        }
    };
}

impl_visitable!(
    FunctorName,
    ModuleTypeName,
    ModuleName,
    KindName,
    TypeName,
    ValueName,
    AnyPat,
    LiteralPat,
    BindPat,
    ListElPat,
    ListPat,
    RecordFieldPat,
    RecordPat,
    VariantTagPat,
    VariantPat,
    PatError,
    Pat,
    LiteralExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    RecordMergeExpr,
    FieldPath,
    QualifiedExpr,
    UnaryOp,
    UnaryExpr,
    BinaryOp,
    BinaryExpr,
    LetExpr,
    CaseBranch,
    CaseExpr,
    IfExpr,
    LambdaExpr,
    CallExpr,
    HandlerClause,
    HandleExpr,
    DoExpr,
    TagExpr,
    TypeWitnessExpr,
    ExprError,
    Expr,
    EffectOpType,
    EffectType,
    QualifiedType,
    TypeVar,
    LabelOrVar,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    TypeExpr,
    TypeError,
    TypeVarBind,
    ForallBinder,
    TypeScheme,
    BindError,
    Vis,
    ValueBind,
    TypeBind,
    ModuleBind,
    ModuleTypeBind,
    FunctorParam,
    FunctorBind,
    Bind,
    ModuleError,
    ModuleBody,
    ModulePath,
    ModuleImport,
    FunctorArgs,
    FunctorApp,
    ModuleExpr,
    SpecError,
    ValueSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType,
);

pub trait Visitor<S: TreeView> {
    type BreakValue;

    fn visit_functor_name(
        &mut self,
        _id: Id<node::FunctorName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_module_type_name(
        &mut self,
        _id: Id<node::ModuleTypeName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_module_name(
        &mut self,
        _id: Id<node::ModuleName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_kind_name(
        &mut self,
        _id: Id<node::KindName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_type_name(
        &mut self,
        _id: Id<node::TypeName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_value_name(
        &mut self,
        _id: Id<node::ValueName>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_any_pat(
        &mut self,
        _id: Id<node::AnyPat>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_literal_pat(
        &mut self,
        _id: Id<node::LiteralPat>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_bind_pat(
        &mut self,
        _id: Id<node::BindPat>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_el_pat(
        &mut self,
        id: Id<node::ListElPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        match *arena.get(id) {
            node::ListElPat::Pat(pat_id) => self.visit_pat(pat_id, arena),
            node::ListElPat::Spread(name_opt) => {
                if let Some(name_id) = name_opt {
                    self.visit_value_name(name_id, arena)?;
                }
                ControlFlow::Continue(())
            }
        }
    }
    fn visit_list_el_pat(
        &mut self,
        id: Id<node::ListElPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_el_pat(id, arena)
    }

    fn walk_list_pat(&mut self, id: Id<node::ListPat>, arena: &S) -> ControlFlow<Self::BreakValue> {
        for element_id in id.get(arena).0.iter(arena) {
            self.visit_list_el_pat(element_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_list_pat(
        &mut self,
        id: Id<node::ListPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_pat(id, arena)
    }

    fn walk_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldPat { field, pat } = arena.get(id);
        self.visit_value_name(*field, arena)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_pat(id, arena)
    }

    fn walk_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in id.get(arena).fields.iter(arena) {
            self.visit_record_field_pat(field_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_pat(id, arena)
    }

    fn walk_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantTagPat { tag, pat } = arena.get(id);
        self.visit_value_name(*tag, arena)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_tag_pat(id, arena)
    }

    fn walk_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for tag_id in id.get(arena).0.iter(arena) {
            self.visit_variant_tag_pat(tag_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_pat(id, arena)
    }

    fn visit_pat_error(
        &mut self,
        _id: Id<node::PatError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_pat(&mut self, id: Id<node::Pat>, arena: &S) -> ControlFlow<Self::BreakValue> {
        use node::Pat::*;
        match *arena.get(id) {
            Error(id) => self.visit_pat_error(id, arena),
            Any(id) => self.visit_any_pat(id, arena),
            Literal(id) => self.visit_literal_pat(id, arena),
            Bind(id) => self.visit_bind_pat(id, arena),
            List(id) => self.visit_list_pat(id, arena),
            Record(id) => self.visit_record_pat(id, arena),
            Variant(id) => self.visit_variant_pat(id, arena),
        }
    }
    fn visit_pat(&mut self, id: Id<node::Pat>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_pat(id, arena)
    }

    fn visit_literal_expr(
        &mut self,
        _id: Id<node::LiteralExpr>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_unary_op(
        &mut self,
        _id: Id<node::UnaryOp>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_binary_op(
        &mut self,
        _id: Id<node::BinaryOp>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_record_update_op(
        &mut self,
        _id: Id<node::RecordUpdateOp>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_expr_error(
        &mut self,
        _id: Id<node::ExprError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_type_error(
        &mut self,
        _id: Id<node::TypeError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for element_id in id.get(arena).0.iter(arena) {
            self.visit_expr(element_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_expr(id, arena)
    }

    fn walk_record_field(
        &mut self,
        id: Id<node::RecordField>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordField { label, ty, value } = arena.get(id);
        self.visit_value_name(*label, arena)?;
        if let Some(ty) = ty {
            self.visit_type_expr(*ty, arena)?;
        }
        self.visit_expr(*value, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_record_field(
        &mut self,
        id: Id<node::RecordField>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field(id, arena)
    }

    fn walk_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in id.get(arena).0.iter(arena) {
            self.visit_record_field(field_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_expr(id, arena)
    }

    fn walk_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordExtendExpr {
            source,
            source_type,
            field_path,
            value,
            value_type,
        } = *arena.get(id);
        self.visit_expr(source, arena)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, arena)?;
        }
        self.visit_field_path(field_path, arena)?;
        self.visit_expr(value, arena)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_extend_expr(id, arena)
    }

    fn walk_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordRestrictExpr {
            source,
            source_type,
            field_path,
            value_type,
        } = *arena.get(id);
        self.visit_expr(source, arena)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, arena)?;
        }
        self.visit_field_path(field_path, arena)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_restrict_expr(id, arena)
    }

    fn walk_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordUpdateExpr {
            source,
            source_type,
            field_path,
            op,
            value,
            value_type,
        } = *arena.get(id);
        self.visit_expr(source, arena)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, arena)?;
        }
        self.visit_field_path(field_path, arena)?;
        self.visit_record_update_op(op, arena)?;
        self.visit_expr(value, arena)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_update_expr(id, arena)
    }

    fn walk_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordMergeExpr { lhs, rhs } = *arena.get(id);
        self.visit_expr(lhs, arena)?;
        self.visit_expr(rhs, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_merge_expr(id, arena)
    }

    fn walk_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in id.get(arena).0.iter(arena) {
            self.visit_value_name(field_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_field_path(id, arena)
    }

    fn walk_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            field_path,
        } = *arena.get(id);
        if let Some(p) = module_path {
            self.visit_module_path(p, arena)?;
        }
        self.visit_value_name(source, arena)?;
        if let Some(p) = field_path {
            self.visit_field_path(p, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_expr(id, arena)
    }

    fn walk_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::UnaryExpr { op, operand } = *arena.get(id);
        self.visit_unary_op(op, arena)?;
        self.visit_expr(operand, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_unary_expr(id, arena)
    }

    fn walk_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::BinaryExpr { op, lhs, rhs } = *arena.get(id);
        self.visit_binary_op(op, arena)?;
        self.visit_expr(lhs, arena)?;
        self.visit_expr(rhs, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_binary_expr(id, arena)
    }

    fn walk_let_expr(&mut self, id: Id<node::LetExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *arena.get(id);
        self.visit_value_name(name, arena)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, arena)?;
        }
        self.visit_expr(value, arena)?;
        self.visit_expr(body, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_let_expr(
        &mut self,
        id: Id<node::LetExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_let_expr(id, arena)
    }

    fn walk_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseBranch { pat, body } = *arena.get(id);
        self.visit_pat(pat, arena)?;
        self.visit_expr(body, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_branch(id, arena)
    }

    fn walk_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseExpr { source, branches } = arena.get(id);
        self.visit_expr(*source, arena)?;
        for branch_id in branches.iter(arena) {
            self.visit_case_branch(branch_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_expr(id, arena)
    }

    fn walk_if_expr(&mut self, id: Id<node::IfExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        let node::IfExpr {
            pred,
            then,
            or_else,
        } = *arena.get(id);
        self.visit_expr(pred, arena)?;
        self.visit_expr(then, arena)?;
        self.visit_expr(or_else, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_if_expr(&mut self, id: Id<node::IfExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_if_expr(id, arena)
    }

    fn walk_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = arena.get(id);
        self.visit_value_name(*param, arena)?;
        if let Some(pt) = param_type {
            self.visit_type_expr(*pt, arena)?;
        }
        self.visit_expr(*body, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_lambda_expr(id, arena)
    }

    fn walk_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CallExpr { func, arg } = arena.get(id);
        self.visit_expr(*func, arena)?;
        self.visit_expr(*arg, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_call_expr(id, arena)
    }

    fn walk_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op, param, body } = *arena.get(id);
        self.visit_value_name(op, arena)?;
        self.visit_value_name(param, arena)?;
        self.visit_expr(body, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handler_clause(id, arena)
    }

    fn walk_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandleExpr { source, clauses } = arena.get(id);
        self.visit_expr(*source, arena)?;
        for clause_id in clauses.iter(arena) {
            self.visit_handler_clause(clause_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handle_expr(id, arena)
    }

    fn walk_do_expr(&mut self, id: Id<node::DoExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        let node::DoExpr { op, arg } = *arena.get(id);
        self.visit_value_name(op, arena)?;
        self.visit_expr(arg, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_do_expr(&mut self, id: Id<node::DoExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_do_expr(id, arena)
    }

    fn walk_tag_expr(&mut self, id: Id<node::TagExpr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        let tag = arena.get(id).0;
        self.visit_value_name(tag, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_tag_expr(
        &mut self,
        id: Id<node::TagExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_expr(id, arena)
    }

    fn walk_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        match *arena.get(id) {
            node::TypeWitnessExpr::Qualified(id) => self.visit_qualified_type(id, arena),
            node::TypeWitnessExpr::Label(id) => self.visit_value_name(id, arena),
        }
    }
    fn visit_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_witness_expr(id, arena)
    }

    fn walk_expr(&mut self, id: Id<node::Expr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        use node::Expr::*;
        match *arena.get(id) {
            Error(id) => self.visit_expr_error(id, arena),
            Literal(id) => self.visit_literal_expr(id, arena),
            Qualified(id) => self.visit_qualified_expr(id, arena),
            List(id) => self.visit_list_expr(id, arena),
            Record(id) => self.visit_record_expr(id, arena),
            RecordExtend(id) => self.visit_record_extend_expr(id, arena),
            RecordRestrict(id) => self.visit_record_restrict_expr(id, arena),
            RecordUpdate(id) => self.visit_record_update_expr(id, arena),
            RecordMerge(id) => self.visit_record_merge_expr(id, arena),
            Unary(id) => self.visit_unary_expr(id, arena),
            Binary(id) => self.visit_binary_expr(id, arena),
            Let(id) => self.visit_let_expr(id, arena),
            If(id) => self.visit_if_expr(id, arena),
            Case(id) => self.visit_case_expr(id, arena),
            Lambda(id) => self.visit_lambda_expr(id, arena),
            Call(id) => self.visit_call_expr(id, arena),
            Handle(id) => self.visit_handle_expr(id, arena),
            Do(id) => self.visit_do_expr(id, arena),
            Tag(id) => self.visit_tag_expr(id, arena),
            TypeWitness(id) => self.visit_type_witness_expr(id, arena),
        }
    }
    fn visit_expr(&mut self, id: Id<node::Expr>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, arena)
    }

    fn walk_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::EffectOpType { name, ty } = *arena.get(id);
        self.visit_value_name(name, arena)?;
        self.visit_type_expr(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_op_type(id, arena)
    }

    fn walk_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for op_id in id.get(arena).0.iter(arena) {
            self.visit_effect_op_type(op_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_type(id, arena)
    }

    fn walk_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = arena.get(id);
        if let Some(p) = path {
            self.visit_module_path(*p, arena)?;
        }
        self.visit_type_name(*ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_type(id, arena)
    }

    fn visit_type_var(
        &mut self,
        _id: Id<node::TypeVar>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        match *arena.get(id) {
            node::LabelOrVar::Label(id) => self.visit_value_name(id, arena),
            node::LabelOrVar::Var(id) => self.visit_type_var(id, arena),
        }
    }
    fn visit_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_label_or_var(id, arena)
    }

    fn walk_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { label_or_var, ty } = *arena.get(id);
        self.visit_label_or_var(label_or_var, arena)?;
        self.visit_type_expr(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_type(id, arena)
    }

    fn walk_record_type(
        &mut self,
        id: Id<node::RecordType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordType { fields, extension } = arena.get(id);
        for field_id in fields.iter(arena) {
            self.visit_record_field_type(field_id, arena)?;
        }
        if let Some(e) = extension {
            self.visit_type_name(*e, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_type(
        &mut self,
        id: Id<node::RecordType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_type(id, arena)
    }

    fn walk_tag_type(&mut self, id: Id<node::TagType>, arena: &S) -> ControlFlow<Self::BreakValue> {
        let node::TagType { name, ty } = arena.get(id);
        self.visit_value_name(*name, arena)?;
        if let Some(t) = ty {
            self.visit_type_expr(*t, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_tag_type(
        &mut self,
        id: Id<node::TagType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_type(id, arena)
    }

    fn walk_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantType { tags, extension } = arena.get(id);
        for tag_id in tags.iter(arena) {
            self.visit_tag_type(tag_id, arena)?;
        }
        if let Some(e) = extension {
            self.visit_type_name(*e, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_type(id, arena)
    }

    fn walk_func_type(
        &mut self,
        id: Id<node::FuncType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FuncType { input, output } = arena.get(id);
        self.visit_type_expr(*input, arena)?;
        self.visit_comp_type(*output, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_func_type(
        &mut self,
        id: Id<node::FuncType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_func_type(id, arena)
    }

    fn walk_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeApplication { constructor, arg } = arena.get(id);
        self.visit_type_expr(*constructor, arena)?;
        self.visit_type_expr(*arg, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_application(id, arena)
    }

    fn walk_comp_type(
        &mut self,
        id: Id<node::CompType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CompType { ty, effect } = *arena.get(id);
        if let Some(e) = effect {
            self.visit_effect_type(e, arena)?;
        }
        self.visit_type_expr(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_comp_type(
        &mut self,
        id: Id<node::CompType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_comp_type(id, arena)
    }

    fn walk_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        use node::TypeExpr::*;
        match *arena.get(id) {
            Error(id) => self.visit_type_error(id, arena),
            Qualified(id) => self.visit_qualified_type(id, arena),
            Record(id) => self.visit_record_type(id, arena),
            Variant(id) => self.visit_variant_type(id, arena),
            Func(id) => self.visit_func_type(id, arena),
            Application(id) => self.visit_type_application(id, arena),
        }
    }
    fn visit_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_expr(id, arena)
    }

    fn walk_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeVarBind { var, kind } = *arena.get(id);
        if let Some(k) = kind {
            self.visit_kind_name(k, arena)?;
        }
        self.visit_type_var(var, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_var_bind(id, arena)
    }

    fn walk_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for bind_id in id.get(arena).0.iter(arena) {
            self.visit_type_var_bind(bind_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_forall_binder(id, arena)
    }

    fn walk_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeScheme { forall, ty } = *arena.get(id);
        if let Some(f) = forall {
            self.visit_forall_binder(f, arena)?;
        }
        self.visit_type_expr(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_scheme(id, arena)
    }

    fn visit_module_error(
        &mut self,
        _id: Id<node::ModuleError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_module_body(
        &mut self,
        id: Id<node::ModuleBody>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for bind_id in id.get(arena).0.iter(arena) {
            self.visit_bind(bind_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_module_body(
        &mut self,
        id: Id<node::ModuleBody>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_body(id, arena)
    }

    fn walk_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for name_id in id.get(arena).0.iter(arena) {
            self.visit_module_name(name_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_path(id, arena)
    }

    fn visit_module_import(
        &mut self,
        _id: Id<node::ModuleImport>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_functor_args(
        &mut self,
        id: Id<node::FunctorArgs>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for arg_id in id.get(arena).0.iter(arena) {
            self.visit_module_path(arg_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_functor_args(
        &mut self,
        id: Id<node::FunctorArgs>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_args(id, arena)
    }

    fn walk_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorApp { path, func, args } = arena.get(id);
        if let Some(p) = path {
            self.visit_module_path(*p, arena)?;
        }
        self.visit_functor_name(*func, arena)?;
        self.visit_functor_args(*args, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_app(id, arena)
    }

    fn walk_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleExpr::*;
        match *arena.get(id) {
            Error(id) => self.visit_module_error(id, arena),
            Import(id) => self.visit_module_import(id, arena),
            Body(id) => self.visit_module_body(id, arena),
            Path(id) => self.visit_module_path(id, arena),
            FunctorApp(id) => self.visit_functor_app(id, arena),
        }
    }
    fn visit_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_expr(id, arena)
    }

    fn visit_vis(&mut self, _id: Id<node::Vis>, _arena: &S) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        } = *arena.get(id);
        self.visit_vis(vis, arena)?;
        self.visit_value_name(name, arena)?;
        if let Some(ts) = ty_scheme {
            self.visit_type_scheme(ts, arena)?;
        }
        self.visit_expr(value, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_bind(id, arena)
    }

    fn walk_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind {
            vis,
            name,
            ty_scheme,
        } = *arena.get(id);
        self.visit_vis(vis, arena)?;
        self.visit_type_name(name, arena)?;
        self.visit_type_scheme(ty_scheme, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_bind(id, arena)
    }

    fn walk_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty,
            value,
        } = *arena.get(id);
        self.visit_vis(vis, arena)?;
        self.visit_module_name(name, arena)?;
        if let Some(t) = ty {
            self.visit_module_type(t, arena)?;
        }
        self.visit_module_expr(value, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_bind(id, arena)
    }

    fn walk_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { vis, name, ty } = *arena.get(id);
        self.visit_vis(vis, arena)?;
        self.visit_module_type_name(name, arena)?;
        self.visit_module_type(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type_bind(id, arena)
    }

    fn walk_functor_param(
        &mut self,
        id: Id<node::FunctorParam>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorParam { name, ty } = *arena.get(id);
        self.visit_module_name(name, arena)?;
        self.visit_module_type(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_param(
        &mut self,
        id: Id<node::FunctorParam>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_param(id, arena)
    }

    fn walk_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorBind {
            vis,
            name,
            params,
            body,
        } = arena.get(id);
        self.visit_vis(*vis, arena)?;
        self.visit_functor_name(*name, arena)?;
        for param_id in params.iter(arena) {
            self.visit_functor_param(param_id, arena)?;
        }
        self.visit_module_body(*body, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_bind(id, arena)
    }

    fn visit_bind_error(
        &mut self,
        _id: Id<node::BindError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_bind(&mut self, id: Id<node::Bind>, arena: &S) -> ControlFlow<Self::BreakValue> {
        use node::Bind::*;
        match *arena.get(id) {
            Value(id) => self.visit_value_bind(id, arena),
            Type(id) => self.visit_type_bind(id, arena),
            Module(id) => self.visit_module_bind(id, arena),
            ModuleType(id) => self.visit_module_type_bind(id, arena),
            Functor(id) => self.visit_functor_bind(id, arena),
            Error(id) => self.visit_bind_error(id, arena),
        }
    }
    fn visit_bind(&mut self, id: Id<node::Bind>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_bind(id, arena)
    }

    fn visit_spec_error(
        &mut self,
        _id: Id<node::SpecError>,
        _arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueSpec { name, ty } = *arena.get(id);
        self.visit_value_name(name, arena)?;
        self.visit_type_scheme(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_spec(id, arena)
    }

    fn walk_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleSpec { name, ty } = *arena.get(id);
        self.visit_module_name(name, arena)?;
        self.visit_module_type(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_spec(id, arena)
    }

    fn walk_spec(&mut self, id: Id<node::Spec>, arena: &S) -> ControlFlow<Self::BreakValue> {
        use node::Spec::*;
        match *arena.get(id) {
            Value(id) => self.visit_value_spec(id, arena),
            Module(id) => self.visit_module_spec(id, arena),
            Error(id) => self.visit_spec_error(id, arena),
        }
    }
    fn visit_spec(&mut self, id: Id<node::Spec>, arena: &S) -> ControlFlow<Self::BreakValue> {
        self.walk_spec(id, arena)
    }

    fn walk_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        for spec_id in id.get(arena).0.iter(arena) {
            self.visit_spec(spec_id, arena)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_concrete_module_type(id, arena)
    }

    fn walk_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *arena.get(id);
        if let Some(p) = path {
            self.visit_module_path(p, arena)?;
        }
        self.visit_module_type_name(ty, arena)?;
        ControlFlow::Continue(())
    }
    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_module_type(id, arena)
    }

    fn walk_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleType::*;
        match *arena.get(id) {
            Concrete(id) => self.visit_concrete_module_type(id, arena),
            Qualified(id) => self.visit_qualified_module_type(id, arena),
        }
    }
    fn visit_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        arena: &S,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type(id, arena)
    }
}
