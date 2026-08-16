use pastey::paste;
use std::ops::ControlFlow;

use crate::{id::Id, id::IdExt, node, tree::Tree};
use kola_relations::{SliceRelation, TotalRelation};

pub trait Visitable<M> {
    fn visit_by<V>(&self, visitor: &mut V, tree: &Tree<M>) -> ControlFlow<V::BreakValue>
    where
        V: Visitor<M>;
}

macro_rules! impl_visitable {
    ($($variant:ident),* $(,)?) => {
        paste!{
            $(
                impl<M> Visitable<M> for Id<node::$variant> {
                    fn visit_by<V>(&self, visitor: &mut V, tree: &Tree<M>) -> ControlFlow<V::BreakValue>
                    where
                        V: Visitor<M>,
                    {
                        visitor.[<visit_ $variant:snake:lower>](*self, tree)
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

pub trait Visitor<M: 'static> {
    type BreakValue;

    fn visit_functor_name(
        &mut self,
        _id: Id<node::FunctorName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_module_type_name(
        &mut self,
        _id: Id<node::ModuleTypeName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_module_name(
        &mut self,
        _id: Id<node::ModuleName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_kind_name(
        &mut self,
        _id: Id<node::KindName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_type_name(
        &mut self,
        _id: Id<node::TypeName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_value_name(
        &mut self,
        _id: Id<node::ValueName>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_any_pat(
        &mut self,
        _id: Id<node::AnyPat>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_literal_pat(
        &mut self,
        _id: Id<node::LiteralPat>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_bind_pat(
        &mut self,
        _id: Id<node::BindPat>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_el_pat(
        &mut self,
        id: Id<node::ListElPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        match *tree.get(id) {
            node::ListElPat::Pat(pat_id) => self.visit_pat(pat_id, tree),
            node::ListElPat::Spread(name_opt) => {
                if let Some(name_id) = name_opt {
                    self.visit_value_name(name_id, tree)?;
                }
                ControlFlow::Continue(())
            }
        }
    }
    fn visit_list_el_pat(
        &mut self,
        id: Id<node::ListElPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_el_pat(id, tree)
    }

    fn walk_list_pat(
        &mut self,
        id: Id<node::ListPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for element_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_list_el_pat(element_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_list_pat(
        &mut self,
        id: Id<node::ListPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_pat(id, tree)
    }

    fn walk_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldPat { field, pat } = tree.get(id);
        self.visit_value_name(*field, tree)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_pat(id, tree)
    }

    fn walk_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in tree.get_slice(id.get(tree).fields).iter().copied() {
            self.visit_record_field_pat(field_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_pat(id, tree)
    }

    fn walk_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantTagPat { tag, pat } = tree.get(id);
        self.visit_value_name(*tag, tree)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_tag_pat(id, tree)
    }

    fn walk_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for tag_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_variant_tag_pat(tag_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_pat(id, tree)
    }

    fn visit_pat_error(
        &mut self,
        _id: Id<node::PatError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_pat(&mut self, id: Id<node::Pat>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        use node::Pat::*;
        match *tree.get(id) {
            Error(id) => self.visit_pat_error(id, tree),
            Any(id) => self.visit_any_pat(id, tree),
            Literal(id) => self.visit_literal_pat(id, tree),
            Bind(id) => self.visit_bind_pat(id, tree),
            List(id) => self.visit_list_pat(id, tree),
            Record(id) => self.visit_record_pat(id, tree),
            Variant(id) => self.visit_variant_pat(id, tree),
        }
    }
    fn visit_pat(&mut self, id: Id<node::Pat>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        self.walk_pat(id, tree)
    }

    fn visit_literal_expr(
        &mut self,
        _id: Id<node::LiteralExpr>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_unary_op(
        &mut self,
        _id: Id<node::UnaryOp>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_binary_op(
        &mut self,
        _id: Id<node::BinaryOp>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_record_update_op(
        &mut self,
        _id: Id<node::RecordUpdateOp>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_expr_error(
        &mut self,
        _id: Id<node::ExprError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }
    fn visit_type_error(
        &mut self,
        _id: Id<node::TypeError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for element_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_expr(element_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_expr(id, tree)
    }

    fn walk_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordField { label, ty, value } = tree.get(id);
        self.visit_value_name(*label, tree)?;
        if let Some(ty) = ty {
            self.visit_type_expr(*ty, tree)?;
        }
        self.visit_expr(*value, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field(id, tree)
    }

    fn walk_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_record_field(field_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_expr(id, tree)
    }

    fn walk_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordExtendExpr {
            source,
            source_type,
            field_path,
            value,
            value_type,
        } = *tree.get(id);
        self.visit_expr(source, tree)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, tree)?;
        }
        self.visit_field_path(field_path, tree)?;
        self.visit_expr(value, tree)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_extend_expr(id, tree)
    }

    fn walk_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordRestrictExpr {
            source,
            source_type,
            field_path,
            value_type,
        } = *tree.get(id);
        self.visit_expr(source, tree)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, tree)?;
        }
        self.visit_field_path(field_path, tree)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_restrict_expr(id, tree)
    }

    fn walk_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordUpdateExpr {
            source,
            source_type,
            field_path,
            op,
            value,
            value_type,
        } = *tree.get(id);
        self.visit_expr(source, tree)?;
        if let Some(t) = source_type {
            self.visit_type_expr(t, tree)?;
        }
        self.visit_field_path(field_path, tree)?;
        self.visit_record_update_op(op, tree)?;
        self.visit_expr(value, tree)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_update_expr(id, tree)
    }

    fn walk_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordMergeExpr { lhs, rhs } = *tree.get(id);
        self.visit_expr(lhs, tree)?;
        self.visit_expr(rhs, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_merge_expr(id, tree)
    }

    fn walk_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for field_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_value_name(field_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_field_path(id, tree)
    }

    fn walk_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            field_path,
        } = *tree.get(id);
        if let Some(p) = module_path {
            self.visit_module_path(p, tree)?;
        }
        self.visit_value_name(source, tree)?;
        if let Some(p) = field_path {
            self.visit_field_path(p, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_expr(id, tree)
    }

    fn walk_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::UnaryExpr { op, operand } = *tree.get(id);
        self.visit_unary_op(op, tree)?;
        self.visit_expr(operand, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_unary_expr(id, tree)
    }

    fn walk_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::BinaryExpr { op, lhs, rhs } = *tree.get(id);
        self.visit_binary_op(op, tree)?;
        self.visit_expr(lhs, tree)?;
        self.visit_expr(rhs, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_binary_expr(id, tree)
    }

    fn walk_let_expr(
        &mut self,
        id: Id<node::LetExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *tree.get(id);
        self.visit_value_name(name, tree)?;
        if let Some(t) = value_type {
            self.visit_type_expr(t, tree)?;
        }
        self.visit_expr(value, tree)?;
        self.visit_expr(body, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_let_expr(
        &mut self,
        id: Id<node::LetExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_let_expr(id, tree)
    }

    fn walk_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseBranch { pat, body } = *tree.get(id);
        self.visit_pat(pat, tree)?;
        self.visit_expr(body, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_branch(id, tree)
    }

    fn walk_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseExpr { source, branches } = tree.get(id);
        self.visit_expr(*source, tree)?;
        for branch_id in tree.get_slice(*branches).iter().copied() {
            self.visit_case_branch(branch_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_expr(id, tree)
    }

    fn walk_if_expr(
        &mut self,
        id: Id<node::IfExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::IfExpr {
            pred,
            then,
            or_else,
        } = *tree.get(id);
        self.visit_expr(pred, tree)?;
        self.visit_expr(then, tree)?;
        self.visit_expr(or_else, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_if_expr(
        &mut self,
        id: Id<node::IfExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_if_expr(id, tree)
    }

    fn walk_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = tree.get(id);
        self.visit_value_name(*param, tree)?;
        if let Some(pt) = param_type {
            self.visit_type_expr(*pt, tree)?;
        }
        self.visit_expr(*body, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_lambda_expr(id, tree)
    }

    fn walk_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CallExpr { func, arg } = tree.get(id);
        self.visit_expr(*func, tree)?;
        self.visit_expr(*arg, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_call_expr(id, tree)
    }

    fn walk_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op, param, body } = *tree.get(id);
        self.visit_value_name(op, tree)?;
        self.visit_value_name(param, tree)?;
        self.visit_expr(body, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handler_clause(id, tree)
    }

    fn walk_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandleExpr { source, clauses } = tree.get(id);
        self.visit_expr(*source, tree)?;
        for clause_id in tree.get_slice(*clauses).iter().copied() {
            self.visit_handler_clause(clause_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handle_expr(id, tree)
    }

    fn walk_do_expr(
        &mut self,
        id: Id<node::DoExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::DoExpr { op, arg } = *tree.get(id);
        self.visit_value_name(op, tree)?;
        self.visit_expr(arg, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_do_expr(
        &mut self,
        id: Id<node::DoExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_do_expr(id, tree)
    }

    fn walk_tag_expr(
        &mut self,
        id: Id<node::TagExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let tag = tree.get(id).0;
        self.visit_value_name(tag, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_tag_expr(
        &mut self,
        id: Id<node::TagExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_expr(id, tree)
    }

    fn walk_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        match *tree.get(id) {
            node::TypeWitnessExpr::Qualified(id) => self.visit_qualified_type(id, tree),
            node::TypeWitnessExpr::Label(id) => self.visit_value_name(id, tree),
        }
    }
    fn visit_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_witness_expr(id, tree)
    }

    fn walk_expr(&mut self, id: Id<node::Expr>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        use node::Expr::*;
        match *tree.get(id) {
            Error(id) => self.visit_expr_error(id, tree),
            Literal(id) => self.visit_literal_expr(id, tree),
            Qualified(id) => self.visit_qualified_expr(id, tree),
            List(id) => self.visit_list_expr(id, tree),
            Record(id) => self.visit_record_expr(id, tree),
            RecordExtend(id) => self.visit_record_extend_expr(id, tree),
            RecordRestrict(id) => self.visit_record_restrict_expr(id, tree),
            RecordUpdate(id) => self.visit_record_update_expr(id, tree),
            RecordMerge(id) => self.visit_record_merge_expr(id, tree),
            Unary(id) => self.visit_unary_expr(id, tree),
            Binary(id) => self.visit_binary_expr(id, tree),
            Let(id) => self.visit_let_expr(id, tree),
            If(id) => self.visit_if_expr(id, tree),
            Case(id) => self.visit_case_expr(id, tree),
            Lambda(id) => self.visit_lambda_expr(id, tree),
            Call(id) => self.visit_call_expr(id, tree),
            Handle(id) => self.visit_handle_expr(id, tree),
            Do(id) => self.visit_do_expr(id, tree),
            Tag(id) => self.visit_tag_expr(id, tree),
            TypeWitness(id) => self.visit_type_witness_expr(id, tree),
        }
    }
    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)
    }

    fn walk_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::EffectOpType { name, ty } = *tree.get(id);
        self.visit_value_name(name, tree)?;
        self.visit_type_expr(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_op_type(id, tree)
    }

    fn walk_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for op_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_effect_op_type(op_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_type(id, tree)
    }

    fn walk_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = tree.get(id);
        if let Some(p) = path {
            self.visit_module_path(*p, tree)?;
        }
        self.visit_type_name(*ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_type(id, tree)
    }

    fn visit_type_var(
        &mut self,
        _id: Id<node::TypeVar>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        match *tree.get(id) {
            node::LabelOrVar::Label(id) => self.visit_value_name(id, tree),
            node::LabelOrVar::Var(id) => self.visit_type_var(id, tree),
        }
    }
    fn visit_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_label_or_var(id, tree)
    }

    fn walk_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { label_or_var, ty } = *tree.get(id);
        self.visit_label_or_var(label_or_var, tree)?;
        self.visit_type_expr(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_type(id, tree)
    }

    fn walk_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordType { fields, extension } = tree.get(id);
        for field_id in tree.get_slice(*fields).iter().copied() {
            self.visit_record_field_type(field_id, tree)?;
        }
        if let Some(e) = extension {
            self.visit_type_name(*e, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_type(id, tree)
    }

    fn walk_tag_type(
        &mut self,
        id: Id<node::TagType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TagType { name, ty } = tree.get(id);
        self.visit_value_name(*name, tree)?;
        if let Some(t) = ty {
            self.visit_type_expr(*t, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_tag_type(
        &mut self,
        id: Id<node::TagType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_type(id, tree)
    }

    fn walk_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantType { tags, extension } = tree.get(id);
        for tag_id in tree.get_slice(*tags).iter().copied() {
            self.visit_tag_type(tag_id, tree)?;
        }
        if let Some(e) = extension {
            self.visit_type_name(*e, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_type(id, tree)
    }

    fn walk_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FuncType { input, output } = tree.get(id);
        self.visit_type_expr(*input, tree)?;
        self.visit_comp_type(*output, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_func_type(id, tree)
    }

    fn walk_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeApplication { constructor, arg } = tree.get(id);
        self.visit_type_expr(*constructor, tree)?;
        self.visit_type_expr(*arg, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_application(id, tree)
    }

    fn walk_comp_type(
        &mut self,
        id: Id<node::CompType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CompType { ty, effect } = *tree.get(id);
        if let Some(e) = effect {
            self.visit_effect_type(e, tree)?;
        }
        self.visit_type_expr(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_comp_type(
        &mut self,
        id: Id<node::CompType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_comp_type(id, tree)
    }

    fn walk_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        use node::TypeExpr::*;
        match *tree.get(id) {
            Error(id) => self.visit_type_error(id, tree),
            Qualified(id) => self.visit_qualified_type(id, tree),
            Record(id) => self.visit_record_type(id, tree),
            Variant(id) => self.visit_variant_type(id, tree),
            Func(id) => self.visit_func_type(id, tree),
            Application(id) => self.visit_type_application(id, tree),
        }
    }
    fn visit_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_expr(id, tree)
    }

    fn walk_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeVarBind { var, kind } = *tree.get(id);
        if let Some(k) = kind {
            self.visit_kind_name(k, tree)?;
        }
        self.visit_type_var(var, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_var_bind(id, tree)
    }

    fn walk_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for bind_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_type_var_bind(bind_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_forall_binder(id, tree)
    }

    fn walk_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeScheme { forall, ty } = *tree.get(id);
        if let Some(f) = forall {
            self.visit_forall_binder(f, tree)?;
        }
        self.visit_type_expr(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_scheme(id, tree)
    }

    fn visit_module_error(
        &mut self,
        _id: Id<node::ModuleError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_module_body(
        &mut self,
        id: Id<node::ModuleBody>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for bind_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_bind(bind_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_module_body(
        &mut self,
        id: Id<node::ModuleBody>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_body(id, tree)
    }

    fn walk_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for name_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_module_name(name_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_path(id, tree)
    }

    fn visit_module_import(
        &mut self,
        _id: Id<node::ModuleImport>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_functor_args(
        &mut self,
        id: Id<node::FunctorArgs>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for arg_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_module_path(arg_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_functor_args(
        &mut self,
        id: Id<node::FunctorArgs>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_args(id, tree)
    }

    fn walk_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorApp { path, func, args } = tree.get(id);
        if let Some(p) = path {
            self.visit_module_path(*p, tree)?;
        }
        self.visit_functor_name(*func, tree)?;
        self.visit_functor_args(*args, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_app(id, tree)
    }

    fn walk_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleExpr::*;
        match *tree.get(id) {
            Error(id) => self.visit_module_error(id, tree),
            Import(id) => self.visit_module_import(id, tree),
            Body(id) => self.visit_module_body(id, tree),
            Path(id) => self.visit_module_path(id, tree),
            FunctorApp(id) => self.visit_functor_app(id, tree),
        }
    }
    fn visit_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_expr(id, tree)
    }

    fn visit_vis(&mut self, _id: Id<node::Vis>, _tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        } = *tree.get(id);
        self.visit_vis(vis, tree)?;
        self.visit_value_name(name, tree)?;
        if let Some(ts) = ty_scheme {
            self.visit_type_scheme(ts, tree)?;
        }
        self.visit_expr(value, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_bind(id, tree)
    }

    fn walk_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind {
            vis,
            name,
            ty_scheme,
        } = *tree.get(id);
        self.visit_vis(vis, tree)?;
        self.visit_type_name(name, tree)?;
        self.visit_type_scheme(ty_scheme, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_bind(id, tree)
    }

    fn walk_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty,
            value,
        } = *tree.get(id);
        self.visit_vis(vis, tree)?;
        self.visit_module_name(name, tree)?;
        if let Some(t) = ty {
            self.visit_module_type(t, tree)?;
        }
        self.visit_module_expr(value, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_bind(id, tree)
    }

    fn walk_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { vis, name, ty } = *tree.get(id);
        self.visit_vis(vis, tree)?;
        self.visit_module_type_name(name, tree)?;
        self.visit_module_type(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type_bind(id, tree)
    }

    fn walk_functor_param(
        &mut self,
        id: Id<node::FunctorParam>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorParam { name, ty } = *tree.get(id);
        self.visit_module_name(name, tree)?;
        self.visit_module_type(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_param(
        &mut self,
        id: Id<node::FunctorParam>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_param(id, tree)
    }

    fn walk_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorBind {
            vis,
            name,
            params,
            body,
        } = tree.get(id);
        self.visit_vis(*vis, tree)?;
        self.visit_functor_name(*name, tree)?;
        for param_id in tree.get_slice(*params).iter().copied() {
            self.visit_functor_param(param_id, tree)?;
        }
        self.visit_module_body(*body, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_bind(id, tree)
    }

    fn visit_bind_error(
        &mut self,
        _id: Id<node::BindError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_bind(&mut self, id: Id<node::Bind>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        use node::Bind::*;
        match *tree.get(id) {
            Value(id) => self.visit_value_bind(id, tree),
            Type(id) => self.visit_type_bind(id, tree),
            Module(id) => self.visit_module_bind(id, tree),
            ModuleType(id) => self.visit_module_type_bind(id, tree),
            Functor(id) => self.visit_functor_bind(id, tree),
            Error(id) => self.visit_bind_error(id, tree),
        }
    }
    fn visit_bind(&mut self, id: Id<node::Bind>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        self.walk_bind(id, tree)
    }

    fn visit_spec_error(
        &mut self,
        _id: Id<node::SpecError>,
        _tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueSpec { name, ty } = *tree.get(id);
        self.visit_value_name(name, tree)?;
        self.visit_type_scheme(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_spec(id, tree)
    }

    fn walk_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleSpec { name, ty } = *tree.get(id);
        self.visit_module_name(name, tree)?;
        self.visit_module_type(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_spec(id, tree)
    }

    fn walk_spec(&mut self, id: Id<node::Spec>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        use node::Spec::*;
        match *tree.get(id) {
            Value(id) => self.visit_value_spec(id, tree),
            Module(id) => self.visit_module_spec(id, tree),
            Error(id) => self.visit_spec_error(id, tree),
        }
    }
    fn visit_spec(&mut self, id: Id<node::Spec>, tree: &Tree<M>) -> ControlFlow<Self::BreakValue> {
        self.walk_spec(id, tree)
    }

    fn walk_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        for spec_id in tree.get_slice(id.get(tree).0).iter().copied() {
            self.visit_spec(spec_id, tree)?;
        }
        ControlFlow::Continue(())
    }
    fn visit_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_concrete_module_type(id, tree)
    }

    fn walk_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *tree.get(id);
        if let Some(p) = path {
            self.visit_module_path(p, tree)?;
        }
        self.visit_module_type_name(ty, tree)?;
        ControlFlow::Continue(())
    }
    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_module_type(id, tree)
    }

    fn walk_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleType::*;
        match *tree.get(id) {
            Concrete(id) => self.visit_concrete_module_type(id, tree),
            Qualified(id) => self.visit_qualified_module_type(id, tree),
        }
    }
    fn visit_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &Tree<M>,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type(id, tree)
    }
}
