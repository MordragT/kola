use paste::paste;
use std::ops::ControlFlow;

use crate::{id::Id, node, tree::TreeView};

pub trait Visitable<T: TreeView> {
    fn visit_by<V>(&self, visitor: &mut V, tree: &T) -> ControlFlow<V::BreakValue>
    where
        V: Visitor<T>;
}

macro_rules! impl_visitable {
    ($($variant:ident),* $(,)?) => {
        paste!{
            $(
                impl<T: TreeView> Visitable<T> for Id<node::$variant> {
                    fn visit_by<V>(&self, visitor: &mut V, tree: &T) -> ControlFlow<V::BreakValue>
                    where
                        V: Visitor<T>,
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
    EffectName,
    TypeName,
    ValueName,
    // Patterns
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
    // Expressions
    LiteralExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
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
    ExprError,
    Expr,
    // Types
    QualifiedEffectType,
    EffectOpType,
    EffectRowType,
    EffectType,
    QualifiedType,
    TypeVar,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    Type,
    TypeError,
    TypeVarBind,
    TypeScheme,
    // Modules
    Vis,
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    EffectTypeBind,
    ModuleBind,
    ModuleTypeBind,
    FunctorBind,
    Bind,
    ModuleError,
    Module,
    ModulePath,
    ModuleImport,
    FunctorApp,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType
);

pub trait Visitor<T: TreeView> {
    type BreakValue;

    fn visit_functor_name(
        &mut self,
        _id: Id<node::FunctorName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_module_type_name(
        &mut self,
        _id: Id<node::ModuleTypeName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_module_name(
        &mut self,
        _id: Id<node::ModuleName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_kind_name(
        &mut self,
        _id: Id<node::KindName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_effect_name(
        &mut self,
        _id: Id<node::EffectName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_type_name(
        &mut self,
        _id: Id<node::TypeName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_value_name(
        &mut self,
        _id: Id<node::ValueName>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_any_pat(&mut self, _id: Id<node::AnyPat>, _tree: &T) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_literal_pat(
        &mut self,
        _id: Id<node::LiteralPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_bind_pat(
        &mut self,
        _id: Id<node::BindPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_el_pat(
        &mut self,
        id: Id<node::ListElPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
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
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_el_pat(id, tree)
    }

    fn walk_list_pat(&mut self, id: Id<node::ListPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let list_pat = id.get(tree);

        for element_id in &list_pat.0 {
            self.visit_list_el_pat(*element_id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_list_pat(&mut self, id: Id<node::ListPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_list_pat(id, tree)
    }

    fn walk_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldPat { field, pat } = id.get(tree);

        self.visit_value_name(*field, tree)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_pat(id, tree)
    }

    fn walk_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let record_pat = id.get(tree);

        for field_id in &record_pat.fields {
            self.visit_record_field_pat(*field_id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_pat(id, tree)
    }

    fn walk_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantTagPat { tag, pat } = id.get(tree);

        self.visit_value_name(*tag, tree)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_tag_pat(
        &mut self,
        id: Id<node::VariantTagPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_tag_pat(id, tree)
    }

    fn walk_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let variant_pat = id.get(tree);

        for id in variant_pat {
            self.visit_variant_tag_pat(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_pat(id, tree)
    }

    fn visit_pat_error(
        &mut self,
        _id: Id<node::PatError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_pat(&mut self, id: Id<node::Pat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Pat::*;

        match *id.get(tree) {
            Error(id) => self.visit_pat_error(id, tree),
            Any(id) => self.visit_any_pat(id, tree),
            Literal(id) => self.visit_literal_pat(id, tree),
            Bind(id) => self.visit_bind_pat(id, tree),
            List(id) => self.visit_list_pat(id, tree),
            Record(id) => self.visit_record_pat(id, tree),
            Variant(id) => self.visit_variant_pat(id, tree),
        }
    }

    fn visit_pat(&mut self, id: Id<node::Pat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_pat(id, tree)
    }

    fn visit_literal_expr(
        &mut self,
        _id: Id<node::LiteralExpr>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let list = id.get(tree);

        for id in list {
            self.visit_expr(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_list_expr(
        &mut self,
        id: Id<node::ListExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_list_expr(id, tree)
    }

    fn walk_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordField { label, ty, value } = id.get(tree);

        self.visit_value_name(*label, tree)?;

        if let Some(ty) = ty {
            self.visit_type(*ty, tree)?;
        }

        self.visit_expr(*value, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field(id, tree)
    }

    fn walk_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let record = id.get(tree);

        for id in record {
            self.visit_record_field(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_expr(
        &mut self,
        id: Id<node::RecordExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_expr(id, tree)
    }

    fn visit_record_update_op(
        &mut self,
        _id: Id<node::RecordUpdateOp>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordExtendExpr {
            source,
            source_type,
            field_path,
            value,
            value_type,
        } = *id.get(tree);

        self.visit_expr(source, tree)?;

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
        }

        self.visit_field_path(field_path, tree)?;
        self.visit_expr(value, tree)?;

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_extend_expr(
        &mut self,
        id: Id<node::RecordExtendExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_extend_expr(id, tree)
    }

    fn walk_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordRestrictExpr {
            source,
            source_type,
            field_path,
            value_type,
        } = *id.get(tree);

        self.visit_expr(source, tree)?;

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
        }

        self.visit_field_path(field_path, tree)?;

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_restrict_expr(
        &mut self,
        id: Id<node::RecordRestrictExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_restrict_expr(id, tree)
    }

    fn walk_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordUpdateExpr {
            source,
            source_type,
            field_path,
            op,
            value,
            value_type,
        } = *id.get(tree);

        self.visit_expr(source, tree)?;

        if let Some(type_) = source_type {
            self.visit_type(type_, tree)?;
        }

        self.visit_field_path(field_path, tree)?;
        self.visit_record_update_op(op, tree)?;
        self.visit_expr(value, tree)?;

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_update_expr(id, tree)
    }

    fn walk_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordMergeExpr { lhs, rhs } = *id.get(tree);

        self.visit_expr(lhs, tree)?;
        self.visit_expr(rhs, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_merge_expr(
        &mut self,
        id: Id<node::RecordMergeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_merge_expr(id, tree)
    }

    fn walk_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        for field in id.get(tree) {
            self.visit_value_name(*field, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_field_path(
        &mut self,
        id: Id<node::FieldPath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_field_path(id, tree)
    }

    fn walk_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            field_path,
        } = *id.get(tree);

        if let Some(path) = module_path {
            self.visit_module_path(path, tree)?;
        }

        self.visit_value_name(source, tree)?;

        if let Some(path) = field_path {
            self.visit_field_path(path, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_expr(id, tree)
    }

    fn visit_unary_op(
        &mut self,
        _id: Id<node::UnaryOp>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::UnaryExpr { op, operand } = *id.get(tree);

        self.visit_unary_op(op, tree)?;
        self.visit_expr(operand, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_unary_expr(
        &mut self,
        id: Id<node::UnaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_unary_expr(id, tree)
    }

    fn visit_binary_op(
        &mut self,
        _id: Id<node::BinaryOp>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::BinaryExpr { op, lhs, rhs } = *id.get(tree);

        self.visit_binary_op(op, tree)?;
        self.visit_expr(lhs, tree)?;
        self.visit_expr(rhs, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_binary_expr(
        &mut self,
        id: Id<node::BinaryExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_binary_expr(id, tree)
    }

    fn walk_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *id.get(tree);

        self.visit_value_name(name, tree)?;

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
        }

        self.visit_expr(value, tree)?;
        self.visit_expr(body, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_let_expr(id, tree)
    }

    fn walk_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseBranch { pat, body } = *id.get(tree);

        self.visit_pat(pat, tree)?;
        self.visit_expr(body, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_branch(id, tree)
    }

    fn walk_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CaseExpr { source, branches } = id.get(tree);

        self.visit_expr(*source, tree)?;
        for id in branches {
            self.visit_case_branch(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_case_expr(
        &mut self,
        id: Id<node::CaseExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_case_expr(id, tree)
    }

    fn walk_if_expr(&mut self, id: Id<node::IfExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::IfExpr {
            pred,
            then,
            or_else,
        } = *id.get(tree);

        self.visit_expr(pred, tree)?;
        self.visit_expr(then, tree)?;
        self.visit_expr(or_else, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_if_expr(&mut self, id: Id<node::IfExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_if_expr(id, tree)
    }

    fn walk_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = id.get(tree);

        self.visit_value_name(*param, tree)?;

        if let Some(param_type) = param_type {
            self.visit_type(*param_type, tree)?;
        }

        self.visit_expr(*body, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_lambda_expr(id, tree)
    }

    fn walk_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CallExpr { func, arg } = id.get(tree);

        self.visit_expr(*func, tree)?;
        self.visit_expr(*arg, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_call_expr(
        &mut self,
        id: Id<node::CallExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_call_expr(id, tree)
    }

    fn walk_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op, param, body } = *id.get(tree);

        self.visit_value_name(op, tree)?;
        self.visit_value_name(param, tree)?;
        self.visit_expr(body, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handler_clause(id, tree)
    }

    fn walk_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandleExpr { source, clauses } = id.get(tree);

        self.visit_expr(*source, tree)?;

        for clause in clauses {
            self.visit_handler_clause(*clause, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_handle_expr(
        &mut self,
        id: Id<node::HandleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_handle_expr(id, tree)
    }

    fn walk_do_expr(&mut self, id: Id<node::DoExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::DoExpr { op, arg } = *id.get(tree);

        self.visit_value_name(op, tree)?;
        self.visit_expr(arg, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_do_expr(&mut self, id: Id<node::DoExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_do_expr(id, tree)
    }

    fn walk_tag_expr(&mut self, id: Id<node::TagExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let tag = id.get(tree).0;

        self.visit_value_name(tag, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_tag_expr(&mut self, id: Id<node::TagExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_expr(id, tree)
    }

    fn walk_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
            node::TypeWitnessExpr::Qualified(id) => self.visit_qualified_type(id, tree),
            node::TypeWitnessExpr::Label(id) => self.visit_value_name(id, tree),
        }
    }

    fn visit_type_witness_expr(
        &mut self,
        id: Id<node::TypeWitnessExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_witness_expr(id, tree)
    }

    fn visit_expr_error(
        &mut self,
        _id: Id<node::ExprError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Expr::*;

        match *id.get(tree) {
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

    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)
    }

    fn walk_qualified_effect_type(
        &mut self,
        id: Id<node::QualifiedEffectType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedEffectType { path, ty } = *id.get(tree);

        if let Some(path) = path {
            self.visit_module_path(path, tree)?;
        }

        self.visit_effect_name(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_qualified_effect_type(
        &mut self,
        id: Id<node::QualifiedEffectType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_effect_type(id, tree)
    }

    fn walk_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::EffectOpType { name, ty } = *id.get(tree);

        self.visit_value_name(name, tree)?;
        self.visit_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_effect_op_type(
        &mut self,
        id: Id<node::EffectOpType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_op_type(id, tree)
    }

    fn walk_effect_row_type(
        &mut self,
        id: Id<node::EffectRowType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        for op in id.get(tree) {
            self.visit_effect_op_type(*op, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_effect_row_type(
        &mut self,
        id: Id<node::EffectRowType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_row_type(id, tree)
    }

    // fn walk_effect_op_type_scheme(
    //     &mut self,
    //     id: Id<node::EffectOpTypeScheme>,
    //     tree: &T,
    // ) -> ControlFlow<Self::BreakValue> {
    //     let node::EffectOpTypeScheme { name, ty_scheme } = *id.get(tree);

    //     self.visit_value_name(name, tree)?;
    //     self.visit_type_scheme(ty_scheme, tree)?;

    //     ControlFlow::Continue(())
    // }

    // fn visit_effect_op_type_scheme(
    //     &mut self,
    //     id: Id<node::EffectOpTypeScheme>,
    //     tree: &T,
    // ) -> ControlFlow<Self::BreakValue> {
    //     self.walk_effect_op_type_scheme(id, tree)
    // }

    // fn walk_effect_row_type_scheme(
    //     &mut self,
    //     id: Id<node::EffectRowTypeScheme>,
    //     tree: &T,
    // ) -> ControlFlow<Self::BreakValue> {
    //     for op in id.get(tree) {
    //         self.visit_effect_op_type_scheme(*op, tree)?;
    //     }

    //     ControlFlow::Continue(())
    // }

    // fn visit_effect_row_type_scheme(
    //     &mut self,
    //     id: Id<node::EffectRowTypeScheme>,
    //     tree: &T,
    // ) -> ControlFlow<Self::BreakValue> {
    //     self.walk_effect_row_type_scheme(id, tree)
    // }

    fn walk_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        match id.get(tree) {
            node::EffectType::Qualified(qual_id) => {
                self.visit_qualified_effect_type(*qual_id, tree)
            }
            node::EffectType::Row(row_id) => self.visit_effect_row_type(*row_id, tree),
        }
    }

    fn visit_effect_type(
        &mut self,
        id: Id<node::EffectType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_type(id, tree)
    }

    fn walk_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = id.get(tree);

        if let Some(path) = path {
            self.visit_module_path(*path, tree)?;
        }

        self.visit_type_name(*ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_type(id, tree)
    }

    fn visit_type_var(
        &mut self,
        _id: Id<node::TypeVar>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
            node::LabelOrVar::Label(name_id) => self.visit_value_name(name_id, tree),
            node::LabelOrVar::Var(var_id) => self.visit_type_var(var_id, tree),
        }
    }

    fn visit_label_or_var(
        &mut self,
        id: Id<node::LabelOrVar>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_label_or_var(id, tree)
    }

    fn walk_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { label_or_var, ty } = *id.get(tree);

        self.visit_label_or_var(label_or_var, tree)?;
        self.visit_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_field_type(id, tree)
    }

    fn walk_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordType { fields, extension } = id.get(tree);

        for id in fields {
            self.visit_record_field_type(*id, tree)?;
        }

        if let Some(ext) = extension {
            self.visit_type_name(*ext, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_record_type(
        &mut self,
        id: Id<node::RecordType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_type(id, tree)
    }

    fn walk_tag_type(&mut self, id: Id<node::TagType>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::TagType { name, ty } = id.get(tree);

        self.visit_value_name(*name, tree)?;
        if let Some(ty) = ty {
            self.visit_type(*ty, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_tag_type(&mut self, id: Id<node::TagType>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_tag_type(id, tree)
    }

    fn walk_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantType { tags, extension } = id.get(tree);

        for id in tags {
            self.visit_tag_type(*id, tree)?;
        }

        if let Some(ext) = extension {
            self.visit_type_name(*ext, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_type(id, tree)
    }

    fn walk_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FuncType { input, output } = id.get(tree);

        self.visit_type(*input, tree)?;
        self.visit_comp_type(*output, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_func_type(
        &mut self,
        id: Id<node::FuncType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_func_type(id, tree)
    }

    fn walk_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeApplication { constructor, arg } = id.get(tree);

        self.visit_type(*constructor, tree)?;
        self.visit_type(*arg, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_application(id, tree)
    }

    fn walk_comp_type(
        &mut self,
        id: Id<node::CompType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::CompType { effect, ty } = *id.get(tree);

        if let Some(effect) = effect {
            self.visit_effect_type(effect, tree)?;
        }
        self.visit_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_comp_type(
        &mut self,
        id: Id<node::CompType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_comp_type(id, tree)
    }

    fn visit_type_error(
        &mut self,
        _id: Id<node::TypeError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Type::*;

        match *id.get(tree) {
            Error(id) => self.visit_type_error(id, tree),
            Qualified(id) => self.visit_qualified_type(id, tree),
            Record(id) => self.visit_record_type(id, tree),
            Variant(id) => self.visit_variant_type(id, tree),
            Func(id) => self.visit_func_type(id, tree),
            Application(id) => self.visit_type_application(id, tree),
        }
    }

    fn visit_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_type(id, tree)
    }

    fn walk_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeVarBind { var, kind } = *id.get(tree);

        if let Some(kind) = kind {
            self.visit_kind_name(kind, tree)?;
        }

        self.visit_type_var(var, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_var_bind(id, tree)
    }

    fn walk_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        for bind in id.get(tree) {
            self.visit_type_var_bind(*bind, tree)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_forall_binder(
        &mut self,
        id: Id<node::ForallBinder>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_forall_binder(id, tree)
    }

    fn walk_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeScheme { forall, ty } = *id.get(tree);

        if let Some(forall) = forall {
            self.visit_forall_binder(forall, tree)?;
        }

        self.visit_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_scheme(id, tree)
    }

    fn visit_module_error(
        &mut self,
        _id: Id<node::ModuleError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module = id.get(tree);

        for id in module {
            self.visit_bind(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_module(id, tree)
    }

    fn walk_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_path = id.get(tree);

        for id in module_path {
            self.visit_module_name(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_path(id, tree)
    }

    fn walk_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let import = id.get(tree);
        self.visit_module_name(import.0, tree)
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_import(id, tree)
    }

    fn walk_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorApp { func, arg } = *id.get(tree);

        self.visit_functor_name(func, tree)?;
        self.visit_module_expr(arg, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_app(id, tree)
    }

    fn walk_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleExpr::*;

        match *id.get(tree) {
            Error(id) => self.visit_module_error(id, tree),
            Import(id) => self.visit_module_import(id, tree),
            Module(id) => self.visit_module(id, tree),
            Path(id) => self.visit_module_path(id, tree),
            FunctorApp(id) => self.visit_functor_app(id, tree),
        }
    }

    fn visit_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_expr(id, tree)
    }

    fn visit_vis(&mut self, _id: Id<node::Vis>, _tree: &T) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_value_name(name, tree)?;
        if let Some(ty_scheme) = ty_scheme {
            self.visit_type_scheme(ty_scheme, tree)?;
        }
        self.visit_expr(value, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_bind(id, tree)
    }

    fn walk_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind {
            vis,
            name,
            ty_scheme,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_type_name(name, tree)?;
        self.visit_type_scheme(ty_scheme, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_bind(id, tree)
    }

    fn walk_opaque_type_bind(
        &mut self,
        id: Id<node::OpaqueTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::OpaqueTypeBind {
            vis,
            name,
            ty_scheme,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_type_name(name, tree)?;
        self.visit_type_scheme(ty_scheme, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_opaque_type_bind(
        &mut self,
        id: Id<node::OpaqueTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_opaque_type_bind(id, tree)
    }

    fn walk_effect_type_bind(
        &mut self,
        id: Id<node::EffectTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::EffectTypeBind { vis, name, ty } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_effect_name(name, tree)?;
        self.visit_effect_row_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_effect_type_bind(
        &mut self,
        id: Id<node::EffectTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_effect_type_bind(id, tree)
    }

    fn walk_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty,
            value,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_module_name(name, tree)?;
        if let Some(ty) = ty {
            self.visit_module_type(ty, tree)?;
        }
        self.visit_module_expr(value, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_bind(id, tree)
    }

    fn walk_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { vis, name, ty } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_module_type_name(name, tree)?;
        self.visit_module_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type_bind(id, tree)
    }

    fn walk_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorBind {
            vis,
            name,
            param,
            param_ty,
            body,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_functor_name(name, tree)?;
        self.visit_module_name(param, tree)?;
        self.visit_module_type(param_ty, tree)?;
        self.visit_module(body, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_functor_bind(id, tree)
    }

    fn walk_bind(&mut self, id: Id<node::Bind>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Bind::*;

        match *id.get(tree) {
            Value(id) => self.visit_value_bind(id, tree),
            Type(id) => self.visit_type_bind(id, tree),
            OpaqueType(id) => self.visit_opaque_type_bind(id, tree),
            EffectType(id) => self.visit_effect_type_bind(id, tree),
            Module(id) => self.visit_module_bind(id, tree),
            ModuleType(id) => self.visit_module_type_bind(id, tree),
            Functor(id) => self.visit_functor_bind(id, tree),
        }
    }

    fn visit_bind(&mut self, id: Id<node::Bind>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_bind(id, tree)
    }

    fn walk_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueSpec { name, ty } = *id.get(tree);

        self.visit_value_name(name, tree)?;
        self.visit_type_scheme(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_value_spec(
        &mut self,
        id: Id<node::ValueSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_value_spec(id, tree)
    }

    fn visit_opaque_type_kind(
        &mut self,
        _id: Id<node::OpaqueTypeKind>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_opaque_type_spec(
        &mut self,
        id: Id<node::OpaqueTypeSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::OpaqueTypeSpec { name, kind } = *id.get(tree);

        self.visit_type_name(name, tree)?;
        self.visit_opaque_type_kind(kind, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_opaque_type_spec(
        &mut self,
        id: Id<node::OpaqueTypeSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_opaque_type_spec(id, tree)
    }

    fn walk_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleSpec { name, ty } = *id.get(tree);

        self.visit_module_name(name, tree)?;
        self.visit_module_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_module_spec(
        &mut self,
        id: Id<node::ModuleSpec>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_spec(id, tree)
    }

    fn walk_spec(&mut self, id: Id<node::Spec>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Spec::*;

        match *id.get(tree) {
            Value(id) => self.visit_value_spec(id, tree),
            TypeBind(id) => self.visit_type_bind(id, tree),
            OpaqueType(id) => self.visit_opaque_type_spec(id, tree),
            Module(id) => self.visit_module_spec(id, tree),
        }
    }

    fn visit_spec(&mut self, id: Id<node::Spec>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_spec(id, tree)
    }

    fn walk_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_type = id.get(tree);

        for id in module_type {
            self.visit_spec(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_concrete_module_type(
        &mut self,
        id: Id<node::ConcreteModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_concrete_module_type(id, tree)
    }

    fn walk_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *id.get(tree);

        if let Some(path) = path {
            self.visit_module_path(path, tree)?;
        }

        self.visit_module_type_name(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_qualified_module_type(id, tree)
    }

    fn walk_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        use node::ModuleType::*;

        match *id.get(tree) {
            Concrete(id) => self.visit_concrete_module_type(id, tree),
            Qualified(id) => self.visit_qualified_module_type(id, tree),
        }
    }

    fn visit_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type(id, tree)
    }
}
