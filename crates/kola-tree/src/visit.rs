use paste::paste;
use std::ops::ControlFlow;

use crate::{id::Id, node, tree::TreeAccess};

pub trait Visitable<T: TreeAccess> {
    fn visit_by<V>(&self, visitor: &mut V, tree: &T) -> ControlFlow<V::BreakValue>
    where
        V: Visitor<T>;
}

macro_rules! impl_visitable {
    ($($variant:ident),* $(,)?) => {
        paste!{
            $(
                impl<T: TreeAccess> Visitable<T> for Id<node::$variant> {
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
    Name,
    // Patterns
    AnyPat,
    LiteralPat,
    IdentPat,
    RecordFieldPat,
    RecordPat,
    VariantCasePat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    PathExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
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
    ExprError,
    Expr,
    // Types
    TypePath,
    TypeVar,
    RecordFieldType,
    RecordType,
    VariantCaseType,
    VariantType,
    FuncType,
    TypeApplication,
    TypeExpr,
    TypeError,
    Type,
    // Modules
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    ModuleBind,
    ModuleTypeBind,
    Bind,
    Module,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ModuleType
);

pub trait Visitor<T: TreeAccess> {
    type BreakValue;

    fn visit_name(&mut self, _id: Id<node::Name>, _tree: &T) -> ControlFlow<Self::BreakValue> {
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

    fn visit_ident_pat(
        &mut self,
        _id: Id<node::IdentPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_record_field_pat(
        &mut self,
        id: Id<node::RecordFieldPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldPat { field, pat } = id.get(tree);

        self.visit_name(*field, tree)?;
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

        for id in &record_pat.0 {
            self.visit_record_field_pat(*id, tree)?;
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

    fn walk_variant_case_pat(
        &mut self,
        id: Id<node::VariantCasePat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantCasePat { case, pat } = id.get(tree);

        self.visit_name(*case, tree)?;
        if let Some(pat) = pat {
            self.visit_pat(*pat, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_case_pat(
        &mut self,
        id: Id<node::VariantCasePat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_case_pat(id, tree)
    }

    fn walk_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let variant_pat = id.get(tree);

        for id in &variant_pat.0 {
            self.visit_variant_case_pat(*id, tree)?;
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
            Ident(id) => self.visit_ident_pat(id, tree),
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

        for id in &list.0 {
            self.visit_expr(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_list_expr(
        &mut self,
        _id: Id<node::ListExpr>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let path = id.get(tree);

        for id in &path.0 {
            self.visit_name(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_path_expr(id, tree)
    }

    fn walk_record_field(
        &mut self,
        id: Id<node::RecordField>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordField { field, value } = id.get(tree);

        self.visit_name(*field, tree)?;
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

        for id in &record.0 {
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
            field,
            value,
        } = id.get(tree);

        self.visit_expr(*source, tree)?;
        self.visit_name(*field, tree)?;
        self.visit_expr(*value, tree)?;

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
        let node::RecordRestrictExpr { source, field } = id.get(tree);

        self.visit_expr(*source, tree)?;
        self.visit_name(*field, tree)?;

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
            field,
            op,
            value,
        } = id.get(tree);

        self.visit_expr(*source, tree)?;
        self.visit_name(*field, tree)?;
        self.visit_record_update_op(*op, tree)?;
        self.visit_expr(*value, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_record_update_expr(
        &mut self,
        id: Id<node::RecordUpdateExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_record_update_expr(id, tree)
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
        let node::UnaryExpr { op, operand } = id.get(tree);

        self.visit_unary_op(*op, tree)?;
        self.visit_expr(*operand, tree)?;

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
        let node::BinaryExpr { op, left, right } = id.get(tree);

        self.visit_binary_op(*op, tree)?;
        self.visit_expr(*left, tree)?;
        self.visit_expr(*right, tree)?;

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
            value,
            inside,
        } = id.get(tree);

        self.visit_name(*name, tree)?;
        self.visit_expr(*value, tree)?;
        self.visit_expr(*inside, tree)?;

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
        let node::CaseBranch { pat, matches } = id.get(tree);

        self.visit_pat(*pat, tree)?;
        self.visit_expr(*matches, tree)?;

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
            predicate,
            then,
            or,
        } = id.get(tree);

        self.visit_expr(*predicate, tree)?;
        self.visit_expr(*then, tree)?;
        self.visit_expr(*or, tree)?;

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
        let node::LambdaExpr { param, body } = id.get(tree);

        self.visit_name(*param, tree)?;
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
            Path(id) => self.visit_path_expr(id, tree),
            List(id) => self.visit_list_expr(id, tree),
            Record(id) => self.visit_record_expr(id, tree),
            RecordExtend(id) => self.visit_record_extend_expr(id, tree),
            RecordRestrict(id) => self.visit_record_restrict_expr(id, tree),
            RecordUpdate(id) => self.visit_record_update_expr(id, tree),
            Unary(id) => self.visit_unary_expr(id, tree),
            Binary(id) => self.visit_binary_expr(id, tree),
            Let(id) => self.visit_let_expr(id, tree),
            If(id) => self.visit_if_expr(id, tree),
            Case(id) => self.visit_case_expr(id, tree),
            Lambda(id) => self.visit_lambda_expr(id, tree),
            Call(id) => self.visit_call_expr(id, tree),
        }
    }

    fn visit_expr(&mut self, id: Id<node::Expr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_expr(id, tree)
    }

    fn walk_type_path(
        &mut self,
        id: Id<node::TypePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let type_path = id.get(tree);

        for id in &type_path.0 {
            self.visit_name(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_type_path(
        &mut self,
        id: Id<node::TypePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_path(id, tree)
    }

    fn visit_type_var(
        &mut self,
        _id: Id<node::TypeVar>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_record_field_type(
        &mut self,
        id: Id<node::RecordFieldType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordFieldType { name, ty } = id.get(tree);

        self.visit_name(*name, tree)?;
        self.visit_type_expr(*ty, tree)?;

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
            self.visit_type_var(*ext, tree)?;
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

    fn walk_variant_case_type(
        &mut self,
        id: Id<node::VariantCaseType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantCaseType { name, ty } = id.get(tree);

        self.visit_name(*name, tree)?;
        if let Some(ty) = ty {
            self.visit_type_expr(*ty, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_case_type(
        &mut self,
        id: Id<node::VariantCaseType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_variant_case_type(id, tree)
    }

    fn walk_variant_type(
        &mut self,
        id: Id<node::VariantType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::VariantType { cases, extension } = id.get(tree);

        for id in cases {
            self.visit_variant_case_type(*id, tree)?;
        }

        if let Some(ext) = extension {
            self.visit_type_var(*ext, tree)?;
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

        self.visit_type_expr(*input, tree)?;
        self.visit_type_expr(*output, tree)?;

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

        self.visit_type_expr(*constructor, tree)?;
        self.visit_type_expr(*arg, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type_application(
        &mut self,
        id: Id<node::TypeApplication>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_application(id, tree)
    }

    fn visit_type_error(
        &mut self,
        _id: Id<node::TypeError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn walk_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        use node::TypeExpr::*;

        match *id.get(tree) {
            Error(id) => self.visit_type_error(id, tree),
            Path(id) => self.visit_type_path(id, tree),
            Record(id) => self.visit_record_type(id, tree),
            Variant(id) => self.visit_variant_type(id, tree),
            Func(id) => self.visit_func_type(id, tree),
            Application(id) => self.visit_type_application(id, tree),
        }
    }

    fn visit_type_expr(
        &mut self,
        id: Id<node::TypeExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_type_expr(id, tree)
    }

    fn walk_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::Type { vars, ty } = id.get(tree);

        for var in vars {
            self.visit_name(*var, tree)?;
        }
        self.visit_type_expr(*ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        self.walk_type(id, tree)
    }

    fn walk_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module = id.get(tree);

        for id in &module.0 {
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

        for id in &module_path.0 {
            self.visit_name(*id, tree)?;
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
        self.visit_name(import.0, tree)
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_import(id, tree)
    }

    fn walk_module_expr(
        &mut self,
        id: Id<node::ModuleExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
            node::ModuleExpr::Import(id) => self.visit_module_import(id, tree),
            node::ModuleExpr::Module(id) => self.visit_module(id, tree),
            node::ModuleExpr::Path(id) => self.visit_module_path(id, tree),
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
            ty,
            value,
        } = *id.get(tree);

        self.visit_vis(vis, tree)?;
        self.visit_name(name, tree)?;
        if let Some(ty) = ty {
            self.visit_type(ty, tree)?;
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
        let node::TypeBind { name, ty } = *id.get(tree);

        self.visit_name(name, tree)?;
        self.visit_type(ty, tree)?;

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
        let node::OpaqueTypeBind { name, ty } = *id.get(tree);

        self.visit_name(name, tree)?;
        self.visit_type(ty, tree)?;

        ControlFlow::Continue(())
    }

    fn visit_opaque_type_bind(
        &mut self,
        id: Id<node::OpaqueTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_opaque_type_bind(id, tree)
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
        self.visit_name(name, tree)?;
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
        let node::ModuleTypeBind { name, ty } = *id.get(tree);

        self.visit_name(name, tree)?;
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

    fn walk_bind(&mut self, id: Id<node::Bind>, tree: &T) -> ControlFlow<Self::BreakValue> {
        use node::Bind::*;

        match *id.get(tree) {
            Value(id) => self.visit_value_bind(id, tree),
            Type(id) => self.visit_type_bind(id, tree),
            OpaqueType(id) => self.visit_opaque_type_bind(id, tree),
            Module(id) => self.visit_module_bind(id, tree),
            ModuleType(id) => self.visit_module_type_bind(id, tree),
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

        self.visit_name(name, tree)?;
        self.visit_type(ty, tree)?;

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

        self.visit_name(name, tree)?;
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

        self.visit_name(name, tree)?;
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

    fn walk_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_type = id.get(tree);

        for id in &module_type.0 {
            self.visit_spec(*id, tree)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_module_type(
        &mut self,
        id: Id<node::ModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        self.walk_module_type(id, tree)
    }
}
