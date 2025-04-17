pub use event::*;
pub use id::*;

use crate::{
    id::NodeId,
    node::{self, Handler},
    tree::Tree,
};

mod event;
mod id;

// TODO use Arena for EventStack creation or just reuse it (probably hard I need a subslice) ??

pub trait Visitor: Handler {
    fn visit_post_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        for (id, node) in tree.iter_nodes().enumerate() {
            self.handle_node(node, id)?;
        }

        Ok(())
    }

    fn visit_pre_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        for (id, node) in tree.iter_nodes().enumerate().rev() {
            self.handle_node(node, id)?;
        }

        Ok(())
    }

    fn visit_level_order(&mut self, tree: &Tree) -> Result<(), Self::Error> {
        let root_id = tree.root_id();
        self.visit_module(root_id, tree)
    }

    fn visit_module(&mut self, id: NodeId<node::Module>, tree: &Tree) -> Result<(), Self::Error> {
        let mut stack = EventStack::new();
        stack.push_branch(id);

        while let Some(event) = stack.next() {
            self.handle(event, tree, &mut stack)?;
        }

        Ok(())
    }

    fn handle(
        &mut self,
        event: Event,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        match event {
            Event::Enter(id) => match id {
                // Pattern nodes
                BranchId::RecordFieldPat(id) => self.walk_record_field_pat(id, tree, stack),
                BranchId::RecordPat(id) => self.walk_record_pat(id, tree, stack),
                BranchId::VariantCasePat(id) => self.walk_variant_case_pat(id, tree, stack),
                BranchId::VariantPat(id) => self.walk_variant_pat(id, tree, stack),
                BranchId::Pat(id) => self.walk_pat(id, tree, stack),
                BranchId::CaseBranch(id) => self.walk_case_branch(id, tree, stack),

                // Expression nodes
                BranchId::Path(id) => self.walk_path_expr(id, tree, stack),
                BranchId::List(id) => self.walk_list_expr(id, tree, stack),
                BranchId::RecordField(id) => self.walk_record_field(id, tree, stack),
                BranchId::Record(id) => self.walk_record_expr(id, tree, stack),
                BranchId::RecordExtend(id) => self.walk_record_extend_expr(id, tree, stack),
                BranchId::RecordRestrict(id) => self.walk_record_restrict_expr(id, tree, stack),
                BranchId::RecordUpdate(id) => self.walk_record_update_expr(id, tree, stack),
                BranchId::Unary(id) => self.walk_unary_expr(id, tree, stack),
                BranchId::Binary(id) => self.walk_binary_expr(id, tree, stack),
                BranchId::Let(id) => self.walk_let_expr(id, tree, stack),
                BranchId::Case(id) => self.walk_case_expr(id, tree, stack),
                BranchId::If(id) => self.walk_if_expr(id, tree, stack),
                BranchId::Lambda(id) => self.walk_lambda_expr(id, tree, stack),
                BranchId::Call(id) => self.walk_call_expr(id, tree, stack),
                BranchId::Expr(id) => self.walk_expr(id, tree, stack),

                // Type nodes
                BranchId::TypePath(id) => self.walk_type_path(id, tree, stack),
                BranchId::RecordFieldType(id) => self.walk_record_field_type(id, tree, stack),
                BranchId::RecordType(id) => self.walk_record_type(id, tree, stack),
                BranchId::VariantCaseType(id) => self.walk_variant_case_type(id, tree, stack),
                BranchId::VariantType(id) => self.walk_variant_type(id, tree, stack),
                BranchId::FuncType(id) => self.walk_func_type(id, tree, stack),
                BranchId::TypeApplication(id) => self.walk_type_application(id, tree, stack),
                BranchId::TypeExpr(id) => self.walk_type_expr(id, tree, stack),
                BranchId::Type(id) => self.walk_type(id, tree, stack),

                // Module nodes
                BranchId::ValueBind(id) => self.walk_value_bind(id, tree, stack),
                BranchId::TypeBind(id) => self.walk_type_bind(id, tree, stack),
                BranchId::OpaqueTypeBind(id) => self.walk_opaque_type_bind(id, tree, stack),
                BranchId::ModuleBind(id) => self.walk_module_bind(id, tree, stack),
                BranchId::ModuleTypeBind(id) => self.walk_module_type_bind(id, tree, stack),
                BranchId::Bind(id) => self.walk_bind(id, tree, stack),
                BranchId::Module(id) => self.walk_module(id, tree, stack),
                BranchId::ValueSpec(id) => self.walk_value_spec(id, tree, stack),
                BranchId::OpaqueTypeSpec(id) => self.walk_opaque_type_spec(id, tree, stack),
                BranchId::ModuleSpec(id) => self.walk_module_spec(id, tree, stack),
                BranchId::Spec(id) => self.walk_spec(id, tree, stack),
                BranchId::ModuleType(id) => self.walk_module_type(id, tree, stack),
            },
            Event::Exit(id) => match id {
                // Pattern nodes
                BranchId::RecordFieldPat(id) => self.handle_record_field_pat(id.get(tree), id),
                BranchId::RecordPat(id) => self.handle_record_pat(id.get(tree), id),
                BranchId::VariantCasePat(id) => self.handle_variant_case_pat(id.get(tree), id),
                BranchId::VariantPat(id) => self.handle_variant_pat(id.get(tree), id),
                BranchId::Pat(id) => self.handle_pat(id.get(tree), id),
                BranchId::CaseBranch(id) => self.handle_case_branch(id.get(tree), id),

                // Expression nodes
                BranchId::Path(id) => self.handle_path_expr(id.get(tree), id),
                BranchId::List(id) => self.handle_list_expr(id.get(tree), id),
                BranchId::RecordField(id) => self.handle_record_field(id.get(tree), id),
                BranchId::Record(id) => self.handle_record_expr(id.get(tree), id),
                BranchId::RecordExtend(id) => self.handle_record_extend_expr(id.get(tree), id),
                BranchId::RecordRestrict(id) => self.handle_record_restrict_expr(id.get(tree), id),
                BranchId::RecordUpdate(id) => self.handle_record_update_expr(id.get(tree), id),
                BranchId::Unary(id) => self.handle_unary_expr(id.get(tree), id),
                BranchId::Binary(id) => self.handle_binary_expr(id.get(tree), id),
                BranchId::Let(id) => self.handle_let_expr(id.get(tree), id),
                BranchId::Case(id) => self.handle_case_expr(id.get(tree), id),
                BranchId::If(id) => self.handle_if_expr(id.get(tree), id),
                BranchId::Lambda(id) => self.handle_lambda_expr(id.get(tree), id),
                BranchId::Call(id) => self.handle_call_expr(id.get(tree), id),
                BranchId::Expr(id) => self.handle_expr(id.get(tree), id),

                // Type nodes
                BranchId::TypePath(id) => self.handle_type_path(id.get(tree), id),
                BranchId::RecordFieldType(id) => self.handle_record_field_type(id.get(tree), id),
                BranchId::RecordType(id) => self.handle_record_type(id.get(tree), id),
                BranchId::VariantCaseType(id) => self.handle_variant_case_type(id.get(tree), id),
                BranchId::VariantType(id) => self.handle_variant_type(id.get(tree), id),
                BranchId::FuncType(id) => self.handle_func_type(id.get(tree), id),
                BranchId::TypeApplication(id) => self.handle_type_application(id.get(tree), id),
                BranchId::TypeExpr(id) => self.handle_type_expr(id.get(tree), id),
                BranchId::Type(id) => self.handle_type(id.get(tree), id),

                // Module nodes
                BranchId::ValueBind(id) => self.handle_value_bind(id.get(tree), id),
                BranchId::TypeBind(id) => self.handle_type_bind(id.get(tree), id),
                BranchId::OpaqueTypeBind(id) => self.handle_opaque_type_bind(id.get(tree), id),
                BranchId::ModuleBind(id) => self.handle_module_bind(id.get(tree), id),
                BranchId::ModuleTypeBind(id) => self.handle_module_type_bind(id.get(tree), id),
                BranchId::Bind(id) => self.handle_bind(id.get(tree), id),
                BranchId::Module(id) => self.handle_module(id.get(tree), id),
                BranchId::ValueSpec(id) => self.handle_value_spec(id.get(tree), id),
                BranchId::OpaqueTypeSpec(id) => self.handle_opaque_type_spec(id.get(tree), id),
                BranchId::ModuleSpec(id) => self.handle_module_spec(id.get(tree), id),
                BranchId::Spec(id) => self.handle_spec(id.get(tree), id),
                BranchId::ModuleType(id) => self.handle_module_type(id.get(tree), id),
            },
            Event::Visit(id) => match id {
                // Pattern nodes
                LeafId::AnyPat(id) => self.handle_any_pat(id.get(tree), id),
                LeafId::LiteralPat(id) => self.handle_literal_pat(id.get(tree), id),
                LeafId::IdentPat(id) => self.handle_ident_pat(id.get(tree), id),
                LeafId::PatError(id) => self.handle_pat_error(id.get(tree), id),

                // Expression nodes
                LeafId::Name(id) => self.handle_name(id.get(tree), id),
                LeafId::Literal(id) => self.handle_literal_expr(id.get(tree), id),
                LeafId::UnaryOp(id) => self.handle_unary_op(id.get(tree), id),
                LeafId::BinaryOp(id) => self.handle_binary_op(id.get(tree), id),
                LeafId::RecordUpdateOp(id) => self.handle_record_update_op(id.get(tree), id),
                LeafId::ExprError(id) => self.handle_expr_error(id.get(tree), id),

                // Type nodes
                LeafId::TypeVar(id) => self.handle_type_var(id.get(tree), id),
                LeafId::TypeError(id) => self.handle_type_error(id.get(tree), id),

                // Module nodes
                LeafId::OpaqueTypeKind(id) => self.handle_opaque_type_kind(id.get(tree), id),
            },
        }
    }

    fn walk_list_expr(
        &mut self,
        id: NodeId<node::ListExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let list = id.get(tree);

        for id in &list.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_path_expr(
        &mut self,
        id: NodeId<node::PathExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let path = id.get(tree);

        for id in &path.0 {
            stack.push_leaf(*id);
        }

        Ok(())
    }

    fn walk_record_field(
        &mut self,
        id: NodeId<node::RecordField>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordField { field, value } = id.get(tree);

        stack.push_leaf(*field);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_record_expr(
        &mut self,
        id: NodeId<node::RecordExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let record = id.get(tree);

        for id in &record.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_record_extend_expr(
        &mut self,
        id: NodeId<node::RecordExtendExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordExtendExpr {
            source,
            field,
            value,
        } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_record_restrict_expr(
        &mut self,
        id: NodeId<node::RecordRestrictExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordRestrictExpr { source, field } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);

        Ok(())
    }

    fn walk_record_update_expr(
        &mut self,
        id: NodeId<node::RecordUpdateExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordUpdateExpr {
            source,
            field,
            op,
            value,
        } = id.get(tree);

        stack.push_branch(*source);
        stack.push_leaf(*field);
        stack.push_leaf(*op);
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_unary_expr(
        &mut self,
        id: NodeId<node::UnaryExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::UnaryExpr { op, operand } = id.get(tree);

        stack.push_leaf(*op);
        stack.push_branch(*operand);

        Ok(())
    }

    fn walk_binary_expr(
        &mut self,
        id: NodeId<node::BinaryExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::BinaryExpr { op, left, right } = id.get(tree);

        stack.push_leaf(*op);
        stack.push_branch(*left);
        stack.push_branch(*right);

        Ok(())
    }

    fn walk_let_expr(
        &mut self,
        id: NodeId<node::LetExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::LetExpr {
            name,
            value,
            inside,
        } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*value);
        stack.push_branch(*inside);

        Ok(())
    }

    fn walk_if_expr(
        &mut self,
        id: NodeId<node::IfExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::IfExpr {
            predicate,
            then,
            or,
        } = id.get(tree);

        stack.push_branch(*predicate);
        stack.push_branch(*then);
        stack.push_branch(*or);

        Ok(())
    }

    fn walk_record_field_pat(
        &mut self,
        id: NodeId<node::RecordFieldPat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordFieldPat { field, pat } = id.get(tree);

        stack.push_leaf(*field);
        if let Some(value) = pat {
            stack.push_branch(*value);
        }

        Ok(())
    }

    fn walk_record_pat(
        &mut self,
        id: NodeId<node::RecordPat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let record_pat = id.get(tree);

        for id in &record_pat.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_variant_case_pat(
        &mut self,
        id: NodeId<node::VariantCasePat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::VariantCasePat { case, pat } = id.get(tree);

        stack.push_leaf(*case);
        if let Some(value) = pat {
            stack.push_branch(*value);
        }

        Ok(())
    }

    fn walk_variant_pat(
        &mut self,
        id: NodeId<node::VariantPat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let variant_pat = id.get(tree);

        for id in &variant_pat.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_pat(
        &mut self,
        id: NodeId<node::Pat>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Pat::*;

        match *id.get(tree) {
            Error(id) => stack.push_leaf(id),
            Any(id) => stack.push_leaf(id),
            Literal(id) => stack.push_leaf(id),
            Ident(id) => stack.push_leaf(id),
            Record(id) => stack.push_branch(id),
            Variant(id) => stack.push_branch(id),
        }

        Ok(())
    }

    fn walk_case_branch(
        &mut self,
        id: NodeId<node::CaseBranch>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CaseBranch { pat, matches } = id.get(tree);

        stack.push_branch(*pat);
        stack.push_branch(*matches);

        Ok(())
    }

    fn walk_case_expr(
        &mut self,
        id: NodeId<node::CaseExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CaseExpr { source, branches } = id.get(tree);

        stack.push_branch(*source);
        for id in branches {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_lambda_expr(
        &mut self,
        id: NodeId<node::LambdaExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::LambdaExpr { param, body } = id.get(tree);

        stack.push_leaf(*param);
        stack.push_branch(*body);

        Ok(())
    }

    fn walk_call_expr(
        &mut self,
        id: NodeId<node::CallExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::CallExpr { func, arg } = id.get(tree);

        stack.push_branch(*func);
        stack.push_branch(*arg);

        Ok(())
    }

    fn walk_expr(
        &mut self,
        id: NodeId<node::Expr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Expr::*;

        match *id.get(tree) {
            Error(id) => stack.push_leaf(id),
            Literal(id) => stack.push_leaf(id),
            Path(id) => stack.push_branch(id),
            List(id) => stack.push_branch(id),
            Record(id) => stack.push_branch(id),
            RecordExtend(id) => stack.push_branch(id),
            RecordRestrict(id) => stack.push_branch(id),
            RecordUpdate(id) => stack.push_branch(id),
            Unary(id) => stack.push_branch(id),
            Binary(id) => stack.push_branch(id),
            Let(id) => stack.push_branch(id),
            If(id) => stack.push_branch(id),
            Case(id) => stack.push_branch(id),
            Lambda(id) => stack.push_branch(id),
            Call(id) => stack.push_branch(id),
        }

        Ok(())
    }

    // Type-related walking functions
    fn walk_type_path(
        &mut self,
        id: NodeId<node::TypePath>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let type_path = id.get(tree);

        for id in &type_path.0 {
            stack.push_leaf(*id);
        }

        Ok(())
    }

    fn walk_record_field_type(
        &mut self,
        id: NodeId<node::RecordFieldType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordFieldType { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_record_type(
        &mut self,
        id: NodeId<node::RecordType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::RecordType { fields, extension } = id.get(tree);

        for id in fields {
            stack.push_branch(*id);
        }

        if let Some(ext) = extension {
            stack.push_leaf(*ext);
        }

        Ok(())
    }

    fn walk_variant_case_type(
        &mut self,
        id: NodeId<node::VariantCaseType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::VariantCaseType { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        if let Some(ty) = ty {
            stack.push_branch(*ty);
        }

        Ok(())
    }

    fn walk_variant_type(
        &mut self,
        id: NodeId<node::VariantType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::VariantType { cases, extension } = id.get(tree);

        for id in cases {
            stack.push_branch(*id);
        }

        if let Some(ext) = extension {
            stack.push_leaf(*ext);
        }

        Ok(())
    }

    fn walk_func_type(
        &mut self,
        id: NodeId<node::FuncType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::FuncType { input, output } = id.get(tree);

        stack.push_branch(*input);
        stack.push_branch(*output);

        Ok(())
    }

    fn walk_type_application(
        &mut self,
        id: NodeId<node::TypeApplication>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::TypeApplication { constructor, arg } = id.get(tree);

        stack.push_branch(*constructor);
        stack.push_branch(*arg);

        Ok(())
    }

    fn walk_type_expr(
        &mut self,
        id: NodeId<node::TypeExpr>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::TypeExpr::*;

        match *id.get(tree) {
            Error(id) => stack.push_leaf(id),
            Path(id) => stack.push_branch(id),
            Record(id) => stack.push_branch(id),
            Variant(id) => stack.push_branch(id),
            Func(id) => stack.push_branch(id),
            Application(id) => stack.push_branch(id),
        }

        Ok(())
    }

    fn walk_type(
        &mut self,
        id: NodeId<node::Type>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::Type { vars, ty } = id.get(tree);

        for var in vars {
            stack.push_leaf(*var);
        }
        stack.push_branch(*ty);

        Ok(())
    }

    // Module-related walking functions
    fn walk_value_bind(
        &mut self,
        id: NodeId<node::ValueBind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::ValueBind { name, ty, value } = id.get(tree);

        stack.push_leaf(*name);
        if let Some(ty) = ty {
            stack.push_branch(*ty);
        }
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_type_bind(
        &mut self,
        id: NodeId<node::TypeBind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::TypeBind { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_opaque_type_bind(
        &mut self,
        id: NodeId<node::OpaqueTypeBind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::OpaqueTypeBind { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_module_bind(
        &mut self,
        id: NodeId<node::ModuleBind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::ModuleBind { name, ty, value } = id.get(tree);

        stack.push_leaf(*name);
        if let Some(ty) = ty {
            stack.push_branch(*ty);
        }
        stack.push_branch(*value);

        Ok(())
    }

    fn walk_module_type_bind(
        &mut self,
        id: NodeId<node::ModuleTypeBind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::ModuleTypeBind { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_bind(
        &mut self,
        id: NodeId<node::Bind>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Bind::*;

        match *id.get(tree) {
            Value(id) => stack.push_branch(id),
            Type(id) => stack.push_branch(id),
            OpaqueType(id) => stack.push_branch(id),
            Module(id) => stack.push_branch(id),
            ModuleType(id) => stack.push_branch(id),
        }

        Ok(())
    }

    fn walk_module(
        &mut self,
        id: NodeId<node::Module>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let module = id.get(tree);

        for id in &module.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }

    fn walk_value_spec(
        &mut self,
        id: NodeId<node::ValueSpec>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::ValueSpec { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_opaque_type_spec(
        &mut self,
        id: NodeId<node::OpaqueTypeSpec>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::OpaqueTypeSpec { name, kind } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_leaf(*kind);

        Ok(())
    }

    fn walk_module_spec(
        &mut self,
        id: NodeId<node::ModuleSpec>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let node::ModuleSpec { name, ty } = id.get(tree);

        stack.push_leaf(*name);
        stack.push_branch(*ty);

        Ok(())
    }

    fn walk_spec(
        &mut self,
        id: NodeId<node::Spec>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        use node::Spec::*;

        match *id.get(tree) {
            Value(id) => stack.push_branch(id),
            TypeBind(id) => stack.push_branch(id),
            OpaqueType(id) => stack.push_branch(id),
            Module(id) => stack.push_branch(id),
        }

        Ok(())
    }

    fn walk_module_type(
        &mut self,
        id: NodeId<node::ModuleType>,
        tree: &Tree,
        stack: &mut EventStack,
    ) -> Result<(), Self::Error> {
        let module_type = id.get(tree);

        for id in &module_type.0 {
            stack.push_branch(*id);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::{self, NodeKind};
    use crate::tree::TreeBuilder;

    #[derive(Debug, Clone, Default)]
    struct TestVisitor {
        flow: Vec<NodeKind>,
    }

    impl Visitor for TestVisitor {}

    impl Handler for TestVisitor {
        type Error = ();

        fn handle_expr(
            &mut self,
            _expr: &node::Expr,
            _id: NodeId<node::Expr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::Expr);
            Ok(())
        }

        fn handle_unary_expr(
            &mut self,
            _unary: &node::UnaryExpr,
            _id: NodeId<node::UnaryExpr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::UnaryExpr);
            Ok(())
        }

        fn handle_literal_expr(
            &mut self,
            _literal: &node::LiteralExpr,
            _id: NodeId<node::LiteralExpr>,
        ) -> Result<(), Self::Error> {
            self.flow.push(NodeKind::LiteralExpr);
            Ok(())
        }
    }

    #[test]
    fn visitor() {
        let mut builder = TreeBuilder::new();

        let target = builder.insert(node::LiteralExpr::Num(10.0));
        let unary = node::UnaryExpr::new_in(node::UnaryOp::Neg, target.into(), &mut builder);
        let bind =
            node::Bind::value_in("name".into(), None, node::Expr::Unary(unary), &mut builder);
        let root = builder.insert(node::Module(vec![bind]));

        // -10
        let tree = builder.finish(root);

        let mut visitor = TestVisitor::default();
        visitor.visit_post_order(&tree).unwrap();
        assert_eq!(
            visitor.flow,
            vec![
                NodeKind::LiteralExpr,
                NodeKind::Expr,
                NodeKind::UnaryExpr,
                NodeKind::Expr,
            ]
        );

        let mut visitor = TestVisitor::default();
        visitor.visit_pre_order(&tree).unwrap();
        assert_eq!(
            visitor.flow,
            vec![
                NodeKind::Expr,
                NodeKind::UnaryExpr,
                NodeKind::Expr,
                NodeKind::LiteralExpr,
            ]
        );
    }
}
