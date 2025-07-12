use kola_span::Loc;
use kola_tree::prelude::*;
use std::ops::ControlFlow;

use crate::{
    constraints::Constraints,
    env::LocalTypeEnv,
    types::{Kind, Label, LabeledType, MonoType},
};

pub struct PatternTyper<'a> {
    env: &'a mut LocalTypeEnv,
    cons: &'a mut Constraints,
    source: MonoType,
    span: Loc,
}

impl<'a> PatternTyper<'a> {
    pub fn new(
        env: &'a mut LocalTypeEnv,
        cons: &'a mut Constraints,
        source: MonoType,
        span: Loc,
    ) -> Self {
        Self {
            env,
            cons,
            source,
            span,
        }
    }

    pub fn run<T, N>(mut self, id: Id<N>, tree: &T)
    where
        T: TreeView,
        Id<N>: Visitable<T>,
    {
        ControlFlow::Continue(()) = id.visit_by(&mut self, tree)
    }
}

impl<'a, T> Visitor<T> for PatternTyper<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    /// **Error Pattern:**
    /// Pattern parsing error - no typing rules apply
    fn visit_pat_error(
        &mut self,
        _id: Id<node::PatError>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    /// **Wildcard Pattern:**
    /// ```ignore
    /// α fresh
    /// -----------------------
    /// ∆;Γ ⊢ _ ⇒ α ⊣ Γ
    /// ```
    /// Wildcard matches any type without binding variables.
    fn visit_any_pat(&mut self, _id: Id<node::AnyPat>, _tree: &T) -> ControlFlow<Self::BreakValue> {
        // Wildcard pattern matches any type - no constraints needed
        // The source type can be anything, and we don't bind any variables
        // This implements the rule: ∆;Γ ⊢ _ ⇒ α ⊣ Γ (where α is the source type)
        ControlFlow::Continue(())
    }

    /// **Literal Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ 42 ⇒ Num ⊣ Γ
    /// ∆;Γ ⊢ true ⇒ Bool ⊣ Γ
    /// ∆;Γ ⊢ () ⇒ Unit ⊣ Γ
    /// ```
    /// Literal patterns must match the source type exactly.
    fn visit_literal_pat(
        &mut self,
        id: Id<node::LiteralPat>,
        _tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Get the literal pattern and determine its type
        let literal_type = match *id.get(_tree) {
            node::LiteralPat::Unit => MonoType::UNIT,
            node::LiteralPat::Bool(_) => MonoType::BOOL,
            node::LiteralPat::Num(_) => MonoType::NUM,
            node::LiteralPat::Char(_) => MonoType::CHAR,
            node::LiteralPat::Str(_) => MonoType::STR,
        };

        // Constrain the source type to match the literal type exactly
        // This implements the rule: ∆;Γ ⊢ literal ⇒ literal_type ⊣ Γ
        self.cons
            .constrain_equal(self.source.clone(), literal_type, self.span);

        // Literal patterns don't bind any variables, so no environment changes
        ControlFlow::Continue(())
    }

    /// **Bind Pattern:**
    /// ```ignore
    /// α fresh
    /// -----------------------
    /// ∆;Γ ⊢ x ⇒ α ⊣ Γ[x : α]
    /// ```
    /// Bind pattern matches any type and binds the variable to that type.
    fn visit_bind_pat(&mut self, id: Id<node::BindPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        // Get the variable name from the bind pattern
        let var_name = id.get(tree).0.get(tree).0;

        // Bind the variable to the source type in the environment
        // This implements the rule: ∆;Γ ⊢ x ⇒ α ⊣ Γ[x : α]
        // where α is the source type and x is the variable name
        self.env.enter(var_name, self.source.clone());

        // No constraints needed - bind patterns accept any type
        // The environment is now extended with the new binding
        ControlFlow::Continue(())
    }

    /// **List Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ ⊣ Γ₂    ...    ∆;Γₙ₋₁ ⊢ pₙ ⇒ τ ⊣ Γₙ
    /// -----------------------
    /// ∆;Γ ⊢ [p₁, p₂, ..., pₙ] ⇒ List τ ⊣ Γₙ
    /// ```
    ///
    /// **List Spread Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ ⊣ Γ₂    ...    ∆;Γₙ₋₁ ⊢ pₙ ⇒ τ ⊣ Γₙ
    /// -----------------------
    /// ∆;Γ ⊢ [p₁, p₂, ..., pₙ, ...x] ⇒ List τ ⊣ Γₙ[x : List τ]
    /// ```
    fn visit_list_pat(&mut self, id: Id<node::ListPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        // Get the list elements from the pattern
        let elements = &id.get(tree).0;

        // Create a fresh type variable for the element type
        let element_type = MonoType::variable(Kind::Type);

        // Constrain the source type to be List(element_type)
        // This implements: ∆;Γ ⊢ [p₁, p₂, ..., pₙ] ⇒ List τ ⊣ Γₙ
        let expected_list_type = MonoType::list(element_type.clone());
        self.cons
            .constrain_equal(self.source.clone(), expected_list_type, self.span);

        // Process each element in the list pattern
        for &element_id in elements {
            match *element_id.get(tree) {
                node::ListElPat::Pat(pat_id) => {
                    // Regular pattern element - type it against the element type
                    // TODO create better functionality for this
                    let element_typer =
                        PatternTyper::new(self.env, self.cons, element_type.clone(), self.span);
                    element_typer.run::<T, node::Pat>(pat_id, tree);
                }
                node::ListElPat::Spread(maybe_name) => {
                    // Spread pattern - if there's a name, bind it to List(element_type)
                    if let Some(name_id) = maybe_name {
                        let var_name = name_id.get(tree).0.clone();
                        let rest_type = MonoType::list(element_type.clone());
                        self.env.enter(var_name, rest_type);
                    }
                    // If no name (anonymous spread), no binding needed
                }
            }
        }

        ControlFlow::Continue(())
    }

    /// **Record Patterns:**
    /// - Exact record: `{ l₁ : p₁, ..., lₙ : pₙ } ⇒ { l₁ : τ₁, ..., lₙ : τₙ }`
    /// - Polymorphic record: `{ l₁ : p₁, ..., lₙ : pₙ, ... } ⇒ { l₁ : τ₁, ..., lₙ : τₙ | ρ }`
    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordPat { fields, polymorph } = id.get(tree);

        // Build the expected record type by processing each field pattern
        let tail_type = if *polymorph {
            // For polymorphic records: { ... } creates a row variable
            MonoType::variable(Kind::Record)
        } else {
            // For exact records: no additional fields allowed
            MonoType::empty_row()
        };

        // Build the record type from right to left (inside-out)
        // This creates: { field₁ : τ₁, field₂ : τ₂, ... | tail }
        let mut expected_record_type = tail_type;

        // Process fields in reverse to build the row type correctly // TODO probably not necessary ?
        for &field_id in fields.iter().rev() {
            let node::RecordFieldPat { field, pat } = *field_id.get(tree);
            let field_name = field.get(tree).0;

            // Create a fresh type variable for this field
            let field_type = MonoType::variable(Kind::Type);

            // Create the labeled type for this field
            let labeled_field = LabeledType::new(Label(field_name), field_type.clone());

            // Extend the record type with this field
            expected_record_type = MonoType::row(labeled_field, expected_record_type);

            // If there's a pattern for this field, type it against the field type
            if let Some(pat_id) = pat {
                let field_typer = PatternTyper::new(self.env, self.cons, field_type, self.span);
                field_typer.run::<T, node::Pat>(pat_id, tree);
            } else {
                // Field shorthand: { field } means { field: field }
                // Bind the field name to the field type
                self.env.enter(field_name, field_type);
            }
        }

        // Constrain the source type to match our expected record type
        // This implements both rules:
        // - Exact: { l₁ : p₁, ..., lₙ : pₙ } ⇒ { l₁ : τ₁, ..., lₙ : τₙ }
        // - Polymorphic: { l₁ : p₁, ..., lₙ : pₙ, ... } ⇒ { l₁ : τ₁, ..., lₙ : τₙ | ρ }
        self.cons
            .constrain_equal(self.source.clone(), expected_record_type, self.span);

        ControlFlow::Continue(())
    }

    /// **Variant Patterns:**
    /// ```ignore
    /// ∆;Γ ⊢ p₁ ⇒ τ₁ ⊣ Γ₁    ∆;Γ₁ ⊢ p₂ ⇒ τ₂ ⊣ Γ₂    ...    ρ fresh
    /// -----------------------
    /// ∆;Γ ⊢ < l₁ : p₁, l₂ : p₂, ... > ⇒ < l₁ : τ₁, l₂ : τ₂, ... | ρ > ⊣ Γₙ
    /// ```
    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let cases = &id.get(tree).0;

        // Variant patterns are always polymorphic - they create an open variant
        // with a row variable to allow additional cases
        let tail_type = MonoType::variable(Kind::Tag);

        // Build the expected variant type by processing each case pattern
        // This creates: < case₁ : τ₁, case₂ : τ₂, ... | ρ >
        let mut expected_variant_type = tail_type;

        // Process cases in reverse to build the row type correctly (inside-out)
        for &case_id in cases.iter().rev() {
            let node::VariantTagPat { tag, pat } = *case_id.get(tree);
            let tag_name = tag.get(tree).0;

            // Create a fresh type variable for this case
            let tag_type = MonoType::variable(Kind::Tag);

            // Create the labeled type for this case
            let labeled_case = LabeledType::new(Label(tag_name), tag_type.clone());

            // Extend the variant type with this case
            expected_variant_type = MonoType::row(labeled_case, expected_variant_type);

            // If there's a pattern for this case, type it against the case type
            if let Some(pat_id) = pat {
                let case_typer = PatternTyper::new(self.env, self.cons, tag_type, self.span);
                case_typer.run::<T, node::Pat>(pat_id, tree);
            }
            // If no pattern, the case has Unit type (no additional constraint needed)
            // TODO but I then must restrict the case type to be Unit ?
        }

        // Constrain the source type to match our expected variant type
        // This implements: ∆;Γ ⊢ < l₁ : p₁, l₂ : p₂, ... > ⇒ < l₁ : τ₁, l₂ : τ₂, ... | ρ > ⊣ Γₙ
        self.cons
            .constrain_equal(self.source.clone(), expected_variant_type, self.span);

        ControlFlow::Continue(())
    }

    fn visit_pat(&mut self, id: Id<node::Pat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        match *id.get(tree) {
            node::Pat::Error(error_id) => self.visit_pat_error(error_id, tree),
            node::Pat::Any(any_id) => self.visit_any_pat(any_id, tree),
            node::Pat::Literal(literal_id) => self.visit_literal_pat(literal_id, tree),
            node::Pat::Bind(bind_id) => self.visit_bind_pat(bind_id, tree),
            node::Pat::List(list_id) => self.visit_list_pat(list_id, tree),
            node::Pat::Record(record_id) => self.visit_record_pat(record_id, tree),
            node::Pat::Variant(variant_id) => self.visit_variant_pat(variant_id, tree),
        }
    }
}

#[cfg(test)]
mod tests {

    use kola_tree::prelude::*;
    use kola_utils::interner::StrInterner;

    use crate::{error::TypeError, test::run_typer, types::*};

    #[test]
    fn case_literal_pattern() {
        let mut builder = TreeBuilder::new();

        // case 42 of 42 => true, _ => false
        let source = builder.insert(node::LiteralExpr::Num(42.0));

        let pat1 = builder.insert(node::LiteralPat::Num(42.0));
        let expr1 = builder.insert(node::LiteralExpr::Bool(true));
        let branch1 = node::CaseBranch::new_in(pat1, expr1, &mut builder);

        let pat2 = builder.insert(node::AnyPat);
        let expr2 = builder.insert(node::LiteralExpr::Bool(false));
        let branch2 = node::CaseBranch::new_in(pat2, expr2, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch1, branch2], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::BOOL);
    }

    #[test]
    fn case_literal_pattern_mismatch() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case "hello" of 42 => true, _ => false (should fail: string vs number)
        let source = builder.insert(node::LiteralExpr::Str(interner.intern("hello")));

        let pat1 = builder.insert(node::LiteralPat::Num(42.0));
        let expr1 = builder.insert(node::LiteralExpr::Bool(true));
        let branch1 = node::CaseBranch::new_in(pat1, expr1, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch1], &mut builder);

        let (errors, _) = run_typer(builder, case_expr).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::STR,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn case_wildcard_pattern() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case 42 of _ => "anything"
        let source = builder.insert(node::LiteralExpr::Num(42.0));
        let pat = builder.insert(node::AnyPat);
        let expr = builder.insert(node::LiteralExpr::Str(interner.intern("anything")));
        let branch = node::CaseBranch::new_in(pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::STR);
    }

    #[test]
    fn case_bind_pattern() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case 42 of x => x
        let source = builder.insert(node::LiteralExpr::Num(42.0));

        let x = interner.intern("x");
        let pat = node::BindPat::new_in(x, &mut builder);
        let expr = node::QualifiedExpr::new_in(None, x, None, &mut builder);
        let branch = node::CaseBranch::new_in(pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::NUM);
    }

    #[test]
    fn case_list_pattern_empty() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case [] of [] => "empty", _ => "not empty"
        let source = node::ListExpr::empty_in(&mut builder);

        let pat1 = builder.insert(node::ListPat(vec![]));
        let expr1 = builder.insert(node::LiteralExpr::Str(interner.intern("empty")));
        let branch1 = node::CaseBranch::new_in(pat1, expr1, &mut builder);

        let pat2 = builder.insert(node::AnyPat);
        let expr2 = builder.insert(node::LiteralExpr::Str(interner.intern("not empty")));
        let branch2 = node::CaseBranch::new_in(pat2, expr2, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch1, branch2], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::STR);
    }

    #[test]
    fn case_list_pattern_with_elements() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case [1, 2, 3] of [x, y, z] => x
        let elem1 = builder.insert(node::LiteralExpr::Num(1.0));
        let elem2 = builder.insert(node::LiteralExpr::Num(2.0));
        let elem3 = builder.insert(node::LiteralExpr::Num(3.0));
        let source = node::ListExpr::new_in(vec![elem1, elem2, elem3], &mut builder);

        let x = interner.intern("x");
        let y = interner.intern("y");
        let z = interner.intern("z");

        let pat_x = node::BindPat::new_in(x, &mut builder);
        let pat_y = node::BindPat::new_in(y, &mut builder);
        let pat_z = node::BindPat::new_in(z, &mut builder);

        let el_x = node::ListElPat::pat(pat_x, &mut builder);
        let el_y = node::ListElPat::pat(pat_y, &mut builder);
        let el_z = node::ListElPat::pat(pat_z, &mut builder);

        let list_pat = builder.insert(node::ListPat(vec![el_x, el_y, el_z]));
        let expr = node::QualifiedExpr::new_in(None, x, None, &mut builder);
        let branch = node::CaseBranch::new_in(list_pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::NUM);
    }

    #[test]
    fn case_list_pattern_with_spread() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case [1, 2, 3] of [head, ...tail] => head
        let elem1 = builder.insert(node::LiteralExpr::Num(1.0));
        let elem2 = builder.insert(node::LiteralExpr::Num(2.0));
        let elem3 = builder.insert(node::LiteralExpr::Num(3.0));
        let source = node::ListExpr::new_in(vec![elem1, elem2, elem3], &mut builder);

        let head = interner.intern("head");
        let tail = interner.intern("tail");

        let pat_head = node::BindPat::new_in(head, &mut builder);
        let el_head = node::ListElPat::pat(pat_head, &mut builder);

        let el_spread = node::ListElPat::spread(Some(tail.into()), &mut builder);

        let list_pat = builder.insert(node::ListPat(vec![el_head, el_spread]));
        let expr = node::QualifiedExpr::new_in(None, head, None, &mut builder);
        let branch = node::CaseBranch::new_in(list_pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::NUM);
    }

    #[test]
    fn case_record_pattern_exact() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case {x = 10, y = 20} of {x, y} => x
        let x_field = interner.intern("x");
        let y_field = interner.intern("y");

        let val_x = builder.insert(node::LiteralExpr::Num(10.0));
        let val_y = builder.insert(node::LiteralExpr::Num(20.0));

        let field_x = node::RecordField::new_in(x_field, None, val_x, &mut builder);
        let field_y = node::RecordField::new_in(y_field, None, val_y, &mut builder);
        let source = node::RecordExpr::new_in(vec![field_x, field_y], &mut builder);

        // Pattern: {x, y} (shorthand for {x: x, y: y})
        let pat_field_x = node::RecordFieldPat {
            field: builder.insert(x_field.into()),
            pat: None,
        };
        let pat_field_y = node::RecordFieldPat {
            field: builder.insert(y_field.into()),
            pat: None,
        };

        let record_pat = node::RecordPat::new_in([pat_field_x, pat_field_y], false, &mut builder);

        let expr = node::QualifiedExpr::new_in(None, x_field, None, &mut builder);
        let branch = node::CaseBranch::new_in(record_pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::NUM);
    }

    #[test]
    fn case_record_pattern_polymorphic() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case {x = 10, y = 20, z = "hello"} of {x, y, ...} => x
        let x_field = interner.intern("x");
        let y_field = interner.intern("y");
        let z_field = interner.intern("z");

        let val_x = builder.insert(node::LiteralExpr::Num(10.0));
        let val_y = builder.insert(node::LiteralExpr::Num(20.0));
        let val_z = builder.insert(node::LiteralExpr::Str(interner.intern("hello")));

        let field_x = node::RecordField::new_in(x_field, None, val_x, &mut builder);
        let field_y = node::RecordField::new_in(y_field, None, val_y, &mut builder);
        let field_z = node::RecordField::new_in(z_field, None, val_z, &mut builder);
        let source = node::RecordExpr::new_in(vec![field_x, field_y, field_z], &mut builder);

        // Pattern: {x, y, ...} (polymorphic - allows extra fields)
        let pat_field_x = node::RecordFieldPat {
            field: builder.insert(x_field.into()),
            pat: None,
        };
        let pat_field_y = node::RecordFieldPat {
            field: builder.insert(y_field.into()),
            pat: None,
        };

        let record_pat = node::RecordPat::new_in(
            [pat_field_x, pat_field_y],
            true, // polymorphic
            &mut builder,
        );

        let expr = node::QualifiedExpr::new_in(None, x_field, None, &mut builder);
        let branch = node::CaseBranch::new_in(record_pat, expr, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch], &mut builder);

        let types = run_typer(builder, case_expr).unwrap();

        assert_eq!(types.meta(case_expr), &MonoType::NUM);
    }

    #[test]
    fn case_branch_type_mismatch() {
        let mut interner = StrInterner::new();
        let mut builder = TreeBuilder::new();

        // case 42 of
        //   x => 10,     // returns Num
        //   _ => "hello" // returns Str - should fail!
        let source = builder.insert(node::LiteralExpr::Num(42.0));

        let x = interner.intern("x");
        let pat1 = node::BindPat::new_in(x, &mut builder);
        let expr1 = builder.insert(node::LiteralExpr::Num(10.0));
        let branch1 = node::CaseBranch::new_in(pat1, expr1, &mut builder);

        let pat2 = builder.insert(node::AnyPat);
        let expr2 = builder.insert(node::LiteralExpr::Str(interner.intern("hello")));
        let branch2 = node::CaseBranch::new_in(pat2, expr2, &mut builder);

        let case_expr = node::CaseExpr::new_in(source, vec![branch1, branch2], &mut builder);

        let (errors, _) = run_typer(builder, case_expr).unwrap_err();

        assert_eq!(
            errors[0],
            TypeError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::STR
            }
        );
    }
}
