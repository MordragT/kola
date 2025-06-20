use kola_span::Loc;
use kola_tree::prelude::*;
use std::ops::ControlFlow;

use crate::{
    constraints::Constraints,
    env::LocalTypeEnv,
    types::{LabeledType, MonoType},
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
        match id.visit_by(&mut self, tree) {
            ControlFlow::Break(_) => (),
            ControlFlow::Continue(_) => (),
        }
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
            .constrain(self.source.clone(), literal_type, self.span);

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
        let var_name = id.get(tree).0.clone();

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
        let element_type = MonoType::variable();

        // Constrain the source type to be List(element_type)
        // This implements: ∆;Γ ⊢ [p₁, p₂, ..., pₙ] ⇒ List τ ⊣ Γₙ
        let expected_list_type = MonoType::list(element_type.clone());
        self.cons
            .constrain(self.source.clone(), expected_list_type, self.span);

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
            MonoType::variable()
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
            let field_type = MonoType::variable();

            // Create the labeled type for this field
            let labeled_field = LabeledType::new(field_name, field_type.clone());

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
            .constrain(self.source.clone(), expected_record_type, self.span);

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
        let tail_type = MonoType::variable();

        // Build the expected variant type by processing each case pattern
        // This creates: < case₁ : τ₁, case₂ : τ₂, ... | ρ >
        let mut expected_variant_type = tail_type;

        // Process cases in reverse to build the row type correctly (inside-out)
        for &case_id in cases.iter().rev() {
            let node::VariantTagPat { case, pat } = *case_id.get(tree);
            let case_name = case.get(tree).0;

            // Create a fresh type variable for this case
            let case_type = MonoType::variable();

            // Create the labeled type for this case
            let labeled_case = LabeledType::new(case_name, case_type.clone());

            // Extend the variant type with this case
            expected_variant_type = MonoType::row(labeled_case, expected_variant_type);

            // If there's a pattern for this case, type it against the case type
            if let Some(pat_id) = pat {
                let case_typer = PatternTyper::new(self.env, self.cons, case_type, self.span);
                case_typer.run::<T, node::Pat>(pat_id, tree);
            }
            // If no pattern, the case has Unit type (no additional constraint needed)
        }

        // Constrain the source type to match our expected variant type
        // This implements: ∆;Γ ⊢ < l₁ : p₁, l₂ : p₂, ... > ⇒ < l₁ : τ₁, l₂ : τ₂, ... | ρ > ⊣ Γₙ
        self.cons
            .constrain(self.source.clone(), expected_variant_type, self.span);

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
