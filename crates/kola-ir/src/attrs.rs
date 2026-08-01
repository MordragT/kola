use std::collections::HashSet;

use crate::{id::Id, instr, ir::IrView, visit::IrVisitor};

/// Labels is a struct that holds a vector of u32 labels for each instruction in the IR.
#[derive(Debug, Clone)]
pub struct Labels {
    labels: Vec<u32>,
    counter: u32,
}

impl Labels {
    pub fn new(count: usize) -> Self {
        Self {
            labels: vec![0; count],
            counter: 0,
        }
    }

    pub fn get<T>(&self, id: Id<T>) -> u32 {
        self.labels[id.as_usize()]
    }

    pub fn set<T>(&mut self, id: Id<T>) {
        self.labels[id.as_usize()] = self.counter;
        self.counter += 1;
    }
}

/// UseCounts is a struct that holds a vector of u32 counts for each instruction in the IR.
#[derive(Debug, Clone)]
pub struct UseCounts(Vec<u32>);

impl UseCounts {
    pub fn new(count: usize) -> Self {
        Self(vec![0; count])
    }

    pub fn get<T>(&self, id: Id<T>) -> u32 {
        self.0[id.as_usize()]
    }

    pub fn inc<T>(&mut self, id: Id<T>) {
        self.0[id.as_usize()] += 1;
    }

    pub fn is_shared<T>(&self, id: Id<T>) -> bool {
        self.0[id.as_usize()] > 1
    }

    pub fn iter_shared(&self) -> impl Iterator<Item = (usize, u32)> + '_ {
        self.0
            .iter()
            .enumerate()
            .filter(|(_, count)| **count > 1)
            .map(|(idx, count)| (idx, *count))
    }
}

/// IrAttrs is a struct that holds the labels and use counts for each instruction in the IR.
#[derive(Debug, Clone)]
pub struct IrAttrs {
    pub labels: Labels,
    pub use_counts: UseCounts,
    pub pattern_successors: HashSet<Id<instr::Expr>>,
}

impl IrAttrs {
    pub fn new(count: usize) -> Self {
        Self {
            labels: Labels::new(count),
            use_counts: UseCounts::new(count),
            pattern_successors: HashSet::new(),
        }
    }

    pub fn label_of<T>(&self, id: Id<T>) -> u32 {
        self.labels.get(id)
    }

    pub fn use_count_of<T>(&self, id: Id<T>) -> u32 {
        self.use_counts.get(id)
    }

    pub fn is_shared<T>(&self, id: Id<T>) -> bool {
        self.use_counts.is_shared(id)
    }

    pub fn take_use_counts(&mut self) -> UseCounts {
        let use_counts = UseCounts::new(self.use_counts.0.len());
        std::mem::replace(&mut self.use_counts, use_counts)
    }
}

/// This visitor is used to label each instruction in the IR with a unique label.
/// The labels are computed by visiting each instruction inside the Instr enum,
/// and assigning a label based on the order of visitation.
/// The labels are stored in a Vec<u32>, where the index of the label corresponds to
/// the Id of the instruction.
impl<Ir> IrVisitor<Ir> for IrAttrs
where
    Ir: IrView,
{
    type Error = !;

    fn visit_atom(&mut self, atom: Id<instr::Atom>, ir: &Ir) -> Result<(), Self::Error> {
        self.use_counts.inc(atom);

        if !self.use_counts.is_shared(atom) {
            self.labels.set(atom);

            if let instr::Atom::Func(f) = ir.instr(atom) {
                self.visit_expr(f.body, ir)?;
            }
        }

        Ok(())
    }

    fn visit_expr(&mut self, expr: Id<instr::Expr>, ir: &Ir) -> Result<(), Self::Error> {
        self.use_counts.inc(expr);

        if !self.use_counts.is_shared(expr) {
            self.labels.set(expr);

            self.walk_expr(expr, ir)?;
        }

        Ok(())
    }

    fn visit_record_field(
        &mut self,
        record_field: Id<instr::RecordField>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.use_counts.inc(record_field);

        if !self.use_counts.is_shared(record_field) {
            self.labels.set(record_field);

            self.walk_record_field(record_field, ir)?;
        }

        Ok(())
    }

    fn visit_list_item(
        &mut self,
        list_item: Id<instr::ListItem>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.use_counts.inc(list_item);

        if !self.use_counts.is_shared(list_item) {
            self.labels.set(list_item);

            self.walk_list_item(list_item, ir)?;
        }

        Ok(())
    }

    fn visit_field_path(
        &mut self,
        field_path: Id<instr::FieldPath>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.use_counts.inc(field_path);

        if !self.use_counts.is_shared(field_path) {
            self.labels.set(field_path);

            self.walk_field_path(field_path, ir)?;
        }

        Ok(())
    }

    fn visit_pattern_matcher(
        &mut self,
        matcher: Id<instr::PatternMatcher>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.use_counts.inc(matcher);

        if !self.use_counts.is_shared(matcher) {
            self.labels.set(matcher);

            self.walk_pattern_matcher(matcher, ir)?;
        }

        Ok(())
    }

    fn visit_pattern_success(
        &mut self,
        pattern_success: instr::PatternSuccess,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.pattern_successors.insert(pattern_success.next);

        self.walk_pattern_success(pattern_success, ir)?;

        Ok(())
    }
}
