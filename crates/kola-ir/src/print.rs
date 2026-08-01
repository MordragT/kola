use std::{collections::HashSet, fmt::Write};

use kola_print::prelude::*;
use kola_utils::interner::StrInterner;

use crate::{
    id::Id,
    instr,
    ir::{Ir, IrView},
    visit::IrVisitor,
};

pub fn render_ir(ir: &Ir, arena: &Bump, interner: &StrInterner, options: PrintOptions) -> String {
    let root = ir.root();

    let mut labeller = IrLabeller::new(ir.count());
    let Ok(()) = labeller.visit_expr(root, ir);

    let printer = IrPrinter::new(ir, &labeller.labels, &labeller.shared, interner);
    let mut result = printer.render(&root, options, arena);

    result.push_str(&format!("\n\n{}\n\n", "With:".bold().bright_white()));

    let mut shared = vec![false; ir.count()];

    for matcher in labeller.defered_patterns {
        let matcher_printer = IrPrinter::new(ir, &labeller.labels, &shared, interner);
        result
            .write_fmt(format_args!("{}: ", matcher.as_usize().bold()))
            .unwrap();
        result.push_str(&matcher_printer.render(&matcher, options, arena));
        result.push('\n');
        result.push('\n');

        shared[matcher.as_usize()] = true;
    }

    for expr in labeller.defered_expr {
        result.push_str(&printer.render(&expr, options, arena));
        result.push('\n');
        result.push('\n');
    }

    result
}

#[derive(Debug, Clone, Copy)]
pub struct IrPrinter<'a> {
    pub ir: &'a Ir,
    pub labels: &'a [u32],
    pub shared: &'a [bool],
    pub interner: &'a StrInterner,
}

impl<'a> IrPrinter<'a> {
    pub fn new(
        ir: &'a Ir,
        labels: &'a [u32],
        shared: &'a [bool],
        interner: &'a StrInterner,
    ) -> Self {
        Self {
            ir,
            labels,
            shared,
            interner,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrLabeller {
    pub labels: Vec<u32>, // every instruction has a label therefore we can use a Vec<u32> to store them
    pub reference_counts: Vec<u32>, // counts how many times each instruction is referenced
    pub shared: Vec<bool>, // whether the instruction is shared or not
    pub label_counter: u32,
    pub defered_expr: HashSet<Id<instr::Expr>>, // defered expressions to be printed later
    pub defered_patterns: HashSet<Id<instr::PatternMatcher>>, // defered patterns to be printed later
}

impl IrLabeller {
    pub fn new(count: usize) -> Self {
        Self {
            labels: vec![0; count],
            reference_counts: vec![0; count],
            shared: vec![false; count],
            label_counter: 0,
            defered_expr: HashSet::new(),
            defered_patterns: HashSet::new(),
        }
    }
}
/// This visitor is used to label each instruction in the IR with a unique label.
/// The labels are computed by visiting each instruction inside the Instr enum,
/// and assigning a label based on the order of visitation.
/// The labels are stored in a Vec<u32>, where the index of the label corresponds to
/// the Id of the instruction.
impl<Ir> IrVisitor<Ir> for IrLabeller
where
    Ir: IrView,
{
    type Error = !;

    fn visit_atom(&mut self, atom: Id<instr::Atom>, ir: &Ir) -> Result<(), Self::Error> {
        self.reference_counts[atom.as_usize()] += 1;

        if self.reference_counts[atom.as_usize()] > 1 {
            self.shared[atom.as_usize()] = true;
        } else {
            self.labels[atom.as_usize()] = self.label_counter;
            self.label_counter += 1;

            if let instr::Atom::Func(f) = ir.instr(atom) {
                self.visit_expr(f.body, ir)?;
            }
        }

        Ok(())
    }

    fn visit_expr(&mut self, expr: Id<instr::Expr>, ir: &Ir) -> Result<(), Self::Error> {
        self.reference_counts[expr.as_usize()] += 1;

        if self.reference_counts[expr.as_usize()] > 1 {
            self.shared[expr.as_usize()] = true;
        } else {
            self.labels[expr.as_usize()] = self.label_counter;
            self.label_counter += 1;
            self.walk_expr(expr, ir)?;
        }

        Ok(())
    }

    fn visit_record_field(
        &mut self,
        record_field: Id<instr::RecordField>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.reference_counts[record_field.as_usize()] += 1;

        if self.reference_counts[record_field.as_usize()] > 1 {
            self.shared[record_field.as_usize()] = true;
        } else {
            self.labels[record_field.as_usize()] = self.label_counter;
            self.label_counter += 1;
            self.walk_record_field(record_field, ir)?;
        }

        Ok(())
    }

    fn visit_list_item(
        &mut self,
        list_item: Id<instr::ListItem>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.reference_counts[list_item.as_usize()] += 1;

        if self.reference_counts[list_item.as_usize()] > 1 {
            self.shared[list_item.as_usize()] = true;
        } else {
            self.labels[list_item.as_usize()] = self.label_counter;
            self.label_counter += 1;
            self.walk_list_item(list_item, ir)?;
        }

        Ok(())
    }

    fn visit_field_path(
        &mut self,
        field_path: Id<instr::FieldPath>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.reference_counts[field_path.as_usize()] += 1;

        if self.reference_counts[field_path.as_usize()] > 1 {
            self.shared[field_path.as_usize()] = true;
        } else {
            self.labels[field_path.as_usize()] = self.label_counter;
            self.label_counter += 1;
            self.walk_field_path(field_path, ir)?;
        }

        Ok(())
    }

    fn visit_pattern_matcher(
        &mut self,
        matcher: Id<instr::PatternMatcher>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.reference_counts[matcher.as_usize()] += 1;

        if self.reference_counts[matcher.as_usize()] > 1 {
            self.shared[matcher.as_usize()] = true;
            self.defered_patterns.insert(matcher);
        } else {
            self.labels[matcher.as_usize()] = self.label_counter;
            self.label_counter += 1;
            self.walk_pattern_matcher(matcher, ir)?;
        }

        Ok(())
    }

    fn visit_pattern_success(
        &mut self,
        pattern_success: instr::PatternSuccess,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.defered_expr.insert(pattern_success.next);

        self.walk_pattern_success(pattern_success, ir)?;

        Ok(())
    }
}
