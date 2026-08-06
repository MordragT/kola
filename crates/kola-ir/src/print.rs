use kola_interner::StrInterner;
use kola_print::prelude::*;

use crate::{
    attrs::IrAttrs,
    id::Id,
    instr::{self, Instr},
    ir::Ir,
    visit::IrVisitor,
};

pub fn render_ir(ir: &Ir, arena: &Bump, interner: &StrInterner, options: PrintOptions) -> String {
    let root = ir.root();

    let mut attrs = IrAttrs::new(ir.count());
    let Ok(()) = attrs.visit_expr(root, ir);

    let printer = IrPrinter::new(ir, &attrs, interner);
    let mut result = printer.render(&root, options, arena);

    result.push_str(&format!("\n\n{}\n\n", "With:".bold().bright_white()));

    let use_counts = attrs.take_use_counts();
    let printer = IrPrinter::new(ir, &attrs, interner);

    for (id, _) in use_counts.iter_shared() {
        // Only pattern matcher do actually render differently when shared
        let s = match ir.instructions[id] {
            // Instr::Atom(_) => printer.render(&Id::<instr::Atom>::new(id as u32), options, arena),
            // Instr::Expr(_) => printer.render(&Id::<instr::Expr>::new(id as u32), options, arena),
            // Instr::Field(field) => printer.render(&field, options, arena),
            // Instr::Item(item) => printer.render(&item, options, arena),
            // Instr::Path(path) => printer.render(&path, options, arena),
            Instr::PatternMatcher(_) => {
                printer.render(&Id::<instr::PatternMatcher>::new(id as u32), options, arena)
            }
            // Instr::HandlerClause(clause) => printer.render(&clause, options, arena),
            _ => continue,
        };

        result.push_str(&s);
        result.push('\n');
        result.push('\n');
    }

    for id in &attrs.pattern_successors {
        let s = printer.render(id, options, arena);
        result.push_str(&s);
        result.push('\n');
        result.push('\n');
    }

    result
}

#[derive(Debug, Clone, Copy)]
pub struct IrPrinter<'a> {
    pub ir: &'a Ir,
    pub attrs: &'a IrAttrs,
    pub interner: &'a StrInterner,
}

impl<'a> IrPrinter<'a> {
    pub fn new(ir: &'a Ir, attrs: &'a IrAttrs, interner: &'a StrInterner) -> Self {
        Self {
            ir,
            attrs,
            interner,
        }
    }

    /// Render a pattern test of the form `head <payload> -> <success> | <failure>`.
    ///
    /// The `payload` is optional; pass `None` for tests without a payload (e.g. `IsUnit`).
    pub fn test(
        &self,
        head: &'a str,
        payload: Option<Notation<'a>>,
        on_success: Id<instr::PatternMatcher>,
        on_failure: Id<instr::PatternMatcher>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let mut parts = BumpVec::new_in(arena);
        parts.push(arena.notate(head));

        if let Some(payload) = payload {
            parts.push(arena.just(' '));
            parts.push(payload);
        }

        parts.push(arena.notate(" -> "));
        parts.push(self.notate(&on_success, arena));
        parts.push(arena.newline());
        parts.push(arena.notate("| "));
        parts.push(self.notate(&on_failure, arena));

        arena.concat(parts.into_bump_slice())
    }

    /// Render an extractor of the form `<bind> = <op> <args>... => <next>`.
    ///
    /// Produces the single-line layout `<bind> = <op> <args> => <next>` and the
    /// multi-line layout `<bind>\n  <op> <args>\n  => <next>` as a choice.
    pub fn extractor(
        &self,
        bind: Notation<'a>,
        op: &'a str,
        args: &[Notation<'a>],
        next: Id<instr::PatternMatcher>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let next = self.notate(&next, arena);

        let head = bind.then(arena.notate(" ="), arena);

        // single: ` <op> <args> => <next>`
        let mut single_parts = BumpVec::new_in(arena);
        single_parts.push(arena.just(' '));
        single_parts.push(arena.notate(op));
        for arg in args {
            single_parts.push(arena.just(' '));
            single_parts.push(arg.clone());
        }
        single_parts.push(arena.notate(" => "));
        single_parts.push(next.clone());
        let single = arena.concat(single_parts.into_bump_slice()).flatten(arena);

        // multi: `\n <op> <args>\n => <next>`
        let mut multi_parts = BumpVec::new_in(arena);
        multi_parts.push(arena.newline());
        multi_parts.push(arena.notate(op));
        for arg in args {
            multi_parts.push(arena.just(' '));
            multi_parts.push(arg.clone());
        }
        multi_parts.push(arena.newline());
        multi_parts.push(arena.notate("=> "));
        multi_parts.push(next);
        let multi = arena.concat(multi_parts.into_bump_slice()).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
