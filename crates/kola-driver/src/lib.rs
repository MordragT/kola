use std::{io, rc::Rc};

use camino::Utf8Path;
use kola_ir::print::IrPrinter;
use kola_lowerer::module::{Program, lower};
use kola_print::{PrintOptions, prelude::*};
use kola_resolver::prelude::*;
use kola_span::Report;
use kola_typer::check::{TypeCheckOutput, type_check};
use kola_utils::{fmt::StrInternerExt, interner::StrInterner, io::FileSystem};
use kola_vm::machine::CekMachine;

pub enum DriverOptions {}

pub struct Driver {
    io: Box<dyn FileSystem>,
    arena: Bump,
    interner: StrInterner,
    report: Report,
    print_options: PrintOptions,
}

impl Driver {
    pub fn new(io: impl FileSystem + 'static) -> Self {
        Self {
            io: Box::new(io),
            arena: Bump::new(),
            interner: StrInterner::new(),
            report: Report::new(),
            print_options: PrintOptions::default(),
        }
    }

    pub fn compile<'a>(mut self, path: impl AsRef<Utf8Path>) -> io::Result<()> {
        let ResolveOutput {
            source_manager,
            forest,
            topography,
            module_graph,
            module_scopes,
            entry_points,
            value_orders,
            type_orders,
        } = resolve(
            path,
            &self.io,
            &self.arena,
            &mut self.interner,
            &mut self.report,
            self.print_options,
        )?;

        if !self.report.is_empty() {
            return self.report.eprint(&source_manager);
        }

        let TypeCheckOutput {
            type_env,
            type_annotations,
        } = type_check(
            &forest,
            &topography,
            &module_graph,
            &module_scopes,
            &value_orders,
            &type_orders,
            &self.arena,
            &mut self.interner,
            &mut self.report,
            self.print_options,
        );

        if !self.report.is_empty() {
            return self.report.eprint(&source_manager);
        }

        // TODO entry_points logic
        let Program { ir, modules } = lower(
            entry_points[0],
            &module_scopes,
            &value_orders,
            &forest,
            &self.arena,
            &self.interner,
            self.print_options,
        );

        let interner = Rc::new(self.interner);
        let mut machine = CekMachine::new(ir, interner.clone());

        match machine.run() {
            Ok(value) => {
                println!("\nExecution result: {}", interner.display(&value))
            }
            Err(e) => eprintln!("\nRuntime error: {}", e),
        }

        Ok(())
    }
}
