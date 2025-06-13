use std::io;

use camino::Utf8Path;
use kola_ir::print::IrPrinter;
use kola_lowerer::module::lower;
use kola_print::{PrintOptions, prelude::*};
use kola_resolver::prelude::*;
use kola_span::Report;
use kola_typer::check::{TypeCheckOutput, type_check};
use kola_utils::{interner::StrInterner, io::FileSystem};
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
            value_orders,
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
            &self.arena,
            &mut self.interner,
            &mut self.report,
            self.print_options,
        );

        if !self.report.is_empty() {
            return self.report.eprint(&source_manager);
        }

        let mut output = lower(&module_scopes, &value_orders, &forest);

        // Must be replaced by proper Ir linking
        let ir = output.remove(0).ir;

        // Ir Printint should be in another function
        {
            let root_id = ir.root();
            let ir_printer = IrPrinter::new(&ir, root_id);
            ir_printer.print(self.print_options, &self.arena);
        }

        let mut machine = CekMachine::new(ir);

        match machine.run() {
            Ok(value) => println!("Execution result: {:?}", value),
            Err(e) => eprintln!("Runtime error: {}", e),
        }

        Ok(())
    }
}
