use std::io;

use camino::Utf8Path;
use kola_print::{PrintOptions, prelude::Bump};
use kola_resolver::prelude::*;
use kola_span::Report;
use kola_utils::{interner::StrInterner, io::FileSystem};

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
            lookup_table,
            module_graph,
            module_scopes,
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

        Ok(())
    }
}
