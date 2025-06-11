use std::io;

use camino::Utf8Path;
use kola_print::{PrintOptions, prelude::Bump};
use kola_resolver::prelude::*;
use kola_span::{IntoDiagnostic, Report};
use kola_typer::{env::TypeEnv, typer::Typer};
use kola_utils::{fmt::StrInternerExt, interner::StrInterner, io::FileSystem};

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
            symbol_table,
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

        // TODO actually fill the `TypeEnv` with the necessary information
        let mut env = TypeEnv::new();

        for (module_sym, module_scope) in module_scopes {
            let path_key = module_scope.path_key();

            let spans = topography[path_key].clone();
            let tree = &*forest[path_key];

            match Typer::new(module_scope.id(), spans, &env, &symbol_table)
                .solve(tree, &mut self.report)
            {
                Ok(types) => todo!(),
                Err((errors, span)) => {
                    let diag = self.interner.display(&errors).into_diagnostic(span);
                    self.report.add_diagnostic(diag);
                }
            }
        }

        Ok(())
    }
}
