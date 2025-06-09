use std::io;

use camino::Utf8Path;
use kola_print::PrintOptions;
use kola_resolve::prelude::*;
use kola_utils::io::FileSystem;

pub enum DriverOptions {}

pub struct Driver;

impl Driver {
    pub fn compile<'a>(
        path: impl AsRef<Utf8Path>,
        io: impl FileSystem + 'static,
    ) -> io::Result<()> {
        let ctx = resolve(path, io, PrintOptions::default())?;

        if !ctx.report.is_empty() {
            return ctx.report.eprint(&ctx.source_manager);
        }

        Ok(())
    }
}
