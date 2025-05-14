use std::{fmt::Debug, io, rc::Rc};

use camino::Utf8PathBuf;
use kola_print::prelude::*;
use kola_resolve::prelude::*;
use kola_syntax::prelude::*;
use kola_utils::io::{FileSystem, RealFileSystem};
use log::debug;
use owo_colors::OwoColorize;

pub enum DriverOptions {}

pub struct Driver<Io = RealFileSystem> {
    forest: Forest<Io>,
}

impl Driver {
    pub fn new() -> Self {
        Self {
            forest: Forest::new(RealFileSystem),
        }
    }
}

impl<Io> Driver<Io>
where
    Io: FileSystem + Debug,
{
    pub fn with_io(io: Io) -> Self {
        Self {
            forest: Forest::new(io),
        }
    }

    pub fn compile<'a>(mut self, path: Utf8PathBuf) -> io::Result<()> {
        // TODO this should be done by the IO stuff
        let path = path.canonicalize_utf8()?;

        let (path_key, source) = self.forest.sources.fetch(path.as_path())?;

        debug!(
            "{} {}\n{}",
            "Source".bold().bright_white(),
            &path,
            source.text()
        );

        let Some(tokens) = tokenize(path_key, source.text(), &mut self.forest.report) else {
            return self.forest.eprint_report();
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            &path,
            TokenPrinter(&tokens).render(PrintOptions::default())
        );

        let input = ParseInput::new(path_key, tokens);

        let ParseOutput { tree, spans, .. } = parse(input, &mut self.forest.report);

        let Some(tree) = tree else {
            return self.forest.eprint_report();
        };

        self.forest.trees.insert(path_key, Rc::new(tree));
        self.forest.topography.insert(path_key, Rc::new(spans));

        // let interner = STR_INTERNER.read().unwrap();

        // debug!(
        //     "{} {:?}\n{}",
        //     "Untyped Abstract Syntax Tree".bold().bright_white(),
        //     &path,
        //     TreePrinter::new(tree.clone(), &interner)
        //         .with(LocDecorator(spans.clone()))
        //         .render(PrintOptions::default())
        // );

        // drop(interner);

        let declared = Resolver::declare(path_key, &mut self.forest);

        // TODO fail after declared stage if there are errors
        // dbg!(&self.forest);
        //
        dbg!(&self.forest.report);

        if !self.forest.report.is_empty() {
            return self.forest.eprint_report();
        }

        declared
            .to_define(&mut self.forest)
            .define(&mut self.forest); // TODO multiple modules must be defined and probably best to start with inner most ones

        Ok(())
    }
}
