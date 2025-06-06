#![feature(never_type)]

use std::io;

use camino::Utf8Path;
use context::ResolveContext;
use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_utils::io::FileSystem;
use log::debug;
use owo_colors::OwoColorize;

use crate::symbol::ModuleSymbol;

pub mod context;
pub mod declare;
pub mod define;
pub mod error;
pub mod forest;
pub mod info;
pub mod scope;
pub mod symbol;
pub mod topography;

pub mod prelude {
    pub use crate::context::ResolveContext;
    pub use crate::forest::Forest;
    pub use crate::resolve;
    pub use crate::topography::Topography;
}

pub fn resolve(
    path: impl AsRef<Utf8Path>,
    io: impl FileSystem + 'static,
) -> io::Result<ResolveContext> {
    let mut ctx = ResolveContext::new(io);

    let path = ctx.io.canonicalize(path.as_ref())?;

    let (path_key, source) = ctx.source_manager.fetch(path.as_path(), &ctx.io)?;

    debug!(
        "{} {}\n{}",
        "Source".bold().bright_white(),
        &path,
        source.text()
    );

    let input = LexInput::new(path_key, source.text());

    let Some(tokens) = tokenize(input, &mut ctx.report) else {
        return Ok(ctx);
    };

    debug!(
        "{} {:?}\n{}",
        "Tokens".bold().bright_white(),
        &path,
        TokenPrinter(&tokens).render(PrintOptions::default())
    );

    let input = ParseInput::new(path_key, tokens);

    let ParseOutput { tree, spans, .. } = parse(input, &mut ctx.interner, &mut ctx.report);

    let Some(tree) = tree else {
        return Ok(ctx);
    };

    ctx.forest.insert(path_key, tree);
    ctx.topography.insert(path_key, spans);

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

    let module_sym = ModuleSymbol::new();

    declare::declare(path_key, module_sym, &mut ctx);

    if !ctx.report.is_empty() {
        return Ok(ctx);
    }

    let scope = ctx.module_scopes.get(&module_sym).cloned().unwrap();
    define::define(scope, &mut ctx);

    Ok(ctx)
}
