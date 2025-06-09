#![feature(never_type)]

use std::io;

use camino::Utf8Path;
use context::ResolveContext;
use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_tree::{
    id::Id,
    print::{Decorators, TreePrinter},
};
use kola_utils::{interner::PathKey, io::FileSystem};
use log::debug;
use owo_colors::OwoColorize;

use crate::symbol::ModuleSym;

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

pub type QualId<T> = (PathKey, Id<T>);

pub fn resolve(
    path: impl AsRef<Utf8Path>,
    io: impl FileSystem + 'static,
    print_options: PrintOptions,
) -> io::Result<ResolveContext> {
    let mut ctx = ResolveContext::new(io, print_options);

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
        TokenPrinter(&tokens, ctx.print_options).render(ctx.print_options, &ctx.arena)
    );

    let input = ParseInput::new(path_key, tokens);

    let ParseOutput { tree, spans, .. } = parse(input, &mut ctx.interner, &mut ctx.report);

    let Some(tree) = tree else {
        return Ok(ctx);
    };

    let decorators = Decorators::new().with(LocDecorator(spans.clone()));
    let tree_printer = TreePrinter::new(&tree, &ctx.interner, &decorators);

    debug!(
        "{} {:?}\n{}",
        "Untyped Abstract Syntax Tree".bold().bright_white(),
        &path,
        tree_printer.render(ctx.print_options, &ctx.arena)
    );

    ctx.forest.insert(path_key, tree);
    ctx.topography.insert(path_key, spans);

    let module_sym = ModuleSym::new();

    declare::declare(path_key, module_sym, &mut ctx);

    if !ctx.report.is_empty() {
        return Ok(ctx);
    }

    define::define_modules(&mut ctx);

    if !ctx.report.is_empty() {
        return Ok(ctx);
    }

    dbg!(&ctx.module_graph);

    // TODO this isn't really beautiful, but it works for now.
    for module_sym in ctx.unresolved_scopes.keys().copied().collect::<Vec<_>>() {
        define::define_value(module_sym, &mut ctx);
    }

    Ok(ctx)
}
