use std::path::PathBuf;

use bumpalo::collections::CollectIn;
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;

use kola_print::prelude::*;
use kola_semantic::{Inferer, error::SemanticReport};
use kola_syntax::prelude::*;
use kola_tree::{
    Tree,
    print::{MetaDecorator, TreePrinter},
};

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(clap::Subcommand, Debug)]
pub enum Cmd {
    Parse { path: PathBuf },
    Analyze { path: PathBuf },
}

fn main() -> miette::Result<()> {
    let cli: Cli = clap::Parser::parse();

    let arena = Bump::new();
    let options = PrintOptions::default().with_width(80);

    match cli.command {
        Cmd::Parse { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            println!("{}", "Source".bold().bright_white());
            println!("{source}\n");

            let (tree, spans) = try_parse(source, &arena)?;

            // println!("{}", "Abstract Syntax Tree".bold().bright_white());
            // println!("{}", syntax_tree.render(&(), options));
        }
        Cmd::Analyze { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            println!("{}", "Source".bold().bright_white());
            println!("{source}\n");

            let (tree, spans) = try_parse(source.clone(), &arena)?;

            // println!("{}", "Untyped Abstract Syntax Tree".bold().bright_white());
            // println!("{}\n", syntax_tree.render(&(), options));

            let spanned = MetaDecorator::new(spans.clone(), |notation, meta, arena| {
                let span = meta.inner_ref();

                let head = span.display_in(arena);

                let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
                let multi = arena.newline().then(notation, arena);

                head.then(single.or(multi, arena), arena)
            });

            let printer = TreePrinter::new(&tree).add_decorator(spanned);
            println!("{}", printer.render(&(), &arena, options));

            let inferer = Inferer::new(&tree, spans);
            let types = inferer
                .solve(&tree)
                .map_err(|(errors, span)| SemanticReport::new(source, span, errors))?;

            // let mut s = Substitution::empty();
            // syntax_tree
            //     .solve(&mut s)
            //     .map_err(|(errors, span)| SemanticReport::new(source, span, errors))?;
            // s.apply(&mut syntax_tree);

            // println!("{}", "Substitution Table".bold().bright_white());
            // println!("{s}");

            // println!("{}", "Typed Abstract Syntax Tree".bold().bright_white());
            // println!("{}\n", semantic_tree.render(&(), options));
        }
    }

    Ok(())
}

struct TokenPrinter<'a>(&'a Vec<Spanned<Token<'a>>>);

impl<'t> Printable<()> for TokenPrinter<'t> {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let tokens = self
            .0
            .iter()
            .flat_map(|(token, span)| {
                let kind = token.kind();

                [
                    format_args!("\"{token}\"\t\t({kind}, {span})").display_in(arena),
                    arena.newline(),
                ]
            })
            .collect_in::<bumpalo::collections::Vec<_>>(arena);

        arena.concat(tokens.into_bump_slice())
    }
}

fn try_parse(source: Source, arena: &Bump) -> Result<(Tree, SpanMetadata), SyntaxReport> {
    let options = PrintOptions::default();

    let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());

    let result = tokens.and_then(|tokens| {
        println!("{}", "Tokens".bold().bright_white());
        let printer = TokenPrinter(&tokens);
        println!("{}", printer.render(&(), arena, options));

        let ParseResult {
            tree,
            spans: meta,
            errors: mut parse_errors,
        } = parse(tokens, source.end_of_input());

        errors.append(&mut parse_errors);
        tree.map(|tree| (tree, meta))
    });

    if errors.has_errors() {
        if let Some((tree, meta)) = result {
            println!("{}", "Erroneous Abstract Syntax Tree".bold().bright_white());
            let spanned = MetaDecorator::new(meta.clone(), |notation, meta, arena| {
                let span = meta.inner_ref();

                let head = span.display_in(arena);

                let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
                let multi = arena.newline().then(notation, arena);

                head.then(single.or(multi, arena), arena)
            });

            let printer = TreePrinter::new(&tree).add_decorator(spanned);
            println!("{}", printer.render(&(), arena, options));
        }
        Err(SyntaxReport::new(source, errors))
    } else {
        Ok(result.unwrap())
    }
}

// let span = self.meta(with).display_in(arena);

// let node = self.get(with).notate(with, arena);
// let ty = meta
//     .ty()
//     .map(|ty| arena.notate(": ").then(ty.display_in(arena), arena));

// let single = [
//     arena.just(' '),
//     node.clone().flatten(arena),
//     ty.clone()
//         .map(|ty| arena.just(' ').then(ty, arena))
//         .or_not(arena)
//         .flatten(arena),
// ]
// .concat_in(arena);

// let multi = [
//     arena.newline(),
//     node,
//     ty.map(|ty| arena.newline().then(ty, arena)).or_not(arena),
// ]
// .concat_in(arena)
// .indent(arena);

// span.then(single.or(multi, arena), arena)
