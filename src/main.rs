use std::path::PathBuf;

use miette::IntoDiagnostic;
use owo_colors::OwoColorize;

use kola_semantic::prelude::*;
// use kola_compiler::prelude::*;
use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;

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

    let options = PrintOptions::default().with_width(120);

    match cli.command {
        Cmd::Parse { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            println!("{}", "Source".bold().bright_white());
            println!("{source}\n");

            let (tree, spans) = try_parse(source)?;

            println!("{}", "Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans.clone()))
                .print(options);
        }
        Cmd::Analyze { path } => {
            let options = ExploreOptions {
                verbosity: ExploreVerbosity {
                    debug: true,
                    source: false,
                    tokens: false,
                    tree: true,
                },
                ..Default::default()
            };

            let mut world = World::new();
            world.explore(path, options)?;

            println!("\n{}", "Module Dependency Order".bold().bright_white());
            for file in &world.order {
                println!("{file:?}");
            }

            world.infer(true, Default::default()).unwrap();
        }
    }

    Ok(())
}

fn try_parse(source: Source) -> Result<(Tree, SpanInfo), SourceReport> {
    let options = PrintOptions::default();

    let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());

    let result = tokens.and_then(|tokens| {
        println!("\n{}", "Tokens".bold().bright_white());
        TokenPrinter(&tokens).print(options);

        let ParseResult {
            tree,
            spans: meta,
            errors: mut parse_errors,
        } = parse(tokens, source.end_of_input());

        errors.append(&mut parse_errors);
        tree.map(|tree| (tree, meta))
    });

    if !errors.is_empty() {
        if let Some((tree, spans)) = result {
            println!("{}", "Erroneous Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans))
                .print(options);
        }
        Err(SourceReport::new(source, errors))
    } else {
        Ok(result.unwrap())
    }
}
