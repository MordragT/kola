use std::path::PathBuf;

use miette::IntoDiagnostic;
use owo_colors::OwoColorize;

use kola_compiler::prelude::*;
use kola_print::prelude::*;
use kola_semantic::prelude::*;
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

    let options = PrintOptions::default().with_width(80);

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
            let source = Source::from_path(path).into_diagnostic()?;

            println!("{}", "Source".bold().bright_white());
            println!("{source}");

            let (tree, spans) = try_parse(source.clone())?;

            println!("\n{}", "Untyped Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans.clone()))
                .print(options);

            let inferer = Inferer::new(&tree, spans.clone());
            let types = inferer
                .solve(&tree)
                .map_err(|(errors, span)| SemanticReport::new(source, span, errors))?;

            println!("\n{}", "Typed Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans.clone()))
                .with(TypeDecorator(types.clone()))
                .print(options);

            let ir = Normalizer::normalize(&tree);

            println!("\n{}", "Intermediate Representation".bold().bright_white());
            ir.print(options);
        }
    }

    Ok(())
}

fn try_parse(source: Source) -> Result<(Tree, SpanMetadata), SyntaxReport> {
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

    if errors.has_errors() {
        if let Some((tree, spans)) = result {
            println!("{}", "Erroneous Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans))
                .print(options);
        }
        Err(SyntaxReport::new(source, errors))
    } else {
        Ok(result.unwrap())
    }
}
