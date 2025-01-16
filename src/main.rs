use kola::{
    semantic::{error::SemanticReport, Inferable, Substitution},
    source::Source,
    syntax::{ast, error::SyntaxReport, parse, tokenize, ParseResult, TokenizeResult},
};
use miette::IntoDiagnostic;
use std::{fs, path::PathBuf};

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

    match cli.command {
        Cmd::Parse { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            let ast = try_parse(source)?;

            println!("{ast:?}")
        }
        Cmd::Analyze { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            let mut ast = try_parse(source.clone())?;

            println!("{ast:?}");

            let mut s = Substitution::empty();
            ast.infer(&mut s)
                .map_err(|(errors, span)| SemanticReport::new(source, span, errors))?;

            println!("{ast:?}");
        }
    }

    Ok(())
}

fn try_parse(source: Source) -> Result<ast::Expr, SyntaxReport> {
    let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());

    let ast = tokens.and_then(|tokens| {
        println!("{tokens:?}");
        let ParseResult {
            ast,
            errors: mut parse_errors,
        } = parse(tokens, source.end_of_input());

        errors.append(&mut parse_errors);
        ast
    });

    if errors.has_errors() {
        if let Some(ast) = ast {
            println!("{ast:?}")
        }
        Err(SyntaxReport::new(source, errors))
    } else {
        Ok(ast.unwrap())
    }
}
