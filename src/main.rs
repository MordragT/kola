use kola::{
    semantic::{error::SemanticReport, Infer, Substitution},
    source::Source,
    syntax::{
        ast,
        error::SyntaxReport,
        parse,
        print::{PrintOptions, Printable},
        tokenize, ParseResult, TokenizeResult,
    },
};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use std::path::PathBuf;

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

            let ast = try_parse(source)?;

            println!("{}", "Abstract Syntax Tree".bold().bright_white());
            println!("{}", ast.render(options));
        }
        Cmd::Analyze { path } => {
            let source = Source::from_path(path).into_diagnostic()?;

            println!("{}", "Source".bold().bright_white());
            println!("{source}\n");

            let mut ast = try_parse(source.clone())?;

            println!("{}", "Untyped Abstract Syntax Tree".bold().bright_white());
            println!("{}\n", ast.render(options));

            let mut s = Substitution::empty();
            ast.solve(&mut s)
                .map_err(|(errors, span)| SemanticReport::new(source, span, errors))?;
            s.apply(&mut ast);

            println!("{}", "Substitution Table".bold().bright_white());
            println!("{s}");

            println!("{}", "Typed Abstract Syntax Tree".bold().bright_white());
            println!("{}\n", ast.render(options));
        }
    }

    Ok(())
}

fn try_parse(source: Source) -> Result<ast::Expr, SyntaxReport> {
    let options = PrintOptions::default();

    let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());

    let ast = tokens.and_then(|tokens| {
        println!("{}", "Tokens".bold().bright_white());
        println!("{}", tokens.render(options));

        let ParseResult {
            ast,
            errors: mut parse_errors,
        } = parse(tokens, source.end_of_input());

        errors.append(&mut parse_errors);
        ast
    });

    if errors.has_errors() {
        if let Some(ast) = ast {
            println!("{}", "Erroneous Abstract Syntax Tree".bold().bright_white());
            println!("{}", ast.render(options));
        }
        Err(SyntaxReport::new(source, errors))
    } else {
        Ok(ast.unwrap())
    }
}
