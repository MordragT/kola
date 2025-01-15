use kola::{
    semantic::{error::SemanticReport, Inferable, Substitution},
    source::Source,
    syntax::{try_parse, try_tokenize},
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
            let path = path.canonicalize().into_diagnostic()?;
            let name = path.file_name().unwrap().to_string_lossy();

            let source = fs::read_to_string(&path).into_diagnostic()?;
            let source = Source::new(name, source);

            let tokens = try_tokenize(&source)?;
            let ast = try_parse(&source, tokens)?;

            println!("{ast:?}")
        }
        Cmd::Analyze { path } => {
            let path = path.canonicalize().into_diagnostic()?;
            let name = path.file_name().unwrap().to_string_lossy();

            let source = fs::read_to_string(&path).into_diagnostic()?;
            let source = Source::new(name, source);

            let tokens = try_tokenize(&source)?;
            let mut ast = try_parse(&source, tokens)?;

            println!("{ast:?}");

            let mut s = Substitution::empty();
            ast.infer(&mut s).map_err(|(errors, span)| {
                SemanticReport::new(errors.into_vec(), span, source.named_source())
            })?;

            println!("{ast:?}");
        }
    }

    Ok(())
}
