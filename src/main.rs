use camino::Utf8PathBuf;
use log::info;
use std::path::PathBuf;

// use kola_compiler::prelude::*;
use kola_print::prelude::*;

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(clap::Subcommand, Debug)]
pub enum Cmd {
    Parse { path: Utf8PathBuf },
    Analyze { path: Utf8PathBuf },
}

fn main() -> miette::Result<()> {
    // env_logger::init();
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .init();

    let cli: Cli = clap::Parser::parse();

    let options = PrintOptions::default().with_width(120);

    match cli.command {
        Cmd::Parse { path } => {
            let (file_path, import_path) = FilePath::open(path).into_diagnostic()?;
            let source = Source::from_path(file_path, import_path).into_diagnostic()?;
            let _file = FileParser::new(source, options).try_parse()?;
        }
        Cmd::Analyze { path } => {
            let mut world = World::explore(path, options)?;

            info!(
                "{}\n{}",
                "Module Dependency Order".bold().bright_white(),
                world
                    .order
                    .iter()
                    .map(ToString::to_string)
                    .fold(String::new(), |mut s, m| {
                        s.push_str(&m);
                        s.push('\n');
                        s
                    }),
            );

            world.type_check().unwrap();
        }
    }

    Ok(())
}
