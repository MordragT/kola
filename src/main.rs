use camino::Utf8PathBuf;
use kola_utils::io::RealFileSystem;
use std::io;

use kola_driver::Driver;

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
    Compile { path: Utf8PathBuf },
    Run { path: Utf8PathBuf },
    Test { path: Utf8PathBuf },
    Doc { path: Utf8PathBuf },
}

fn main() -> io::Result<()> {
    env_logger::init();
    // env_logger::builder()
    //     .filter_level(log::LevelFilter::Debug)
    //     .init();

    let cli: Cli = clap::Parser::parse();

    let mut driver = Driver::new(RealFileSystem);

    match cli.command {
        Cmd::Parse { path } => {
            driver.parse(path)?;
        }
        Cmd::Analyze { path } => {
            driver.analyze(path)?;
        }
        Cmd::Compile { path } => {
            driver.compile(path)?;
        }
        Cmd::Run { path } => {
            driver.run(path)?;
        }
        Cmd::Test { path } => {
            todo!()
        }
        Cmd::Doc { path } => {
            todo!()
        }
    }

    Ok(())
}
