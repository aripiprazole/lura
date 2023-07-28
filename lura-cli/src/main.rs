use clap::*;

use lura_driver::RootDb;

use crate::build::Manifest;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    Run,
    TypeCheck,
}

pub mod build;

fn main() -> eyre::Result<()> {
    let cli = Cli::parse();
    let (tx, _) = crossbeam_channel::unbounded();
    let db = RootDb::new(tx);

    match cli.command {
        Command::Run => {
            let mut manifest = Manifest::load_in_folder(&db, std::env::current_dir()?)?;
            manifest.register_packages()?;
            manifest.resolve_all_files()?;

            println!("Diagnosticated {} errors", manifest.diagnostics.len());

            for diagnostic in manifest.diagnostics {
                println!(" - {:#?}", diagnostic);
            }
        }
        Command::TypeCheck => todo!(),
    }
    Ok(())
}
