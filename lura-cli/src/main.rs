use clap::*;
use itertools::Itertools;
use lura_driver::RootDb;
use lura_eyre::eyre;

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
  Js {
    #[clap(short, long)]
    watch: bool,

    #[clap(short, long)]
    package: String,
  },
  TypeCheck,
}

pub mod build;

fn main() -> lura_eyre::Result<()> {
  let cli = Cli::parse();
  let db = RootDb::default();

  match cli.command {
    Command::Js { package, .. } => {
      let mut manifest = Manifest::load_in_folder(&db, std::env::current_dir()?)?;
      manifest.register_packages()?;

      let source_map = manifest.resolve_all_files()?;
      if manifest.diagnostics.is_empty() {
        let current_source = source_map
          .get_in_db(manifest.db, package)
          .ok_or_else(|| eyre!("could not locate the package"))?;

        let mut source = Vec::new();
        if let Err(err) = lura_js::dump_into_string(manifest.db, current_source, &mut source) {
          eprintln!("{err}");
        }

        println!("{}", String::from_utf8(source)?);
      }

      lura_ariadne::AriadneReport::default()
        .expand(manifest.diagnostics.into_iter().collect_vec())
        .eprint()?;
    }
    Command::TypeCheck => todo!(),
  }
  Ok(())
}
