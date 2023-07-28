use std::{
    hash::{Hash, Hasher},
    ops::Range,
};

use ariadne::Fmt;
use clap::*;

use im::HashMap;
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

#[derive(Debug, Clone)]
struct FileDescriptor {
    path: String,
    content: String,
}

impl Eq for FileDescriptor {}

impl PartialEq for FileDescriptor {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Hash for FileDescriptor {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

fn main() -> eyre::Result<()> {
    let cli = Cli::parse();
    let (tx, _) = crossbeam_channel::unbounded();
    let db = RootDb::new(tx);

    match cli.command {
        Command::Run => {
            let mut manifest = Manifest::load_in_folder(&db, std::env::current_dir()?)?;
            manifest.register_packages()?;
            manifest.resolve_all_files()?;

            let mut diagnostics = HashMap::new();
            for report in manifest.diagnostics.iter() {
                diagnostics
                    .entry(FileDescriptor {
                        path: report.file_name().to_string(),
                        content: report.location().unwrap().source().to_string(),
                    })
                    .or_insert_with(im::HashSet::new)
                    .insert(report.clone());
            }

            for (file, diagnostics) in diagnostics {
                use ariadne::ReportKind::*;

                type Span = (String, Range<usize>);
                ariadne::Report::<Span>::build(Error, file.path.clone(), 0)
                    .with_code("E0001")
                    .with_message(format!("found {} resolution errors", diagnostics.len()))
                    .with_config(
                        ariadne::Config::default()
                            .with_char_set(ariadne::CharSet::Ascii)
                            .with_label_attach(ariadne::LabelAttach::Start),
                    )
                    .with_labels(diagnostics.into_iter().map(|d| {
                        ariadne::Label::new((d.file_name(), d.range().unwrap()))
                            .with_color(ariadne::Color::Red)
                            .with_message(d.markdown_text().fg(ariadne::Color::Red))
                    }))
                    .finish()
                    .eprint((file.path, ariadne::Source::from(&file.content)))
                    .unwrap();
            }
        }
        Command::TypeCheck => todo!(),
    }
    Ok(())
}
