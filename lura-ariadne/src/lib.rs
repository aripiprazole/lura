use std::collections::HashMap;

use ariadne::Fmt;
use eyre::Context;
use fxhash::FxBuildHasher;
use lura_diagnostic::Report;

type Span = (String, std::ops::Range<usize>);

/// NOTE: For some reason, rust-analyzer catches [`content`] as
/// not used, but it's used when displaying the errors.
///
/// So we suppress it, by using `#[allow(dead_code)]`
#[derive(Debug, Clone)]
#[allow(dead_code)]
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

impl std::hash::Hash for FileDescriptor {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

#[derive(Default)]
pub struct AriadneReport {
    pub reports: Vec<Report>,
}

impl AriadneReport {
    /// Expands the current report with the given reports.
    pub fn expand(mut self, reports: Vec<Report>) -> Self {
        self.reports.extend(reports);
        self
    }

    /// Prints the report to stderr.
    pub fn eprint(self) -> eyre::Result<()> {
        let errors = self.group_errors_by_file();

        for (file, diagnostics) in errors {
            use ariadne::ReportKind::*;

            ariadne::Report::<Span>::build(Error, file.path.clone(), 0)
                .with_message(format!("found {} errors", diagnostics.len()))
                .with_config(
                    ariadne::Config::default()
                        .with_char_set(ariadne::CharSet::Unicode)
                        .with_label_attach(ariadne::LabelAttach::Start),
                )
                .with_labels(diagnostics.into_iter().map(|d| {
                    let kind = match d.error_kind() {
                        lura_diagnostic::ErrorKind::ParseError => "parse error",
                        lura_diagnostic::ErrorKind::TypeError => "type error",
                        lura_diagnostic::ErrorKind::ResolutionError => "resolution error",
                        lura_diagnostic::ErrorKind::RuntimeError => "runtime error",
                        lura_diagnostic::ErrorKind::InternalError(_) => "internal error",
                    };
                    let message = d.markdown_text();
                    ariadne::Label::new((d.file_name(), d.range().unwrap()))
                        .with_color(ariadne::Color::Red)
                        .with_message(format!("{kind}: {message}").fg(ariadne::Color::Red))
                }))
                .finish()
                .eprint((file.path.clone(), ariadne::Source::from(&file.content)))
                .wrap_err_with(|| format!("failed to print the report for file {}", file.path))?;
        }
        Ok(())
    }

    // Group all errors by file.
    fn group_errors_by_file(&self) -> HashMap<FileDescriptor, Vec<Report>, FxBuildHasher> {
        let mut diagnostics = std::collections::HashMap::default();
        for report in self.reports.iter() {
            diagnostics
                .entry(FileDescriptor {
                    path: report.file_name().to_string(),
                    content: report.location().unwrap().source().to_string(),
                })
                .or_insert_with(Vec::new)
                .push(report.clone());
        }
        diagnostics
    }
}
