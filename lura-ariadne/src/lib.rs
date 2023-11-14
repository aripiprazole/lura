use std::collections::HashMap;

use ariadne::Fmt;
use fxhash::{FxBuildHasher, FxHashSet};
use lura_diagnostic::Report;
use lura_eyre::Context;

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

  pub fn write(self, output: &mut dyn std::io::Write) -> lura_eyre::Result<()> {
    write!(output, "{}", self.dump()?)?;
    Ok(())
  }

  pub fn dump(self) -> lura_eyre::Result<String> {
    let mut output = Vec::new();
    let errors = self.group_errors_by_file();

    // NOTE: We use a `FxHashSet` to avoid printing the same file
    // multiple times.
    let mut already_reported = FxHashSet::default();

    for (file, diagnostics) in errors {
      use ariadne::ReportKind::*;

      let mut content = file.content.clone();
      if content.is_empty() {
        // NOTE: Just to avoid errors with ariadne, that tries to index
        // the content of the file.
        content = "  ".into();
      }

      ariadne::Report::<Span>::build(Error, file.path.clone(), 0)
        .with_message(format!("found {} errors", diagnostics.len()))
        .with_config(
          ariadne::Config::default()
            .with_char_set(ariadne::CharSet::Unicode)
            .with_label_attach(ariadne::LabelAttach::Start),
        )
        .with_labels(diagnostics.into_iter().filter_map(|d| {
          // If we already reported this error, we don't need to
          // report it again.
          if already_reported.get(&d).is_some() {
            return None;
          }

          // Remember that we already reported this error.
          already_reported.insert(d.clone());

          let kind = match d.error_kind() {
            lura_diagnostic::ErrorKind::ParseError => "parse error",
            lura_diagnostic::ErrorKind::TypeError => "type error",
            lura_diagnostic::ErrorKind::ResolutionError => "resolution error",
            lura_diagnostic::ErrorKind::RuntimeError => "runtime error",
            lura_diagnostic::ErrorKind::InternalError(_) => "internal error",
          };
          let message = d.markdown_text();

          Some(
            ariadne::Label::new((d.file_name(), d.range().unwrap()))
              .with_color(ariadne::Color::Red)
              .with_message(format!("{kind}: {message}").fg(ariadne::Color::Red)),
          )
        }))
        .finish()
        .write(
          (file.path.clone(), ariadne::Source::from(&content)),
          &mut output,
        )
        .wrap_err_with(|| format!("failed to print the report for file {}", file.path))?;
    }

    Ok(String::from_utf8_lossy(&output).into_owned())
  }

  /// Prints the report to stderr.
  pub fn eprint(self) -> lura_eyre::Result<()> {
    println!("{}", self.dump()?);
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
