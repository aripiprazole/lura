use std::{collections::HashMap, io::Write};

use ariadne::{Color, Fmt};
use itertools::Itertools;
use lura_ariadne::AriadneReport;
use lura_diagnostic::{Report, TextRange};
use lura_hir::{fmt::HirFormatter, source::HirElement};
use lura_typer::table::TypeTable;
use salsa_2022::DebugWithDb;
use similar::{ChangeTag, TextDiff};

use crate::RootDb;

#[macro_export]
macro_rules! make_test {
  ($name:ident, $run:expr) => {
    #[test]
    fn $name() {
      let source_code =
        std::fs::read_to_string(concat!("suite/", stringify!($name), ".lura")).unwrap();
      let expect = std::fs::read_to_string(concat!("suite/", stringify!($name), ".expect"))
        .unwrap_or_default();
      $crate::suite::run_test_suite(stringify!($name), &source_code, &expect, $run);
    }
  };
  ($name:ident, $file:expr, $run:expr) => {
    #[test]
    fn $name() {
      let source_code = std::fs::read_to_string(concat!("suite/", $file, ".lura")).unwrap();
      let expect = std::fs::read_to_string(concat!("suite/", $file, ".expect")).unwrap_or_default();
      $crate::suite::run_test_suite($file, &source_code, &expect, $run);
    }
  };
}

#[macro_export]
macro_rules! make_test_suite {
  (tests ($e:expr) {$($name:ident $file:expr)*} run $run:expr) => {
    const _: &str = $e;
    $($crate::make_test!($name, $file, $run);)*
  };
}

type SourceCode = String;
type Expect<'a> = &'a mut dyn Write;

/// Runs a test suite, with the given `name` and `f`.
pub fn run_test_suite(
  file: &str,
  source_code: &str,
  expect: &str,
  f: impl FnOnce(RootDb, SourceCode, Expect) -> lura_eyre::Result<()>,
) {
  let _ = env_logger::builder()
    .is_test(true)
    .filter_level(log::LevelFilter::Debug)
    .filter_module("salsa_2022", log::LevelFilter::Off)
    .try_init();

  let db = RootDb::default();
  let mut output = Vec::new();
  if let Err(err) = f(db, source_code.into(), &mut output) {
    panic!("{}", err);
  }

  let output = strip_ansi_escapes::strip_str(String::from_utf8_lossy(&output));
  if expect.is_empty() {
    std::fs::write(format!("suite/{file}.expect"), output).unwrap();
    return;
  }
  if output != expect {
    let diff = TextDiff::from_lines(expect, &output);
    for change in diff.iter_all_changes() {
      let sign = match change.tag() {
        ChangeTag::Delete => format!("- {change}").fg(Color::Red),
        ChangeTag::Insert => format!("+ {change}").fg(Color::Green),
        ChangeTag::Equal => format!("  {change}").fg(Color::White),
      };
      print!("{}", sign);
    }
    println!();
    panic!("The expected output does not match the actual output.");
  }
}

/// Groups the errors by file.
pub fn push_ariadne_errors(output: Expect, outputs: &[Vec<Report>]) -> lura_eyre::Result<()> {
  let mut ariadne = AriadneReport::default();
  for output in outputs {
    ariadne = ariadne.expand(output.clone());
  }
  ariadne.write(output)?;
  Ok(())
}

/// Prints a debug report of the given `type_table`.
pub fn debug_type_table(
  expect: Expect,
  db: &RootDb,
  type_table: TypeTable,
) -> lura_eyre::Result<()> {
  let mut output = Vec::new();
  let expressions = type_table.expressions(db);

  let mut parameters = HashMap::new();
  for (parameter, type_rep) in type_table.parameters(db) {
    let location = parameter.location(db);
    let file_name = location.file_name().to_string();

    parameters
      .entry(file_name)
      .or_insert_with(Vec::new)
      .push((parameter, type_rep));
  }

  let file_type_tables = expressions.iter().group_by(|(expression, _)| {
    let location = expression.location(db);
    let file_name = location.file_name().to_string();
    let contents = location.source().to_string();
    (file_name, contents)
  });

  for ((file, contents), type_table) in file_type_tables.into_iter() {
    // Get all the parameters contained in the current file,
    // to be able to print them in the report.
    let parameters = parameters.get(&file).cloned().unwrap_or_default();

    type Span = (String, std::ops::Range<usize>);
    ariadne::Report::<Span>::build(ariadne::ReportKind::Advice, file.clone(), 0)
      .with_message("type table information")
      .with_note("These are generated types, they are not part of the source code.")
      .with_config(
        ariadne::Config::default()
          .with_cross_gap(true)
          .with_char_set(ariadne::CharSet::Unicode)
          .with_label_attach(ariadne::LabelAttach::Start),
      )
      .with_labels(parameters.into_iter().map(|(parameter, type_rep)| {
        let location = parameter.location(db);
        let range = location.start().0..location.end().0;

        // TODO: use a better representation for types
        let type_rep = type_rep.to_string();

        // Build pattern string
        let pattern = parameter.binding(db);
        let pattern = pattern.formatter();
        let pattern = format!("{:?}", pattern.debug_all(db));

        ariadne::Label::new((file.clone(), range))
          .with_color(Color::Yellow)
          .with_message(format!(
            "parameter {} has type {}",
            pattern.fg(Color::Yellow),
            type_rep.fg(Color::Red)
          ))
      }))
      .with_labels(type_table.into_iter().map(|(expr, type_rep)| {
        let location = expr.location(db);
        let range = location.start().0..location.end().0;

        // TODO: use a better representation for types
        let type_rep = type_rep.to_string();

        ariadne::Label::new((file.clone(), range))
          .with_color(Color::Green)
          .with_message(format!("has type {}", type_rep.fg(Color::Green)))
      }))
      .finish()
      .write(
        (file.clone(), ariadne::Source::from(&contents)),
        &mut output,
      )
      .unwrap();

    write!(expect, "{}", String::from_utf8_lossy(&output))?;
  }
  Ok(())
}
