#![feature(stmt_expr_attributes)]

use std::{
  any::Any,
  fmt::{Debug, Display},
  hash::Hash,
  ops::Deref,
  sync::Arc,
};

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = DiagnosticDb)]
pub struct Jar(crate::Diagnostics);

/// The diagnostic accumulator, used to store diagnostics, accross
/// salsa revisions.
///
/// This is used to store diagnostics, accross salsa revisions.
#[salsa::accumulator]
pub struct Diagnostics(Report);

pub trait DiagnosticDb: DbWithJar<Jar> {}

impl<DB> DiagnosticDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

pub use reports::Report;

/// Represents a diagnostic. This is used to report errors, warnings, and
/// other kinds of diagnostics.
pub mod reports {
  use super::*;

  /// The diagnostic database. This is used to store diagnostics.
  /// This is used to store diagnostics.
  #[derive(Clone)]
  #[cfg_attr(not(debug_assertions), repr(transparent))]
  pub struct Report {
    pub diagnostic: Arc<dyn DiagnosticDyn>,

    #[cfg(debug_assertions)]
    pub internal_location: core::panic::Location<'static>,
  }

  impl Report {
    /// Creates a new diagnostic report. This is the only way to create
    /// a diagnostic report.
    #[track_caller]
    #[inline]
    pub fn new(diagnostic: impl Diagnostic + 'static) -> Self {
      Self {
        diagnostic: Arc::new(diagnostic),

        #[cfg(debug_assertions)]
        internal_location: *core::panic::Location::caller(),
      }
    }

    /// Gets a dynamic reference to the diagnostic. This is useful
    /// when you want to downcast the diagnostic to a specific type.
    #[inline]
    pub fn into_inner(self) -> Arc<dyn DiagnosticDyn> {
      self.diagnostic
    }
  }

  impl Deref for Report {
    type Target = Arc<dyn DiagnosticDyn>;

    fn deref(&self) -> &Self::Target {
      &self.diagnostic
    }
  }

  impl Hash for Report {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      self.diagnostic.error_kind().hash(state);
      self.diagnostic.text().hash(state);
      self.diagnostic.location().hash(state);
    }
  }

  impl Debug for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      self.diagnostic.fmt(f)
    }
  }

  impl Eq for Report {}

  impl PartialEq for Report {
    fn eq(&self, other: &Self) -> bool {
      self.diagnostic.error_kind() == other.diagnostic.error_kind()
        && self.diagnostic.text() == other.diagnostic.text()
        && self.diagnostic.location() == other.diagnostic.location()
    }
  }
}

/// Represents a piece of text that is being reported.
///
/// This is useful to highlight the source code that is
/// being reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ErrorId(pub &'static str);

/// Represents a range of text in the source program.
///
/// This is useful to highlight the source code that is
/// being reported.
pub trait TextRange {
  fn file_name(&self) -> &str;
  fn start(&self) -> Offset;
  fn end(&self) -> Offset;
  fn source(&self) -> &str;
}

impl Hash for dyn TextRange {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.file_name().hash(state);
    self.start().hash(state);
    self.end().hash(state);
    self.source().hash(state);
  }
}

impl PartialEq for dyn TextRange {
  fn eq(&self, other: &Self) -> bool {
    self.file_name() == other.file_name()
      && self.start() == other.start()
      && self.end() == other.end()
      && self.source() == other.source()
  }
}

/// Represents a piece of text that is being reported.
pub trait Diagnostic: Any + Debug + Send + Sync {
  /// The type of the text range that is being reported. This
  /// is useful to group errors of the same kind together.
  type TextRange: TextRange;

  /// The kind of error that is being reported.
  const KIND: ErrorKind;

  /// The id of the error that is being reported. This is useful
  /// to group errors of the same kind together.
  fn error_id(&self) -> ErrorId {
    ErrorId("error")
  }

  /// The text that is being reported.
  fn text(&self) -> Vec<ErrorText>;

  /// The location of the error.
  fn location(&self) -> Option<Self::TextRange>;
}

/// Represents the kind of error that is being reported. This is useful
/// to group errors of the same kind together.
///
/// This trait is dyn because it is used as a key in the salsa database,
/// and it needs to be dynamic.
pub trait DiagnosticDyn: Any + Debug + Send + Sync {
  fn error_id(&self) -> ErrorId;
  fn error_kind(&self) -> ErrorKind;
  fn text(&self) -> Vec<ErrorText>;
  fn location(&self) -> Option<Box<dyn TextRange>>;
}

pub mod dyn_utils {
  use super::*;

  /// A trait that allows downcasting a diagnostic to a specific type.
  impl dyn DiagnosticDyn {
    /// Gets the kind of the diagnostic.
    #[inline(always)]
    pub fn file_name(&self) -> String {
      self
        .location()
        .map(|it| it.file_name().to_string())
        .unwrap_or("~INTERNAL ERROR~".into())
    }

    /// Gets the range of the diagnostic.
    #[inline(always)]
    pub fn range(&self) -> Option<std::ops::Range<usize>> {
      self.location().map(|it| it.start().0..it.end().0)
    }

    /// Gets the markdown text of the diagnostic.
    #[cold]
    #[inline(never)]
    pub fn markdown_text(&self) -> String {
      self
        .text()
        .into_iter()
        .map(|it| match it {
          ErrorText::Text(it) => it,
          ErrorText::Code(it) => format!("`{}`", it),
          ErrorText::Break => "\n".to_string(),
        })
        .collect::<Vec<_>>()
        .join(" ")
    }
  }

  impl Hash for dyn DiagnosticDyn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      self.error_kind().hash(state);
      self.text().hash(state);
      self.location().hash(state);
    }
  }

  /// This is a hack to work around the fact that `DiagnosticDyn` is not object safe. This
  /// is a problem because we want to store `DiagnosticDyn` in a `HashMap`, but we can't
  /// because `DiagnosticDyn` is not object safe.
  ///
  /// This is a problem because `DiagnosticDyn`
  /// is not object safe.
  impl<T: Diagnostic> DiagnosticDyn for T {
    #[inline]
    fn error_kind(&self) -> ErrorKind {
      Self::KIND
    }

    #[inline]
    fn text(&self) -> Vec<ErrorText> {
      self.text()
    }

    #[inline]
    fn location(&self) -> Option<Box<dyn TextRange>> {
      self.location().map(|it| Box::new(it) as Box<dyn TextRange>)
    }

    #[inline]
    fn error_id(&self) -> ErrorId {
      self.error_id()
    }
  }
}

/// A kind of error text. This is used to format the error message.
/// For example, `ErrorText::Code("foo")` will be formatted as `foo`.
#[derive(Clone)]
pub enum ErrorText {
  Text(String),
  Code(Arc<dyn Display + Sync + Send>),
  Break,
}

impl Debug for ErrorText {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrorText::Text(it) => write!(f, "{:?}", it),
      ErrorText::Code(it) => write!(f, "`{}`", it),
      ErrorText::Break => write!(f, "\\n"),
    }
  }
}

impl Eq for ErrorText {}

impl Hash for ErrorText {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    match self {
      ErrorText::Text(it) => it.hash(state),
      ErrorText::Code(it) => it.to_string().hash(state),
      ErrorText::Break => "\n".hash(state),
    }
  }
}

impl PartialEq for ErrorText {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (ErrorText::Text(a), ErrorText::Text(b)) => a == b,
      (ErrorText::Code(a), ErrorText::Code(b)) => a.to_string() == b.to_string(),
      (ErrorText::Break, ErrorText::Break) => true,
      _ => false,
    }
  }
}

impl From<String> for ErrorText {
  fn from(value: String) -> Self {
    ErrorText::Text(value)
  }
}

impl From<&str> for ErrorText {
  fn from(value: &str) -> Self {
    ErrorText::Text(value.to_string())
  }
}

/// The kind of error that is being reported. This is useful
/// to group errors of the same kind together.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ErrorKind {
  ParseError,
  TypeError,
  ResolutionError,
  RuntimeError,
  InternalError(Box<ErrorKind>),
}

/// Represents an offset in the source program relative to some anchor.
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offset(pub usize);

/// Represents a location in the source program.
pub mod offsets {
  use super::*;

  impl From<usize> for Offset {
    fn from(value: usize) -> Self {
      Offset(value)
    }
  }

  impl Debug for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "Offset({})", self.0)
    }
  }
}

/// Defines a code location in the source program.
#[macro_export]
macro_rules! code {
  ($e:expr) => {
    $crate::ErrorText::Code(std::sync::Arc::new($e))
  };
}

/// Defines a text location in the source program.
#[macro_export]
macro_rules! message {
    ($($x:expr),+ $(,)?) => {
        {
            #[allow(clippy::vec_init_then_push)]
            let mut message: Vec<$crate::ErrorText> = vec![];
            $(message.push($x.into());)+
            message
        }
    };
}
