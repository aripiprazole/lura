use std::{any::Any, fmt::Debug, hash::Hash, sync::Arc};

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

/// Represents a specific location into the source string
/// as a utf-8 offset.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location(usize);

/// Represents an offset in the source program relative to some anchor.
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offset(pub usize);

impl From<usize> for Offset {
    fn from(value: usize) -> Self {
        Offset(value)
    }
}

impl Location {
    pub fn as_usize(self) -> usize {
        self.0
    }

    pub fn start() -> Self {
        Self(0)
    }
}

impl Debug for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Offset({})", self.0)
    }
}

impl std::ops::Add<Offset> for Location {
    type Output = Location;

    fn add(self, rhs: Offset) -> Self::Output {
        Location(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for Location {
    type Output = Location;

    fn add(self, rhs: usize) -> Self::Output {
        Location(self.0 + rhs)
    }
}

impl std::ops::AddAssign<usize> for Location {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs
    }
}

impl std::ops::Sub<Location> for Location {
    type Output = Offset;

    fn sub(self, rhs: Location) -> Self::Output {
        Offset(self.0 - rhs.0)
    }
}

#[salsa::jar(db = DiagnosticDb)]
pub struct Jar(crate::Diagnostics);

#[salsa::accumulator]
pub struct Diagnostics(Report);

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
    pub fn new(diagnostic: impl Diagnostic + 'static) -> Self {
        Self {
            diagnostic: Arc::new(diagnostic),

            #[cfg(debug_assertions)]
            internal_location: *core::panic::Location::caller(),
        }
    }

    /// Gets a dynamic reference to the diagnostic. This is useful
    /// when you want to downcast the diagnostic to a specific type.
    pub fn into_inner(self) -> Arc<dyn DiagnosticDyn> {
        self.diagnostic
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

pub trait Diagnostic: Any + Debug + Send + Sync {
    type TextRange: TextRange;

    const KIND: ErrorKind;

    fn text(&self) -> Vec<ErrorText>;
    fn location(&self) -> Option<Self::TextRange>;
}

pub trait DiagnosticDyn: Any + Debug + Send + Sync {
    fn error_kind(&self) -> ErrorKind;
    fn text(&self) -> Vec<ErrorText>;
    fn location(&self) -> Option<Box<dyn TextRange>>;
}

impl Hash for dyn DiagnosticDyn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.error_kind().hash(state);
        self.text().hash(state);
        self.location().hash(state);
    }
}

impl<T: Diagnostic> DiagnosticDyn for T {
    fn error_kind(&self) -> ErrorKind {
        Self::KIND
    }

    fn text(&self) -> Vec<ErrorText> {
        self.text()
    }

    fn location(&self) -> Option<Box<dyn TextRange>> {
        self.location().map(|it| Box::new(it) as Box<dyn TextRange>)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ErrorText {
    Text(String),
    Code(String),
    Break,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ErrorKind {
    ParseError,
    TypeError,
    ResolutionError,
    RuntimeError,
    InternalError(Box<ErrorKind>),
}

pub trait DiagnosticDb: DbWithJar<Jar> {}

impl<DB> DiagnosticDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
