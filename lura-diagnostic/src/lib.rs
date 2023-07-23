use std::{any::Any, fmt::Debug, sync::Arc};

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

/// Represents a specific location into the source string
/// as a utf-8 offset.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location(usize);

/// Represents an offset in the source program relative to some anchor.
#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Clone, Debug)]
pub struct Report(pub Arc<dyn DiagnosticDyn>);

impl Report {
    pub fn new(diagnostic: impl Diagnostic + 'static) -> Self {
        Self(Arc::new(diagnostic))
    }

    pub fn into_inner(self) -> Arc<dyn DiagnosticDyn> {
        self.0
    }
}

pub trait TextRange {
    fn file_name(&self) -> &str;
    fn start(&self) -> Offset;
    fn end(&self) -> Offset;
    fn source(&self) -> &str;
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
