use std::{fmt::Debug, sync::Arc};

use lura_diagnostic::Offset;
use lura_hir::source::HirSource;

#[salsa::tracked]
pub struct ThirSource {
    pub source: HirSource,
    pub type_table: crate::table::TypeTable,
}

/// A text range in a source file with a file name and the source text. This is the concrete
/// implementation of [`Location`].
///
/// [`Location`]: crate::Location
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ThirTextRange {
    pub source: HirSource,
    pub start: Offset,
    pub end: Offset,

    pub file_name: String,
    pub text: Arc<String>,
}

/// A location in a source file. It can be either a text range or a lazy location to be evaluated
/// in the `call_site`.
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum ThirLocation {
    /// A text range in a source file.
    TextRange(ThirTextRange),
    CallSite,
}

impl Debug for ThirLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TextRange(range) => f
                .debug_struct("TextRange")
                .field("start", &range.start)
                .field("end", &range.end)
                .field("file_name", &range.file_name)
                .finish(),
            Self::CallSite => write!(f, "CallSite"),
        }
    }
}

impl ThirLocation {
    /// Creates a new [Location] with the given [`source`] and range of [`start`] and [`end`].
    pub fn new<I>(db: &dyn crate::HirDb, src: HirSource, start: I, end: I) -> Self
    where
        I: Into<Offset>,
    {
        Self::TextRange(ThirTextRange {
            source: src,
            start: start.into(),
            end: end.into(),
            file_name: src.source(db).file_path(db).to_string_lossy().into_owned(),
            text: Arc::new(src.source(db).source_text(db).to_string()),
        })
    }

    /// Creates a "call site" location, this is a location that is not resolved yet, and will be
    /// resolved later.
    ///
    /// It takes an [`db`] argument, just to be consistent with the other methods. And if sometime
    /// it will need the db argument, it will be already there, and we won't need to change the
    /// method signature, and the call sites.
    pub fn call_site(_db: &dyn crate::HirDb) -> Self {
        Self::CallSite
    }

    /// Sets the [`end`] of the location. It's useful when we don't know the end of the location
    /// when we create it, but we know it later.
    pub fn ending(self, end: Offset) -> Self {
        match self {
            Self::TextRange(mut range) => {
                range.end = end;
                Self::TextRange(range)
            }
            Self::CallSite => Self::CallSite,
        }
    }
}

/// Represents the diagnostic for High-Level Intermediate Representation. It's intended to be used
/// to report errors to the diagnostic database, by this crate, only.
#[derive(Debug)]
pub struct ThirDiagnostic {
    pub id: lura_diagnostic::ErrorId,
    pub location: ThirLocation,
    pub message: Vec<lura_diagnostic::ErrorText>,
}

impl lura_diagnostic::Diagnostic for ThirDiagnostic {
    type TextRange = ThirLocation;

    const KIND: lura_diagnostic::ErrorKind = lura_diagnostic::ErrorKind::TypeError;

    fn error_id(&self) -> lura_diagnostic::ErrorId {
        self.id
    }

    fn text(&self) -> Vec<lura_diagnostic::ErrorText> {
        self.message.clone()
    }

    fn location(&self) -> Option<Self::TextRange> {
        Some(self.location.clone())
    }
}

impl lura_diagnostic::TextRange for ThirLocation {
    /// Returns the start offset of the source file.
    fn start(&self) -> Offset {
        match self {
            Self::TextRange(range) => range.start,
            Self::CallSite => Offset(0),
        }
    }

    /// Returns the end offset of the source file.
    fn end(&self) -> Offset {
        match self {
            Self::TextRange(range) => range.end,
            Self::CallSite => Offset(0),
        }
    }

    /// Returns the file name of the source file.
    fn file_name(&self) -> &str {
        match self {
            Self::TextRange(range) => &range.file_name,
            Self::CallSite => "unresolved",
        }
    }

    /// Returns the text of the source file.
    fn source(&self) -> &str {
        match self {
            Self::TextRange(range) => &range.text,
            Self::CallSite => "",
        }
    }
}
