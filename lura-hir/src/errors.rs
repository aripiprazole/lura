use lura_diagnostic::{Diagnostic, ErrorId, ErrorKind, ErrorText};

use crate::source::Location;

/// Represents the diagnostic for High-Level Intermediate Representation. It's intended to be used
/// to report errors to the diagnostic database, by this crate, only.
#[derive(Debug)]
pub struct HirDiagnostic {
  pub location: Location,
  pub message: Vec<ErrorText>,

  /// The error id, used to identify the error in the diagnostic database.
  pub id: ErrorId,
}

impl Diagnostic for HirDiagnostic {
  type TextRange = Location;

  const KIND: ErrorKind = ErrorKind::ResolutionError;

  fn text(&self) -> Vec<lura_diagnostic::ErrorText> {
    self.message.clone()
  }

  fn location(&self) -> Option<Self::TextRange> {
    Some(self.location.clone())
  }

  fn error_id(&self) -> ErrorId {
    self.id
  }
}
