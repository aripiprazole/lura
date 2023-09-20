use std::sync::Arc;

use lura_diagnostic::{Diagnostic, Diagnostics, ErrorKind, ErrorText, Offset, Report, TextRange};

use crate::Source;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxLocation {
  pub start: Offset,
  pub end: Offset,

  pub file_name: String,
  pub text: Arc<String>,
}

impl TextRange for SyntaxLocation {
  fn start(&self) -> Offset {
    self.start
  }

  fn end(&self) -> Offset {
    self.end
  }

  fn file_name(&self) -> &str {
    &self.file_name
  }

  fn source(&self) -> &str {
    &self.text
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxDiagnostic {
  pub location: SyntaxLocation,
  pub message: String,
}

#[salsa::tracked]
pub struct SyntaxError {
  pub diagnostic: SyntaxDiagnostic,
}

impl Diagnostic for SyntaxDiagnostic {
  type TextRange = SyntaxLocation;

  const KIND: ErrorKind = ErrorKind::ParseError;

  fn text(&self) -> Vec<lura_diagnostic::ErrorText> {
    vec![ErrorText::Text(self.message.clone())]
  }

  fn location(&self) -> Option<Self::TextRange> {
    Some(self.location.clone())
  }
}

#[salsa::tracked]
impl Source {
  /// Defines the [`Source::errors`] query.
  ///
  /// Returns a list of syntax errors in this program. This query is memoized, so it will only be
  /// executed once for each program.
  #[salsa::tracked]
  pub fn errors(self, db: &dyn crate::ParseDb) -> Vec<SyntaxError> {
    let node = self.syntax_node(db).as_ref().root_node();

    let mut stack = vec![node];
    let mut errors = vec![];

    let file_name = self.file_path(db).to_string_lossy().to_string();
    let text = Arc::new(self.source_text(db).clone());

    while let Some(node) = stack.pop() {
      for child in node.children(&mut node.walk()) {
        stack.push(child);
      }

      let location = SyntaxLocation {
        start: Offset(node.start_byte()),
        end: Offset(node.end_byte()),
        file_name: file_name.clone(),
        text: text.clone(),
      };

      match true {
        _ if node.has_error() && node.is_missing() => {
          errors.push(SyntaxError::new(db, SyntaxDiagnostic {
            // The error message is the node's S-expression.
            message: node.to_sexp().to_lowercase(),
            location,
          }));
        }
        _ if node.is_error() => {
          let mut cursor = node.walk();
          let unexpected = node
            .children(&mut cursor)
            .flat_map(|node| node.utf8_text(text.as_bytes()))
            .map(|pao| pao.to_string())
            .collect::<Vec<_>>()
            .join(" ");

          errors.push(SyntaxError::new(db, SyntaxDiagnostic {
            message: format!("unexpected token(s): {unexpected}"),
            location,
          }));
        }
        _ => {}
      };
    }

    errors
  }

  /// Defines the [`Source::validated`] query.
  ///
  /// Validates a Lura program. This query is memoized, so it will only be executed once for each
  /// program.
  ///
  /// This query will also report any errors that it finds to the diagnostics database.
  #[salsa::tracked]
  pub fn validated(self, db: &dyn crate::ParseDb) -> Source {
    for error in self.errors(db) {
      Diagnostics::push(db, Report::new(error.diagnostic(db)));
    }

    self
  }
}
