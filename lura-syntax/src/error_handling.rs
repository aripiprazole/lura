use std::sync::Arc;

use lura_diagnostic::{Diagnostic, ErrorKind, ErrorText, Offset, TextRange};

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
                _ if node.is_error() => {
                    errors.push(SyntaxError::new(
                        db,
                        SyntaxDiagnostic {
                            message: format!("unexpected {:?}", node.kind()),
                            location,
                        },
                    ));
                }
                _ if node.is_missing() => {
                    errors.push(SyntaxError::new(
                        db,
                        SyntaxDiagnostic {
                            message: format!("missing {:?}", node.kind()),
                            location,
                        },
                    ));
                }
                _ => {}
            };
        }

        errors
    }
}
