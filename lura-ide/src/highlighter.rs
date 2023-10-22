use tower_lsp::lsp_types::SemanticTokenType;
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

const HIGHLIGHT_NAMES: &[&str] = &[
  "attribute",
  "constant",
  "number",
  "function.builtin",
  "function.method",
  "function",
  "keyword",
  "operator",
  "property",
  "punctuation",
  "punctuation.bracket",
  "punctuation.delimiter",
  "string",
  "string.special",
  "tag",
  "type",
  "type.builtin",
  "variable",
  "variable.builtin",
  "variable.parameter",
];

fn lsp_token_type(name: &str) -> Option<SemanticTokenType> {
  match name {
    "attribute" => Some(SemanticTokenType::DECORATOR),
    "keyword" => Some(SemanticTokenType::KEYWORD),
    "comment" => Some(SemanticTokenType::COMMENT),
    "constant" => Some(SemanticTokenType::NUMBER),
    "number" => Some(SemanticTokenType::NUMBER),
    "string" => Some(SemanticTokenType::STRING),
    "operator" => Some(SemanticTokenType::OPERATOR),
    "variable" => Some(SemanticTokenType::VARIABLE),
    "variable.builtin" => Some(SemanticTokenType::VARIABLE),
    "variable.parameter" => Some(SemanticTokenType::PARAMETER),
    "function" => Some(SemanticTokenType::FUNCTION),
    "function.builtin" => Some(SemanticTokenType::FUNCTION),
    "function.method" => Some(SemanticTokenType::METHOD),
    _ => None,
  }
}

#[derive(Debug)]
pub struct LuraSemanticToken {
  pub start: usize,
  pub length: usize,
  pub token_type: SemanticTokenType,
}

pub fn semantic_highlight(text: &str) -> Vec<LuraSemanticToken> {
  let mut highlighter = Highlighter::new();
  let mut config = HighlightConfiguration::new(
    /* language   = */ tree_sitter_lura::language(),
    /* highlights = */ tree_sitter_lura::HIGHLIGHTS_QUERY,
    /* injections = */ "",
    /* locals     = */ tree_sitter_lura::LOCALS_QUERY,
  )
  .unwrap();

  // Configure permitted highlight names into config
  config.configure(HIGHLIGHT_NAMES);

  let tokens = highlighter
    .highlight(&config, text.as_bytes(), None, |_| None)
    .unwrap();

  let mut semantic_tokens = Vec::new();
  let mut highlights = Vec::new();
  for event in tokens {
    match event {
      Ok(HighlightEvent::HighlightStart(s)) => {
        highlights.push(s);
      }
      Ok(HighlightEvent::HighlightEnd) => {
        highlights.pop();
      }
      Ok(HighlightEvent::Source { start, end }) => {
        let Some(Highlight(highlight)) = highlights.last().copied() else {
          continue;
        };
        let name = HIGHLIGHT_NAMES[highlight];

        if let Some(token_type) = lsp_token_type(name) {
          semantic_tokens.push(LuraSemanticToken {
            length: end - start,
            start,
            token_type,
          });
        }
      }
      Err(_) => break,
    }
  }

  semantic_tokens
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn works() {
    println!("{:?}", semantic_highlight("Main {}"));
  }
}
