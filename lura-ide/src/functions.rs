use itertools::Itertools;
use tower_lsp::{jsonrpc::Result, lsp_types::*};

use crate::backend::Backend;

/// Defines the handler for the `textDocument/semanticTokens/range` method.
///
/// The `textDocument/semanticTokens/range` method is used to get semantic
/// tokens for a given range in a text document.
pub mod semantic_tokens_range {
  use super::*;
  use crate::{highlighter::semantic_highlight, LEGEND_TYPE};

  pub type Input = SemanticTokensRangeParams;

  pub type Output = Result<Option<SemanticTokensRangeResult>>;

  pub async fn invoke(server: &Backend, params: Input) -> Output {
    let uri = params.text_document.uri.to_string();
    let semantic_tokens = || -> Option<Vec<SemanticToken>> {
      let rope = server.workspace.file_map.get(&uri)?;
      let tokens = semantic_highlight(&rope.to_string());

      let mut pre_line = 0;
      let mut pre_start = 0;

      tokens
        .iter()
        .filter_map(|token| {
          let line = rope.try_byte_to_line(token.start).ok()? as u32;
          let first = rope.try_line_to_char(line as usize).ok()? as u32;
          let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
          let token_type = LEGEND_TYPE
            .iter()
            .position(|t| *t == token.token_type)
            .map(|i| i as u32)?;
          let delta_line = line - pre_line;
          let delta_start = if delta_line == 0 {
            start - pre_start
          } else {
            start
          };
          let token = Some(SemanticToken {
            delta_line,
            delta_start,
            length: token.length as u32,
            token_type,
            token_modifiers_bitset: 0,
          });
          pre_line = line;
          pre_start = start;
          token
        })
        .collect_vec()
        .into()
    };

    Ok(semantic_tokens().map(|tokens| {
      SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data: tokens,
      })
    }))
  }
}

/// Defines the handler for the `textDocument/semanticTokens/full` method.
///
/// The `textDocument/semanticTokens/full` method is used to get semantic
/// tokens for a given text document.
pub mod semantic_tokens_full {
  use super::*;
  use crate::{highlighter::semantic_highlight, LEGEND_TYPE};

  pub type Input = SemanticTokensParams;
  pub type Output = Result<Option<SemanticTokensResult>>;

  pub async fn invoke(server: &Backend, params: Input) -> Output {
    let uri = params.text_document.uri.to_string();
    let semantic_tokens = || -> Option<Vec<SemanticToken>> {
      let rope = server.workspace.file_map.get(&uri)?;
      let tokens = semantic_highlight(&rope.to_string());

      let mut pre_line = 0;
      let mut pre_start = 0;

      tokens
        .iter()
        .filter_map(|token| {
          let line = rope.try_byte_to_line(token.start).ok()? as u32;
          let first = rope.try_line_to_char(line as usize).ok()? as u32;
          let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
          let token_type = LEGEND_TYPE
            .iter()
            .position(|t| *t == token.token_type)
            .map(|i| i as u32)?;
          let delta_line = line - pre_line;
          let delta_start = if delta_line == 0 {
            start - pre_start
          } else {
            start
          };
          let token = Some(SemanticToken {
            delta_line,
            delta_start,
            length: token.length as u32,
            token_type,
            token_modifiers_bitset: 0,
          });
          pre_line = line;
          pre_start = start;
          token
        })
        .collect_vec()
        .into()
    };

    Ok(semantic_tokens().map(|tokens| {
      SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: tokens,
      })
    }))
  }
}

/// Defines the handler for the `didOpen` notification.
///
/// The `didOpen` notification is sent from the client to the server to signal
/// newly opened text documents. The document's truth is now managed by the
/// client and the server must not try to read the document's truth using the
/// document's uri.
pub mod did_open {
  use super::*;

  pub type Input = DidOpenTextDocumentParams;
  pub type Output = ();

  pub async fn invoke(server: &Backend, params: Input) -> Output {
    log::info!("file opened!");

    server
      .on_change(crate::backend::TextDocumentItem {
        uri: params.text_document.uri,
        text: params.text_document.text,
        version: params.text_document.version,
      })
      .await
  }
}

/// Defines the handler for the `didChange` notification.
///
/// The `didChange` notification is sent from the client to the server to signal
/// changes to a text document.
pub mod did_change {
  use super::*;

  pub type Input = DidChangeTextDocumentParams;
  pub type Output = ();

  pub async fn invoke(server: &Backend, params: Input) -> Output {
    log::info!("file changed!");

    server
      .on_change(crate::backend::TextDocumentItem {
        uri: params.text_document.uri,
        text: params.content_changes[0].text.clone(),
        version: params.text_document.version,
      })
      .await
  }
}

/// Defines the handler for the `completions` notification.
///
/// The `completions` notification is sent from the client to the server to
/// request code completion at a given text document position.
pub mod completions {
  use super::*;

  pub type Input = CompletionParams;
  pub type Output = Result<Option<CompletionResponse>>;

  pub async fn invoke(server: &Backend, params: Input) -> Output {
    let new_completions = vec![CompletionItem::new_simple("Hello".into(), "World".into())];

    let path = params.text_document_position.text_document.uri.clone();
    server
      .client
      .log_message(MessageType::INFO, format!("completion: {}", path))
      .await;

    if let Some(hir_source) = server.hir_source(path.clone()) {
      return Ok(
        server
          .completions(params.text_document_position, hir_source)
          .await,
      );
    }

    Ok(Some(CompletionResponse::Array(new_completions)))
  }
}
