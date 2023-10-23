use std::sync::{Arc, RwLock, RwLockReadGuard};

use tower_lsp::{async_trait, jsonrpc::Result, lsp_types::*, LanguageServer};

use crate::{functions::*, LEGEND_TYPE};

#[derive(Clone)]
pub struct Backend {
  pub client: tower_lsp::Client,
  pub db: Arc<RwLock<lura_driver::RootDb>>,
  pub workspace: Arc<crate::workspace::Workspace>,
}

/// This struct represents a file that is opened in the editor.
#[allow(dead_code)]
pub struct TextDocumentItem {
  pub text: String,
  pub uri: Url,
  pub version: i32,
}

// TODO: remove this
unsafe impl Sync for Backend {}

// TODO: remove this
unsafe impl Send for Backend {}

impl Backend {
  pub fn db(&self) -> RwLockReadGuard<lura_driver::RootDb> {
    self.db.read().unwrap()
  }

  /// This function is called when a file is opened or changed. It will parse the file and
  /// send the diagnostics to the client.
  pub async fn on_change(&self, params: TextDocumentItem) {
    self
      .client
      .log_message(MessageType::INFO, format!("on_change: {}", params.uri))
      .await;
    self.get_or_create_file(params);
  }

  /// Defines the handler for the `initialize` request. It does return
  /// the capabilities of the language server.
  fn default_initialize_result() -> InitializeResult {
    InitializeResult {
      server_info: None,
      capabilities: ServerCapabilities {
        inlay_hint_provider: Some(OneOf::Left(false)),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
          resolve_provider: Some(false),
          trigger_characters: Some(vec![".".to_string()]),
          work_done_progress_options: Default::default(),
          all_commit_characters: None,
          completion_item: None,
        }),
        execute_command_provider: Some(ExecuteCommandOptions {
          commands: vec![],
          work_done_progress_options: Default::default(),
        }),
        workspace: Some(WorkspaceServerCapabilities {
          workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            supported: Some(true),
            change_notifications: Some(OneOf::Left(true)),
          }),
          file_operations: None,
        }),
        semantic_tokens_provider: Some(
          SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
            SemanticTokensRegistrationOptions {
              text_document_registration_options: TextDocumentRegistrationOptions {
                document_selector: Some(vec![DocumentFilter {
                  language: Some("lura".to_string()),
                  scheme: Some("file".to_string()),
                  pattern: None,
                }]),
              },
              semantic_tokens_options: SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                  token_types: LEGEND_TYPE.into(),
                  token_modifiers: vec![],
                },
                range: Some(true),
                full: Some(SemanticTokensFullOptions::Bool(true)),
              },
              static_registration_options: StaticRegistrationOptions::default(),
            },
          ),
        ),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
      },
    }
  }
}

/// This trait is used to implement the Language Server Protocol.
#[async_trait]
impl LanguageServer for Backend {
  async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
    Ok(Self::default_initialize_result())
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }

  async fn semantic_tokens_range(
    &self,
    params: semantic_tokens_range::Input,
  ) -> semantic_tokens_range::Output {
    semantic_tokens_range::invoke(self, params).await
  }

  async fn semantic_tokens_full(
    &self,
    params: semantic_tokens_full::Input,
  ) -> semantic_tokens_full::Output {
    semantic_tokens_full::invoke(self, params).await
  }

  async fn did_open(&self, params: did_open::Input) {
    did_open::invoke(self, params).await
  }

  async fn did_change(&self, params: did_change::Input) {
    did_change::invoke(self, params).await
  }

  async fn completion(&self, params: completions::Input) -> completions::Output {
    completions::invoke(self, params).await
  }
}

fn _assert_send_sync() {
  fn _assert<T: Sync + Send>() {}

  _assert::<Backend>();
}
