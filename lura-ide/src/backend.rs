use crate::completion::{hir_completion_kind, hir_detail, hir_docs};
use crate::highlighter::semantic_highlight;
use itertools::Itertools;
use lura_diagnostic::Offset;
use lura_hir::completions::{completions, Position};
use lura_hir::source::HirSource;
use std::sync::{Arc, RwLock, RwLockReadGuard};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{async_trait, LanguageServer};

use crate::LEGEND_TYPE;

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
    async fn on_change(&self, params: TextDocumentItem) {
        self.client
            .log_message(MessageType::INFO, format!("on_change: {}", params.uri))
            .await;
        self.get_or_create_file(params);
    }

    async fn completions(
        &self,
        params: TextDocumentPositionParams,
        hir_source: HirSource,
    ) -> Option<CompletionResponse> {
        let path = params.text_document.uri.to_string();
        let text_file = self.workspace.file_map.get(&path).unwrap();
        let searching_for_name = Default::default();
        let offset = text_file
            .try_line_to_char(params.position.line as usize)
            .ok()?;
        let position = Position {
            offset: Offset(offset + (params.position.character as usize)),
        };

        // log for client
        self.client
            .log_message(
                MessageType::INFO,
                format!("trying completions at {:?}", position.offset.0),
            )
            .await;

        let completions = completions(&*self.db(), hir_source, searching_for_name, position);

        let new_completions = completions
            .into_iter()
            .map(|c| CompletionItem {
                label: c.name.clone(),
                kind: Some(hir_completion_kind(&c)),
                detail: Some(hir_detail(&c)),
                documentation: hir_docs(&self.db(), &c),
                ..CompletionItem::default()
            })
            .collect();

        Some(CompletionResponse::Array(new_completions))
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(false)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let rope = self.workspace.file_map.get(&uri)?;
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let rope = self.workspace.file_map.get(&uri)?;
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

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!("file opened!");

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let new_completions = vec![CompletionItem::new_simple("Hello".into(), "World".into())];

        let path = params.text_document_position.text_document.uri.clone();
        self.client
            .log_message(MessageType::INFO, format!("completion: {}", path))
            .await;

        if let Some(hir_source) = self.hir_source(path.clone()) {
            return Ok(self
                .completions(params.text_document_position, hir_source)
                .await);
        }

        Ok(Some(CompletionResponse::Array(new_completions)))
    }
}

fn _assert_send_sync() {
    fn _assert<T: Sync + Send>() {}

    _assert::<Backend>();
}
