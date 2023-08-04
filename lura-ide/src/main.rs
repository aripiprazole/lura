#![feature(async_closure)]

use lura_hir::source::DefaultWithDb;
use std::sync::{Arc, RwLock};
use tower_lsp::lsp_types::SemanticTokenType;
use tower_lsp::{LspService, Server};
use workspace::Workspace;

use crate::backend::Backend;
use lura_driver::RootDb;

mod backend;
mod completion;
mod highlighter;
mod workspace;
mod functions;

const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
];

#[tokio::main]
async fn main() {
    env_logger::init();

    let db = RootDb::default();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        workspace: Arc::new(Workspace::default_with_db(&db)),
        db: Arc::new(RwLock::new(db)),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
