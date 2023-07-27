use std::sync::Arc;
use tower_lsp::lsp_types::SemanticTokenType;
use tower_lsp::{LspService, Server};

use crate::backend::Backend;
use lura_driver::RootDb;

mod backend;
mod highlighter;
mod workspace;

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

    let (tx, _) = crossbeam_channel::unbounded();
    let db = RootDb::new(tx);

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        db: Arc::new(db),
        workspace: Arc::default(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
