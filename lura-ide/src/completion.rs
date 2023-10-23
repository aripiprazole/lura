use lura_diagnostic::Offset;
use lura_driver::RootDb;
use lura_hir::{
  completions::{completions, HirCompletion, Position},
  solver::DefinitionKind,
  source::HirSource,
};
use tower_lsp::lsp_types::{
  CompletionItem, CompletionItemKind, CompletionResponse, Documentation, MessageType,
  TextDocumentPositionParams,
};

use crate::backend::Backend;

impl Backend {
  pub async fn completions(
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
    self
      .client
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

pub fn hir_completion_kind(completion: &HirCompletion) -> CompletionItemKind {
  match completion.kind {
    DefinitionKind::Function => CompletionItemKind::FUNCTION,
    DefinitionKind::Constructor => CompletionItemKind::CONSTRUCTOR,
    DefinitionKind::Type => CompletionItemKind::VALUE,
    DefinitionKind::Variable => CompletionItemKind::VARIABLE,
    DefinitionKind::Module => CompletionItemKind::MODULE,
    DefinitionKind::Command => CompletionItemKind::REFERENCE,
    DefinitionKind::Unresolved => CompletionItemKind::TEXT,
    DefinitionKind::Trait => CompletionItemKind::CLASS,
  }
}

pub fn hir_detail(completion: &HirCompletion) -> String {
  match completion.kind {
    DefinitionKind::Function => format!("Function `{}`", completion.name),
    DefinitionKind::Constructor => format!("Constructor `{}`", completion.name),
    DefinitionKind::Type => format!("Type `{}`", completion.name),
    DefinitionKind::Variable => format!("Variable `{}`", completion.name),
    DefinitionKind::Module => format!("Module `{}`", completion.name),
    DefinitionKind::Command => format!("Command `{}`", completion.name),
    DefinitionKind::Unresolved => format!("Unresolved `{}`", completion.name),
    DefinitionKind::Trait => format!("Trait `{}`", completion.name),
  }
}

pub fn hir_docs(_db: &RootDb, _completion: &HirCompletion) -> Option<Documentation> {
  None
}
