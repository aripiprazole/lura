use lura_driver::RootDb;
use lura_hir::{completions::HirCompletion, resolve::DefinitionKind};
use tower_lsp::lsp_types::{CompletionItemKind, Documentation};

pub fn hir_completion_kind(completion: &HirCompletion) -> CompletionItemKind {
    match completion.kind {
        DefinitionKind::Function => CompletionItemKind::FUNCTION,
        DefinitionKind::Constructor => CompletionItemKind::CONSTRUCTOR,
        DefinitionKind::Type => CompletionItemKind::VALUE,
        DefinitionKind::Variable => CompletionItemKind::VARIABLE,
        DefinitionKind::Module => CompletionItemKind::MODULE,
        DefinitionKind::Command => CompletionItemKind::REFERENCE,
        DefinitionKind::Unresolved => CompletionItemKind::TEXT,
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
    }
}

pub fn hir_docs(_db: &RootDb, _completion: &HirCompletion) -> Option<Documentation> {
    None
}
