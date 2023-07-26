use dashmap::DashMap;
use ropey::Rope;
use std::sync::atomic::AtomicBool;
use tree_sitter::Tree;

/// This struct represents the workspace. It is used to store the files that are
/// opened in the editor.
#[derive(Default)]
pub struct Workspace {
    pub file_map: DashMap<String, Rope>,

    /// Tree map is used to store the syntax tree for each file. This is used
    /// to get the syntax tree for a file when the client requests it.
    pub tree_map: DashMap<String, Tree>,

    /// This is used to prevent the server from sending diagnostics before the
    /// workspace is ready, and this is needed because the server will send
    /// diagnostics for all files when it starts up.
    pub ready: AtomicBool,
}
