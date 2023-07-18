use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

extern crate salsa_2022 as salsa;

/// The root database implementation for the Lura frontend, after the frontend, there's the
/// codegen backend, and it's not mean't to use incremental compilation, so it's not part of the
/// database.
///
/// The database is used to store the results of the various passes of the compiler, and to
/// invalidate them when the input changes.
///
/// The database is also used to store the logs of the compiler, so that they can be printed.
#[derive(Default)]
#[salsa::db(lura_hir::Jar)]
pub struct RootDb {
    /// Salsa storage, used to store the results of the various passes of the compiler.
    storage: salsa::Storage<RootDb>,

    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl salsa::Database for RootDb {
    fn salsa_event(&self, event: salsa::Event) {
        // Log interesting events, if logging is enabled
        if let Some(logs) = &self.logs {
            // don't log boring events
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                logs.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}

impl salsa::ParallelDatabase for RootDb {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
        })
    }
}

#[cfg(test)]
#[allow(clippy::unnecessary_mut_passed)]
mod tests {
    use lura_hir::program::SourceProgram;
    use salsa_2022::DebugWithDb;

    use crate::RootDb;

    const EXAMPLE: &str = "Main { IO.println \"Hello, world\" }";

    /// This is an end-to-end test of the pipeline, from parsing to type checking/compiling, etc,
    /// it's not a unit test.
    ///
    /// This test is meant to be run with `cargo test -- --nocapture` so that the logs are printed.
    #[test]
    fn pipeline_tests() {
        let mut root = RootDb::default();
        let program = SourceProgram::new(&mut root, "example.lura".into(), EXAMPLE.into());
        println!("{:?}", program.into_debug_all(&mut root));
    }
}
