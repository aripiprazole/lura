use lura_hir::{reparse::reparse_hir_path, resolve::Definition};
use salsa_2022::Durability;

use crate::RootDb;

impl RootDb {
    /// Rename a definition, with a new name. It does not update the references to the definition.
    pub fn rename(&mut self, definition: Definition, new_name: String) -> Definition {
        let name = definition.name(&*self);

        // Update the name of the definition
        //
        // The segments should be updated too, within a query that reorganize the [`CallSite`]
        // locations, to be coherent with the new name.
        let segments = reparse_hir_path(&*self, new_name);
        name.set_segments(&mut *self)
            .with_durability(Durability::LOW)
            .to(segments);

        definition
    }
}
