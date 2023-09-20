use std::collections::HashSet;

use fxhash::FxBuildHasher;
use lura_diagnostic::{Offset, TextRange};
use lura_hir::{
  reparse::reparse_hir_path,
  solver::{references, Definition},
  source::{HirLocation, Location},
};
use salsa_2022::Durability;

use crate::RootDb;

/// Renames results containing the text edits to apply to the source code. It
#[derive(Debug, Clone)]
pub struct RenamesResult {
  pub edits: HashSet<TextEdit, FxBuildHasher>,
  pub definition: Definition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextEdit {
  pub old_location: Location,
  pub new_text: String,
}

impl RootDb {
  /// Rename a definition, with a new name. It does not update the references to the definition.
  pub fn rename(&mut self, definition: Definition, new_name: &str) -> RenamesResult {
    let mut edits = vec![TextEdit {
      old_location: definition.name(&*self).location(&*self),
      new_text: new_name.to_string(),
    }];
    let name = definition.name(&*self);

    // Update the name of the definition
    //
    // The segments should be updated too, within a query that reorganize the [`CallSite`]
    // locations, to be coherent with the new name.
    let old_location = name.location(&*self);
    let Offset(end_offset) = old_location.end();
    let location = old_location.ending(Offset(end_offset + new_name.len()));
    let hir_location = HirLocation::new(&*self, location);
    let segments = reparse_hir_path(&*self, hir_location, new_name.to_string());

    // Update the references to the definition
    let references = references(&*self, definition);

    name
      .set_segments(&mut *self)
      .with_durability(Durability::LOW)
      .to(segments);

    for reference in references {
      // Create a proper text edit to apply to the source code.
      edits.push(TextEdit {
        old_location: reference.location(&*self),
        new_text: new_name.to_string(),
      });

      // Set the name within new location
      let location = reference.location(&*self);
      let Offset(end_offset) = location.end();

      reference
        .set_location(&mut *self)
        .with_durability(Durability::LOW)
        .to(location.ending(Offset(end_offset + new_name.len())));
    }

    RenamesResult {
      edits: edits.into_iter().collect(),
      definition,
    }
  }
}
