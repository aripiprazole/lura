use std::sync::Arc;

use dashmap::DashSet;
use lura_diagnostic::{Offset, TextRange};

use crate::{
  reference::ReferenceWalker,
  solver::{Definition, DefinitionKind},
  source::{
    declaration::{DocString, Vis},
    HirSource, Location,
  },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirCompletion {
  pub definition: Definition,
  pub name: String,
  pub kind: DefinitionKind,
  pub location: Location,
  pub docs: Option<Vec<DocString>>,
  pub visibility: Vis,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position {
  pub offset: Offset,
}

/// Defines the [`completion`] query.
///
/// Takes the path to the cursor. It's used to search the name completions, and the
/// location of the cursor. It's used to search for the scope completions.
#[salsa::tracked]
pub fn completions(
  db: &dyn crate::HirDb,
  file: HirSource,
  path: String,
  position: Position,
) -> Vec<HirCompletion> {
  // TODO: filter by path
  let _ = path;

  let completions = Arc::new(DashSet::new());
  let result = completions.clone();

  ReferenceWalker::<()>::empty()
    .enter_scope(move |db, location, scope| {
      let is_in_start_bounds = location.start() < position.offset;
      let is_in_end_bounds = location.end() > position.offset;
      if is_in_start_bounds && is_in_end_bounds {
        scope
          .all_definitions()
          .values()
          .map(|definition| HirCompletion {
            definition: *definition,
            name: definition.name(db).to_string(db).unwrap_or_default(),
            kind: definition.kind(db),
            location: location.clone(),
            docs: None,
            visibility: Vis::Public, // TODO: Get from db
          })
          .for_each(|completion| {
            completions.insert(completion);
          });
      }
    })
    .build(db)
    .collect(file);

  result.iter().map(|completion| completion.clone()).collect()
}
