//! Defines a module for resolving language "patterns", that matches agains't values and another
//! things. It will be used in the resolution, and it's a helper module for the [`LowerHir`] struct.
//!
//! It's only a module, to organization purposes.

use lura_hir::source::pattern::{BindingPattern, Constructor, ConstructorPattern, Pattern};

use super::*;

#[rustfmt::skip]
type SyntaxPattern<'tree> = lura_syntax::anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>;

impl HirLowering<'_, '_> {
  pub fn trait_pattern(&mut self, tree: SyntaxPattern) -> Pattern {
    use lura_syntax::anon_unions::ConsPattern_GroupPattern_Literal_RestPattern::*;

    let location = self.range(tree.range());

    match tree {
      // Defines a custom solver for the constructor pattern,
      // because it needs to search for the trait constructor in the scope.
      ConsPattern(pattern) => {
        let name = pattern.name().solve(self, |this, node| this.path(node));
        let patterns = self.trait_patterns(pattern.patterns(&mut pattern.walk()));
        let location = self.range(pattern.range());

        // If the patterns are empty, it's a binding pattern, otherwise, it's a constructor
        // pattern.
        if patterns.is_empty() {
          // Defines the node on the scope
          let name = self
            .scope
            .define(self.db, name, location.clone(), DefinitionKind::Type);

          Pattern::Binding(BindingPattern::new(self.db, name, location))
        } else {
          let def = self.qualify(name, DefinitionKind::Trait);
          let reference = self.scope.using(self.db, def, name.location(self.db));
          let name = Constructor::Path(reference);

          Pattern::Constructor(ConstructorPattern::new(self.db, name, patterns, location))
        }
      }
      GroupPattern(group_pattern) => {
        // There's no AST for groups, so we need to solve it directly
        group_pattern
          .pattern()
          .solve(self, |this, node| this.trait_pattern(node))
      }
      Literal(literal) => self.literal(literal).upgrade_pattern(location, self.db),
      RestPattern(_) => Pattern::Rest(location),
    }
  }

  pub fn pattern(&mut self, tree: SyntaxPattern) -> Pattern {
    use lura_syntax::anon_unions::ConsPattern_GroupPattern_Literal_RestPattern::*;

    let location = self.range(tree.range());

    match tree {
      ConsPattern(cons_pattern) => self.cons_pattern(cons_pattern),
      GroupPattern(group_pattern) => self.group_pattern(group_pattern),
      Literal(literal) => self.literal(literal).upgrade_pattern(location, self.db),
      RestPattern(_) => Pattern::Rest(location),
    }
  }

  pub fn group_pattern(&mut self, group: lura_syntax::GroupPattern) -> Pattern {
    // There's no AST for groups, so we need to solve it directly
    return group.pattern().solve(self, |this, node| this.pattern(node));
  }

  pub fn cons_pattern(&mut self, pattern: lura_syntax::ConsPattern) -> Pattern {
    let name = pattern.name().solve(self, |this, node| this.path(node));
    let patterns = self.patterns(pattern.patterns(&mut pattern.walk()));
    let location = self.range(pattern.range());

    // If the patterns are empty, it's a binding pattern, otherwise, it's a constructor
    // pattern.
    if patterns.is_empty() {
      // Defines the node on the scope
      let name = self
        .scope
        .define(self.db, name, location.clone(), DefinitionKind::Variable);

      Pattern::Binding(BindingPattern::new(self.db, name, location))
    } else {
      let def = self.qualify(name, DefinitionKind::Constructor);
      let reference = self.scope.using(self.db, def, name.location(self.db));
      let name = Constructor::Path(reference);

      Pattern::Constructor(ConstructorPattern::new(self.db, name, patterns, location))
    }
  }

  pub fn trait_patterns<'a, I>(&mut self, patterns: I) -> Vec<Pattern>
  where
    I: Iterator<Item = NodeResult<'a, ExtraOr<'a, SyntaxPattern<'a>>>>,
  {
    patterns
      .flatten()
      .filter_map(|pattern| pattern.regular())
      .map(|pattern| self.trait_pattern(pattern))
      .collect()
  }

  pub fn patterns<'a, I>(&mut self, patterns: I) -> Vec<Pattern>
  where
    I: Iterator<Item = NodeResult<'a, ExtraOr<'a, SyntaxPattern<'a>>>>,
  {
    patterns
      .flatten()
      .filter_map(|pattern| pattern.regular())
      .map(|pattern| self.pattern(pattern))
      .collect()
  }
}
