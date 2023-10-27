//! Defines a module for resolving language "statements", that are the statements, like `let`,
//! `ask`, and other things. It will be used in the resolution, and it's a helper module for the
//! [`LowerHir`] struct.
//!
//! It's only a module, to organization purposes.

use lura_hir::{
  solver::HirLevel,
  source::{
    expr::{MatchArm, MatchExpr, MatchKind},
    literal::Literal,
    pattern::Pattern,
    stmt::{AskStmt, Block, LetStmt, Stmt},
    HirElement,
  },
};

use super::*;

type SyntaxStmt<'tree> = lura_syntax::anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt<'tree>;

impl HirLowering<'_, '_> {
  /// Resolves an statement.
  ///
  /// It does resolves a syntax statement,
  /// into a high-level statement.
  pub fn stmt(&mut self, stmt: SyntaxStmt, level: HirLevel) -> Stmt {
    use lura_syntax::anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt::*;

    match stmt {
      AskStmt(ask_stmt) => self.ask_stmt(ask_stmt, level),
      ExprStmt(expr_stmt) => self.expr_stmt(expr_stmt, level),
      IfStmt(if_stmt) => self.if_stmt(if_stmt, level),
      LetStmt(let_stmt) => self.let_stmt(let_stmt, level),
    }
  }

  /// Resolves an ask statement.
  ///
  /// It does resolves a syntax ask statement,
  /// into a high-level statement.
  pub fn ask_stmt(&mut self, stmt: lura_syntax::AskStmt, level: HirLevel) -> Stmt {
    let pattern = stmt.pattern().solve(self, |this, node| this.pattern(node));
    let expr = stmt
      .value()
      .solve(self, |this, node| this.expr(node, level));

    let location = self.range(stmt.range());

    Stmt::Ask(AskStmt::new(self.db, pattern, expr, location))
  }

  /// Resolves an expression statement.
  ///
  /// It does resolves a syntax expression statement,
  /// into a high-level statement.
  pub fn expr_stmt(&mut self, stmt: lura_syntax::ExprStmt, level: HirLevel) -> Stmt {
    let expr = stmt
      .child()
      .solve(self, |this, node| this.expr(node, level));

    Stmt::Downgrade(expr)
  }

  /// Resolves an if statement.
  ///
  /// It does resolves a syntax if statement,
  /// into a high-level statement.
  pub fn if_stmt(&mut self, stmt: lura_syntax::IfStmt, level: HirLevel) -> Stmt {
    let scrutinee = stmt
      .condition()
      .solve(self, |this, node| this.expr(node, level));

    let then = stmt.then().solve(self, |this, node| {
      use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

      node.child().solve(this, |this, node| match node {
        Block(block) => Expr::block(this.db, this.block(block, level)),
        _ => this.expr(node.into_node().try_into().unwrap(), level),
      })
    });

    let otherwise = stmt.otherwise().map(|then| {
      then.solve(self, |this, node| {
        use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

        node.value().solve(this, |this, node| match node {
          Block(block) => Expr::block(this.db, this.block(block, level)),
          _ => this.expr(node.into_node().try_into().unwrap(), level),
        })
      })
    })
    .unwrap_or_else(|| Expr::call_unit_expr(Location::CallSite, self.db));

    let clauses = vec![
      MatchArm {
        pattern: Pattern::Literal(Spanned::on_call_site(Literal::TRUE)),
        location: then.location(self.db),
        value: then,
      },
      MatchArm {
        pattern: Pattern::Literal(Spanned::on_call_site(Literal::FALSE)),
        location: otherwise.location(self.db),
        value: otherwise,
      },
    ];

    let location = self.range(stmt.range());

    Stmt::Downgrade(Expr::Match(MatchExpr::new(
      self.db,
      /* kind      = */ MatchKind::StmtLevel(Box::new(MatchKind::If)),
      /* scrutinee = */ scrutinee,
      /* clauses   = */ clauses,
      /* location  = */ location,
    )))
  }

  /// Resolves a let statement.
  ///
  /// It's a statement that binds a pattern to a value.
  pub fn let_stmt(&mut self, stmt: lura_syntax::LetStmt, level: HirLevel) -> Stmt {
    let pattern = stmt.pattern().solve(self, |this, node| this.pattern(node));
    let expr = stmt
      .value()
      .solve(self, |this, node| this.expr(node, level));

    let location = self.range(stmt.range());

    Stmt::Let(LetStmt::new(self.db, pattern, expr, location))
  }

  /// Creates a new block using the current supplied scope.
  ///
  /// It's useful when you already have a well founded scope,
  /// and you want to create a block.
  pub fn scoped(&mut self, block: lura_syntax::Block, level: HirLevel) -> Block {
    let stmts = block
      .statements(&mut block.walk())
      .flatten()
      .filter_map(|stmt| stmt.regular())
      .map(|stmt| self.stmt(stmt, level))
      .collect();

    Block::new(
      self.db,
      /* statements = */ stmts,
      /* location   = */ self.range(block.range()),
      /* scope      = */ self.scope.clone().into(),
    )
  }

  /// Creates a new scope, and returns the value of the block.
  ///
  /// It does fork a new scope, and then pop it,
  /// so it's not necessary to call [`pop_scope`].
  pub fn block(&mut self, block: lura_syntax::Block, level: HirLevel) -> Block {
    self.scope = self.scope.fork(ScopeKind::Block);
    let value = self.scoped(block, level);
    self.pop_scope();
    value
  }
}
