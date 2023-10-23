use std::{
  cell::Cell,
  fmt::{Display, Formatter},
};

use salsa_2022::DebugWithDb;

use crate::HirDb;

/// Defines the formatting scope, which will hold contextual information
/// about the code to format.
#[derive(Default)]
pub struct Scope {
  /// The indentation of the current scope.
  indent: Cell<usize>,
}

/// A trait for formatting HIR nodes, into strings, with the proposal of
/// debugging.
///
/// A real source code formatter should be implemented within the concrete
/// tree, and not the resolved tree.
pub trait HirFormatter {
  /// Format the node with the given scope.
  #[inline(always)]
  fn fmt(
    &self,
    db: &dyn HirDb,
    f: &mut Formatter,
    scope: &Scope,
    indent: usize,
  ) -> std::fmt::Result {
    scope.indent.set(scope.indent.take() + indent);
    self.hir_fmt(db, f, scope)?;
    scope.indent.set(scope.indent.take() - indent);
    Ok(())
  }

  fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result;

  fn formatter(&self) -> HirFormatterDebug<'_>
  where
    Self: Sized,
  {
    HirFormatterDebug(self)
  }
}

impl Scope {
  /// Indent the scope.
  pub fn indent(&self) {
    self.indent.set(self.indent.take() + 1);
  }

  /// Unindent the scope.
  pub fn unindent(&self) {
    self.indent.set(self.indent.take() - 1);
  }

  /// Write a new line with the current indentation.
  pub fn write_indent(&self, f: &mut Formatter) -> std::fmt::Result {
    for _ in 0..self.indent.get() {
      write!(f, "  ")?;
    }
    Ok(())
  }

  /// Writes punctuated items. The separator is not written at the end.
  pub fn punctuated<T: HirFormatter>(
    &self,
    db: &dyn HirDb,
    f: &mut Formatter,
    value: Vec<T>,
    sep: &str,
  ) -> std::fmt::Result {
    for (i, item) in value.iter().enumerate() {
      if i != 0 {
        write!(f, "{} ", sep)?;
      }
      item.hir_fmt(db, f, self)?;
    }

    Ok(())
  }

  /// Writes punctuated items. The separator is not written at the end.
  ///
  /// The separator is written at the end of each line.
  pub fn unlined<T: HirFormatter>(
    &self,
    db: &dyn HirDb,
    f: &mut Formatter,
    value: Vec<T>,
    sep: &str,
  ) -> std::fmt::Result {
    for (i, item) in value.iter().enumerate() {
      if i != 0 {
        self.write_indent(f)?;
        write!(f, "{} ", sep)?;
      }
      item.hir_fmt(db, f, self)?;
      writeln!(f)?;
    }

    Ok(())
  }
}

pub struct HirFormatterDebug<'a>(&'a dyn HirFormatter);

impl DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for HirFormatterDebug<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn HirDb, _: bool) -> std::fmt::Result {
    self.0.hir_fmt(db, f, &Scope::default())
  }
}

impl<T: Display> HirFormatter for T {
  /// Writes [`Display`] implementations with the
  /// [`DebugWithDb`](salsa_2022::DebugWithDb) implementation.
  fn hir_fmt(&self, _: &dyn HirDb, f: &mut Formatter, _: &Scope) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

/// A modules that holds all formatter implementations.
///
/// It's only a module to avoid polluting the root namespace.
mod impls {
  use super::*;
  use crate::{
    solver::{Definition, Reference},
    source::{declaration::Declaration, type_rep::TypeReference, *},
  };

  /// Formats a declaration using the given formatter. The declaration is
  /// followed by a semicolon.
  ///
  /// The given function is called to format the declaration.
  fn format_decl<Prefix, D, F>(
    decl: &D,
    prefix: Prefix,
    db: &dyn HirDb,
    scope: &Scope,
    f: &mut Formatter,
    format_decl: F,
  ) -> std::fmt::Result
  where
    Prefix: Into<Option<&'static str>>,
    D: Declaration,
    F: FnOnce(&dyn HirDb, &mut Formatter, &Scope) -> std::fmt::Result,
  {
    scope.write_indent(f)?;
    if let Some(prefix) = prefix.into() {
      write!(f, "{prefix}")?;
    }
    decl.name(db).hir_fmt(db, f, scope)?;
    scope.punctuated(db, f, decl.parameters(db), " ")?;
    if let Some(type_rep) = decl.type_rep(db) {
      write!(f, " : ")?;
      type_rep.hir_fmt(db, f, scope)?;
    }
    format_decl(db, f, scope)?;
    write!(f, ";")?;

    Ok(())
  }

  /// Formats a simple code block using the given formatter. The code block
  /// is surrounded by curly braces.
  ///
  /// The given function is called to format the declaration.
  fn code_block<F>(
    db: &dyn HirDb,
    scope: &Scope,
    f: &mut Formatter,
    format_decl: F,
  ) -> std::fmt::Result
  where
    F: FnOnce(&dyn HirDb, &mut Formatter, &Scope) -> std::fmt::Result,
  {
    write!(f, "{{")?;
    writeln!(f)?;
    scope.indent();
    format_decl(db, f, scope)?;
    scope.unindent();
    scope.write_indent(f)?;
    write!(f, "}}")
  }

  /// A formatter for [`Reference`].
  impl HirFormatter for Reference {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.name(db).hir_fmt(db, f, scope)
    }
  }

  /// A formatter for [`Definition`].
  impl HirFormatter for Definition {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.name(db).hir_fmt(db, f, scope)
    }
  }

  /// A formatter for [`Identifier`]s.
  impl HirFormatter for Identifier {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, _: &Scope) -> std::fmt::Result {
      if self.refers_symbol(db) {
        write!(f, "`{}", self.contents(db))
      } else {
        write!(f, "{}", self.contents(db))
      }
    }
  }

  /// A formatter for [`HirPath`].
  impl HirFormatter for HirPath {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      for (i, segment) in self.segments(db).iter().enumerate() {
        if i != 0 {
          write!(f, ".")?;
        }
        segment.hir_fmt(db, f, scope)?;
      }

      Ok(())
    }
  }

  /// A formatter for [`declaration::Attribute`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for declaration::Attribute {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      write!(f, "@")?;
      self.name(db).hir_fmt(db, f, scope)?;
      let arguments = self.arguments(db);
      if !arguments.is_empty() {
        write!(f, "(")?;
        scope.punctuated(db, f, arguments, ",")?;
        write!(f, ")")?;
      }
      writeln!(f)
    }
  }

  /// A formatter for [`declaration::DocString`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for declaration::DocString {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      write!(f, "//! ")?;
      let _ = db; // TODO
      writeln!(f)
    }
  }

  /// A formatter for [`declaration::Parameter`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for declaration::Parameter {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.binding(db).hir_fmt(db, f, scope)?;
      write!(f, " : ")?;
      self.parameter_type(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for top_level::UsingTopLevel {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      write!(f, "using ")?;
      self.path(db).hir_fmt(db, f, scope)?;
      writeln!(f)
    }
  }

  impl HirFormatter for top_level::CommandTopLevel {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      write!(f, "#")?;
      self.path(db).hir_fmt(db, f, scope)?;
      write!(f, " ")?;
      scope.punctuated(db, f, self.arguments(db), ", ")?;
      writeln!(f)
    }
  }

  impl HirFormatter for top_level::Signature {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      format_decl(self, "val", db, scope, f, |_, _, _| {
        // Nothing to do
        Ok(())
      })
    }
  }

  impl HirFormatter for top_level::Clause {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      self.name(db).hir_fmt(db, f, scope)?;
      scope.punctuated(db, f, self.arguments(db), " ")?;
      write!(f, " = ")?;
      self.value(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for top_level::BindingGroup {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.signature(db).hir_fmt(db, f, scope)?;
      for clause in self.clauses(db) {
        clause.hir_fmt(db, f, scope)?;
      }
      Ok(())
    }
  }

  impl HirFormatter for top_level::ClassDecl {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      format_decl(self, "class", db, scope, f, |db, f, scope| {
        // Write the classes' methods wi
        code_block(db, scope, f, |db, f, scope| {
          scope.unlined(db, f, self.methods(db), ";")
        })
      })
    }
  }

  impl HirFormatter for top_level::InstanceDecl {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      // TODO: Print implementation's type
      format_decl(self, "instance", db, scope, f, |db, f, scope| {
        // Write the classes' methods wi
        code_block(db, scope, f, |db, f, scope| {
          scope.unlined(db, f, self.methods(db), ";")
        })
      })
    }
  }

  impl HirFormatter for top_level::TraitDecl {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      format_decl(self, "trait", db, scope, f, |db, f, scope| {
        // Write the classes' methods wi
        code_block(db, scope, f, |db, f, scope| {
          scope.unlined(db, f, self.methods(db), ";")
        })
      })
    }
  }

  impl HirFormatter for top_level::Constructor {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      scope.write_indent(f)?;
      self.name(db).hir_fmt(db, f, scope)?;
      write!(f, " : ")?;
      self.return_type(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for top_level::DataDecl {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      format_decl(self, "data", db, scope, f, |db, f, scope| {
        // Write the classes' methods wi
        code_block(db, scope, f, |db, f, scope| {
          scope.unlined(db, f, self.variants(db), ";")?;
          scope.unlined(db, f, self.methods(db), ";")
        })
      })
    }
  }

  impl HirFormatter for top_level::TypeDecl {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      format_decl(self, "type", db, scope, f, |_, _, _| {
        // Nothing to do as [`format_decl`] does everything
        // for us
        Ok(())
      })
    }
  }

  /// A formatter for [`top_level::TopLevel`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for top_level::TopLevel {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      use top_level::TopLevel::*;

      match self {
        Error(_) => write!(f, "!"),
        Using(using_top_level) => using_top_level.hir_fmt(db, f, scope),
        Command(command_top_level) => command_top_level.hir_fmt(db, f, scope),
        BindingGroup(binding_group) => binding_group.hir_fmt(db, f, scope),
        ClassDecl(class_decl) => class_decl.hir_fmt(db, f, scope),
        InstanceDecl(instance_decl) => instance_decl.hir_fmt(db, f, scope),
        TraitDecl(trait_decl) => trait_decl.hir_fmt(db, f, scope),
        DataDecl(decl_decl) => decl_decl.hir_fmt(db, f, scope),
        TypeDecl(type_decl) => type_decl.hir_fmt(db, f, scope),
      }
    }
  }

  /// A formatter for [`stmt::Block`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for stmt::Block {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      code_block(db, scope, f, |db, f, scope| {
        for statement in self.statements(db) {
          statement.hir_fmt(db, f, scope)?;
        }
        Ok(())
      })
    }
  }

  impl HirFormatter for stmt::LetStmt {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.pattern(db).hir_fmt(db, f, scope)?;
      write!(f, " = ")?;
      self.value(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for stmt::AskStmt {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      write!(f, "ask ")?;
      self.pattern(db).hir_fmt(db, f, scope)?;
      write!(f, " <- ")?;
      self.value(db).hir_fmt(db, f, scope)
    }
  }

  /// A formatter for [`stmt::Stmt`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for stmt::Stmt {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      use stmt::Stmt::*;

      // Writes indent and then the statement
      // semicolon/newline.
      scope.write_indent(f)?;
      let stmt_result = match self {
        Empty => write!(f, "_"),
        Error(_) => write!(f, "!"),
        Ask(ask_stmt) => ask_stmt.hir_fmt(db, f, scope),
        Let(let_stmt) => let_stmt.hir_fmt(db, f, scope),
        Downgrade(expr) => expr.hir_fmt(db, f, scope),
      };
      writeln!(f, ";")?;

      stmt_result
    }
  }

  impl HirFormatter for pattern::Constructor {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      match self {
        pattern::Constructor::Array => write!(f, "[]"),
        pattern::Constructor::Tuple => write!(f, "(,)"),
        pattern::Constructor::Unit => write!(f, "()"),
        pattern::Constructor::Path(path) => path.hir_fmt(db, f, scope),
      }
    }
  }

  impl HirFormatter for pattern::ConstructorPattern {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      let arguments = self.arguments(db);
      if arguments.is_empty() {
        self.name(db).hir_fmt(db, f, scope)?;
        write!(f, " ")?;
        scope.punctuated(db, f, arguments, ", ")
      } else {
        write!(f, "(")?;
        self.name(db).hir_fmt(db, f, scope)?;
        write!(f, " ")?;
        scope.punctuated(db, f, arguments, ", ")?;
        write!(f, ")")
      }
    }
  }
  impl HirFormatter for pattern::BindingPattern {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.name(db).hir_fmt(db, f, scope)
    }
  }

  /// A formatter for [`pattern::Pattern`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for pattern::Pattern {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      use pattern::Pattern::*;

      match self {
        Hole => write!(f, "_"),
        Wildcard(_) => write!(f, "_"),
        Rest(_) => write!(f, ".."),
        Error(_) => write!(f, "!"),
        Literal(literal) => literal.value.hir_fmt(db, f, scope),
        Constructor(pattern) => pattern.hir_fmt(db, f, scope),
        Binding(binding) => binding.hir_fmt(db, f, scope),
      }
    }
  }

  impl HirFormatter for type_rep::QPath {
    fn hir_fmt(&self, _: &dyn HirDb, _: &mut Formatter, _: &Scope) -> std::fmt::Result {
      todo!("QPath is not implemented")
    }
  }

  impl HirFormatter for type_rep::AppTypeRep {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      let arguments = self.arguments(db);
      if arguments.is_empty() {
        self.callee(db).hir_fmt(db, f, scope)
      } else {
        write!(f, "(")?;
        self.callee(db).hir_fmt(db, f, scope)?;
        write!(f, " ")?;
        scope.punctuated(db, f, arguments, ", ")?;
        write!(f, ")")
      }
    }
  }

  impl HirFormatter for type_rep::ArrowTypeRep {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      for parameter in self.parameters(db) {
        parameter.hir_fmt(db, f, scope)?;
        write!(f, " -> ")?;
      }
      self.value(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for type_rep::TypeReference {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      match self {
        TypeReference::Reference(path) => path.hir_fmt(db, f, scope),
        _ => write!(f, "{:?}", self),
      }
    }
  }

  /// A formatter for [`type_rep::TypeRep`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for type_rep::TypeRep {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      use type_rep::TypeRep::*;

      match self {
        Unit => write!(f, "()"),
        Hole => write!(f, "_"),
        SelfType => write!(f, "Self"),
        Type => write!(f, "*"),
        Error(_) => write!(f, "!"),
        Path(path, _) => path.hir_fmt(db, f, scope),
        QPath(qpath) => qpath.hir_fmt(db, f, scope),
        App(app) => app.hir_fmt(db, f, scope),
        Arrow(arrow) => arrow.hir_fmt(db, f, scope),
        Downgrade(expr) => expr.hir_fmt(db, f, scope),
      }
    }
  }

  /// A formatter for [`literal::Literal`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for literal::Literal {
    fn hir_fmt(&self, _: &dyn HirDb, f: &mut Formatter, _: &Scope) -> std::fmt::Result {
      use literal::Literal::*;

      match self {
        Empty => write!(f, "_"),
        Int8(value) => write!(f, "{value}"),
        UInt8(value) => write!(f, "{value}"),
        Int16(value) => write!(f, "{value}"),
        UInt16(value) => write!(f, "{value}"),
        Int32(value) => write!(f, "{value}"),
        UInt32(value) => write!(f, "{value}"),
        Int64(value) => write!(f, "{value}"),
        UInt64(value) => write!(f, "{value}"),
        String(value) => write!(f, "\"{value}\""),
        Boolean(value) => write!(f, "{value}"),
        Char(value) => write!(f, "'{value}'"),
      }
    }
  }

  impl HirFormatter for expr::Callee {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      match self {
        expr::Callee::Array => write!(f, "[]"),
        expr::Callee::Tuple => write!(f, "(,)"),
        expr::Callee::Unit => write!(f, "()"),
        expr::Callee::Pure => write!(f, "pure"),
        expr::Callee::Do => write!(f, "do"),
        expr::Callee::Reference(path) => path.hir_fmt(db, f, scope),
        expr::Callee::Expr(expr) => expr.hir_fmt(db, f, scope),
      }
    }
  }

  impl HirFormatter for expr::CallExpr {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      write!(f, "(")?;
      self.callee(db).hir_fmt(db, f, scope)?;
      let arguments = self.arguments(db);
      if !arguments.is_empty() {
        write!(f, " ")?;
        scope.punctuated(db, f, arguments, " ")?;
      }
      if let Some(block) = self.do_notation(db) {
        write!(f, " ")?;
        block.hir_fmt(db, f, scope)?;
      }
      write!(f, ")")?;

      Ok(())
    }
  }

  impl HirFormatter for expr::AbsExpr {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      write!(f, "λ")?;
      write!(f, " ")?;
      scope.punctuated(db, f, self.parameters(db), ", ")?;
      write!(f, ".")?;
      write!(f, " ")?;
      self.value(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for expr::AnnExpr {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.value(db).hir_fmt(db, f, scope)?;
      write!(f, " : ")?;
      self.type_rep(db).hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for expr::MatchArm {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      self.pattern.hir_fmt(db, f, scope)?;
      write!(f, " => ")?;
      self.value.hir_fmt(db, f, scope)
    }
  }

  impl HirFormatter for expr::MatchExpr {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      write!(f, "match")?;
      write!(f, " ")?;
      self.scrutinee(db).hir_fmt(db, f, scope)?;
      write!(f, " ")?;
      write!(f, "{{")?;
      code_block(db, scope, f, |db, f, scope| {
        scope.unlined(db, f, self.clauses(db), ", ")
      })?;
      write!(f, "}}")
    }
  }

  /// A formatter for [`expr::Expr`]. It does
  /// takes an attribute and format it as it would be written
  /// in a source file.
  impl HirFormatter for expr::Expr {
    fn hir_fmt(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
      use expr::Expr::*;

      match self {
        Empty => write!(f, "_"),
        Error(_) => write!(f, "!"),
        Meta(_) => write!(f, "!"),
        Path(path) => path.hir_fmt(db, f, scope),
        Literal(literal) => literal.value.hir_fmt(db, f, scope),
        Call(call_expr) => call_expr.hir_fmt(db, f, scope),
        Ann(ann_expr) => ann_expr.hir_fmt(db, f, scope),
        Abs(abs_expr) => abs_expr.hir_fmt(db, f, scope),
        Match(match_expr) => match_expr.hir_fmt(db, f, scope),
        Upgrade(type_rep) => type_rep.hir_fmt(db, f, scope),
      }
    }
  }
}
