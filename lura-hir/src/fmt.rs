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
        self.write(db, f, scope)?;
        scope.indent.set(scope.indent.take() - indent);
        Ok(())
    }

    fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result;

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

    /// Write a new line.
    pub fn new_line(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f)
    }

    /// Write a new line with the current indentation.
    pub fn write_with_indent(&self, f: &mut Formatter, string: &str) -> std::fmt::Result {
        for _ in 0..self.indent.get() {
            write!(f, "  ")?;
        }
        write!(f, "{}", string)?;
        Ok(())
    }

    /// Write a new string.
    pub fn write(&self, f: &mut Formatter, string: &str) -> std::fmt::Result {
        write!(f, "{}", string)?;
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
            item.write(db, f, self)?;
        }

        Ok(())
    }

    /// Writes punctuated items. The separator is not written at the end.
    ///
    /// The separator is written at the end of each line.
    pub fn line_punctuated<T: HirFormatter>(
        &self,
        db: &dyn HirDb,
        f: &mut Formatter,
        value: Vec<T>,
        sep: &str,
    ) -> std::fmt::Result {
        for (i, item) in value.iter().enumerate() {
            if i != 0 {
                self.write_with_indent(f, &format!("{} ", sep))?;
            }
            item.write(db, f, self)?;
            self.new_line(f)?;
        }

        Ok(())
    }
}

pub struct HirFormatterDebug<'a>(&'a dyn HirFormatter);

impl DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for HirFormatterDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn HirDb, _: bool) -> std::fmt::Result {
        self.0.write(db, f, &Scope::default())
    }
}

impl<T: Display> HirFormatter for T {
    /// Writes [`Display`] implementations with the
    /// [`DebugWithDb`](salsa_2022::DebugWithDb) implementation.
    fn write(&self, _: &dyn HirDb, f: &mut Formatter, _: &Scope) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

/// A modules that holds all formatter implementations.
///
/// It's only a module to avoid polluting the root namespace.
mod impls {
    use super::*;

    use crate::{
        resolve::{Definition, Reference},
        source::*,
    };

    /// A formatter for [`Reference`].
    impl HirFormatter for Reference {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.name(db).write(db, f, scope)
        }
    }

    /// A formatter for [`Definition`].
    impl HirFormatter for Definition {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.name(db).write(db, f, scope)
        }
    }

    /// A formatter for [`Identifier`]s.
    impl HirFormatter for Identifier {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, _: &Scope) -> std::fmt::Result {
            if self.refers_symbol(db) {
                write!(f, "`{}", self.contents(db))
            } else {
                write!(f, "{}", self.contents(db))
            }
        }
    }

    /// A formatter for [`HirPath`].
    impl HirFormatter for HirPath {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            for (i, segment) in self.segments(db).iter().enumerate() {
                if i != 0 {
                    write!(f, ".")?;
                }
                segment.write(db, f, scope)?;
            }

            Ok(())
        }
    }

    /// A formatter for [`declaration::Attribute`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for declaration::Attribute {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "@")?;
            self.name(db).write(db, f, scope)?;

            let arguments = self.arguments(db);
            if !arguments.is_empty() {
                scope.write(f, "(")?;
                scope.punctuated(db, f, arguments, ",")?;
                scope.write(f, ")")?;
            }

            scope.new_line(f)
        }
    }

    /// A formatter for [`declaration::DocString`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for declaration::DocString {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "//!")?;
            let _ = db; // TODO
            scope.new_line(f)
        }
    }

    /// A formatter for [`declaration::Parameter`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for declaration::Parameter {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.binding(db).write(db, f, scope)?;
            scope.write(f, " : ")?;
            self.parameter_type(db).write(db, f, scope)
        }
    }

    impl HirFormatter for top_level::UsingTopLevel {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "using ")?;
            self.path(db).write(db, f, scope)?;
            scope.new_line(f)
        }
    }

    impl HirFormatter for top_level::CommandTopLevel {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "#")?;
            self.path(db).write(db, f, scope)?;
            scope.write(f, " ")?;
            scope.punctuated(db, f, self.arguments(db), ", ")?;
            scope.new_line(f)
        }
    }

    impl HirFormatter for top_level::Signature {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "val")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.parameters(db), " ")?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)
        }
    }

    impl HirFormatter for top_level::Clause {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.arguments(db), " ")?;
            scope.write(f, " = ")?;
            self.value(db).write(db, f, scope)
        }
    }

    impl HirFormatter for top_level::BindingGroup {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.signature(db).write(db, f, scope)?;
            scope.new_line(f)?;
            for clause in self.clauses(db) {
                clause.write(db, f, scope)?;
                scope.new_line(f)?;
            }
            Ok(())
        }
    }

    impl HirFormatter for top_level::ClassDecl {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "class ")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.parameters(db), " ")?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)?;
            scope.write(f, " where")?;
            scope.new_line(f)?;
            scope.indent();
            scope.write_with_indent(f, "")?;
            scope.line_punctuated(db, f, self.methods(db), ";")?;
            scope.unindent();
            scope.new_line(f)
        }
    }

    impl HirFormatter for top_level::TraitDecl {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "trait ")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.parameters(db), " ")?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)?;
            scope.write(f, " where")?;
            scope.new_line(f)?;
            scope.indent();
            scope.write_with_indent(f, "")?;
            scope.line_punctuated(db, f, self.methods(db), ";")?;
            scope.unindent();
            scope.new_line(f)
        }
    }

    impl HirFormatter for top_level::Constructor {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "| ")?;
            self.name(db).write(db, f, scope)?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)
        }
    }

    impl HirFormatter for top_level::DataDecl {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "data ")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.parameters(db), " ")?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)?;
            scope.write(f, " where")?;
            scope.new_line(f)?;
            scope.indent();
            scope.write_with_indent(f, "")?;
            scope.line_punctuated(db, f, self.variants(db), ";")?;
            scope.line_punctuated(db, f, self.methods(db), ";")?;
            scope.unindent();
            scope.new_line(f)
        }
    }

    impl HirFormatter for top_level::TypeDecl {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "type ")?;
            self.name(db).write(db, f, scope)?;
            scope.punctuated(db, f, self.parameters(db), " ")?;
            scope.write(f, " : ")?;
            self.return_type(db).write(db, f, scope)
        }
    }

    /// A formatter for [`top_level::TopLevel`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for top_level::TopLevel {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use top_level::TopLevel::*;

            match self {
                Empty => scope.write(f, "_"),
                Error(_) => scope.write(f, "!"),
                Using(using_top_level) => using_top_level.write(db, f, scope),
                Command(command_top_level) => command_top_level.write(db, f, scope),
                BindingGroup(binding_group) => binding_group.write(db, f, scope),
                ClassDecl(class_decl) => class_decl.write(db, f, scope),
                TraitDecl(trait_decl) => trait_decl.write(db, f, scope),
                DataDecl(decl_decl) => decl_decl.write(db, f, scope),
                TypeDecl(type_decl) => type_decl.write(db, f, scope),
            }
        }
    }

    /// A formatter for [`stmt::Block`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for stmt::Block {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "{")?;
            scope.new_line(f)?;
            scope.indent();
            scope.write_with_indent(f, "")?;
            scope.line_punctuated(db, f, self.statements(db), ";")?;
            scope.unindent();
            scope.write_with_indent(f, "}")
        }
    }

    impl HirFormatter for stmt::LetStmt {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "let ")?;
            self.pattern(db).write(db, f, scope)?;
            scope.write(f, " = ")?;
            self.value(db).write(db, f, scope)?;
            scope.new_line(f)
        }
    }

    impl HirFormatter for stmt::AskStmt {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write_with_indent(f, "ask ")?;
            self.pattern(db).write(db, f, scope)?;
            scope.write(f, " <- ")?;
            self.value(db).write(db, f, scope)?;
            scope.new_line(f)
        }
    }

    /// A formatter for [`stmt::Stmt`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for stmt::Stmt {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use stmt::Stmt::*;

            match self {
                Empty => scope.write(f, "_"),
                Error(_) => todo!(),
                Ask(ask_stmt) => ask_stmt.write(db, f, scope),
                Let(let_stmt) => let_stmt.write(db, f, scope),
                Downgrade(expr) => expr.write(db, f, scope),
            }
        }
    }

    impl HirFormatter for pattern::Constructor {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            match self {
                pattern::Constructor::Array => scope.write(f, "[]"),
                pattern::Constructor::Tuple => scope.write(f, "(,)"),
                pattern::Constructor::Unit => scope.write(f, "()"),
                pattern::Constructor::Path(path) => path.write(db, f, scope),
            }
        }
    }

    impl HirFormatter for pattern::ConstructorPattern {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            let arguments = self.arguments(db);
            if arguments.is_empty() {
                self.name(db).write(db, f, scope)?;
                scope.write(f, " ")?;
                scope.punctuated(db, f, arguments, ", ")
            } else {
                scope.write(f, "(")?;
                self.name(db).write(db, f, scope)?;
                scope.write(f, " ")?;
                scope.punctuated(db, f, arguments, ", ")?;
                scope.write(f, ")")
            }
        }
    }
    impl HirFormatter for pattern::BindingPattern {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.name(db).write(db, f, scope)
        }
    }

    /// A formatter for [`pattern::Pattern`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for pattern::Pattern {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use pattern::Pattern::*;

            match self {
                Empty => scope.write(f, "_"),
                Literal(literal) => literal.value.write(db, f, scope),
                Wildcard(_) => scope.write(f, "_"),
                Rest(_) => scope.write(f, ".."),
                Error(_) => scope.write(f, "!"),
                Constructor(pattern) => pattern.write(db, f, scope),
                Binding(binding) => binding.write(db, f, scope),
            }
        }
    }

    impl HirFormatter for type_rep::QPath {
        fn write(&self, _: &dyn HirDb, _: &mut Formatter, _: &Scope) -> std::fmt::Result {
            todo!("QPath is not implemented")
        }
    }

    impl HirFormatter for type_rep::AppTypeRep {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            let arguments = self.arguments(db);
            if arguments.is_empty() {
                self.callee(db).write(db, f, scope)
            } else {
                scope.write(f, "(")?;
                self.callee(db).write(db, f, scope)?;
                scope.write(f, " ")?;
                scope.punctuated(db, f, arguments, ", ")?;
                scope.write(f, ")")
            }
        }
    }

    impl HirFormatter for type_rep::ArrowTypeRep {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            for parameter in self.parameters(db) {
                parameter.write(db, f, scope)?;
                scope.write(f, " -> ")?;
            }
            self.value(db).write(db, f, scope)
        }
    }

    /// A formatter for [`type_rep::TypeRep`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for type_rep::TypeRep {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use type_rep::TypeRep::*;

            match self {
                Unit => scope.write(f, "()"),
                Empty => scope.write(f, "_"),
                SelfType => scope.write(f, "Self"),
                Type => scope.write(f, "*"),
                Error(_) => scope.write(f, "!"),
                Path(path) => path.write(db, f, scope),
                QPath(qpath) => qpath.write(db, f, scope),
                App(app) => app.write(db, f, scope),
                Arrow(arrow) => arrow.write(db, f, scope),
                Downgrade(expr) => expr.write(db, f, scope),
            }
        }
    }

    /// A formatter for [`literal::Literal`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for literal::Literal {
        fn write(&self, _: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use literal::Literal::*;

            match self {
                Empty => scope.write(f, "_"),
                Int8(value) => scope.write(f, &value.to_string()),
                UInt8(value) => scope.write(f, &value.to_string()),
                Int16(value) => scope.write(f, &value.to_string()),
                UInt16(value) => scope.write(f, &value.to_string()),
                Int32(value) => scope.write(f, &value.to_string()),
                UInt32(value) => scope.write(f, &value.to_string()),
                Int64(value) => scope.write(f, &value.to_string()),
                UInt64(value) => scope.write(f, &value.to_string()),
                String(value) => scope.write(f, &value.to_string()),
                Boolean(value) => scope.write(f, &value.to_string()),
                Char(value) => scope.write(f, &value.to_string()),
            }
        }
    }

    impl HirFormatter for expr::Callee {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            match self {
                expr::Callee::Array => scope.write(f, "[]"),
                expr::Callee::Tuple => scope.write(f, "(,)"),
                expr::Callee::Unit => scope.write(f, "()"),
                expr::Callee::Pure => scope.write(f, "pure"),
                expr::Callee::Do => scope.write(f, "do"),
                expr::Callee::Reference(path) => path.write(db, f, scope),
                expr::Callee::Expr(expr) => expr.write(db, f, scope),
            }
        }
    }

    impl HirFormatter for expr::CallExpr {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "(")?;
            self.callee(db).write(db, f, scope)?;
            scope.write(f, " ")?;
            scope.punctuated(db, f, self.arguments(db), " ")?;
            if let Some(block) = self.do_notation(db) {
                scope.write(f, " ")?;
                block.write(db, f, scope)?;
            }
            scope.write(f, ")")?;

            Ok(())
        }
    }

    impl HirFormatter for expr::AbsExpr {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "λ ")?;
            scope.punctuated(db, f, self.parameters(db), ", ")?;
            scope.write(f, ". ")?;
            self.value(db).write(db, f, scope)
        }
    }

    impl HirFormatter for expr::AnnExpr {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.value(db).write(db, f, scope)?;
            scope.write(f, " : ")?;
            self.type_rep(db).write(db, f, scope)
        }
    }

    impl HirFormatter for expr::MatchArm {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            self.pattern.write(db, f, scope)?;
            scope.write(f, " => ")?;
            self.value.write(db, f, scope)
        }
    }

    impl HirFormatter for expr::MatchExpr {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            scope.write(f, "match ")?;
            self.scrutinee(db).write(db, f, scope)?;
            scope.write(f, " {")?;
            scope.new_line(f)?;
            scope.indent();
            scope.write_with_indent(f, "")?;
            scope.line_punctuated(db, f, self.clauses(db), ", ")?;
            scope.unindent();
            scope.write_with_indent(f, "}")
        }
    }

    /// A formatter for [`expr::Expr`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for expr::Expr {
        fn write(&self, db: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use expr::Expr::*;

            match self {
                Empty => scope.write(f, "_"),
                Error(_) => scope.write(f, "!"),
                Path(path) => path.write(db, f, scope),
                Literal(literal) => literal.value.write(db, f, scope),
                Call(call_expr) => call_expr.write(db, f, scope),
                Ann(ann_expr) => ann_expr.write(db, f, scope),
                Abs(abs_expr) => abs_expr.write(db, f, scope),
                Match(match_expr) => match_expr.write(db, f, scope),
                Upgrade(type_rep) => type_rep.write(db, f, scope),
            }
        }
    }
}
