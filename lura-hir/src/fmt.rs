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

impl DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for dyn HirFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn HirDb, _: bool) -> std::fmt::Result {
        self.write(db, f, &Scope::default())
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

    /// A formatter for [`top_level::TopLevel`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for top_level::TopLevel {
        fn write(&self, _: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use top_level::TopLevel::*;

            match self {
                Empty => scope.write(f, "_"),
                Error(_) => scope.write(f, "!"),
                Using(_) => todo!(),
                Command(_) => todo!(),
                BindingGroup(_) => todo!(),
                ClassDecl(_) => todo!(),
                TraitDecl(_) => todo!(),
                DataDecl(_) => todo!(),
                TypeDecl(_) => todo!(),
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
            scope.line_punctuated(db, f, self.statements(db), ";")?;
            scope.unindent();
            scope.write_with_indent(f, "}")
        }
    }

    /// A formatter for [`stmt::Stmt`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for stmt::Stmt {
        fn write(&self, _: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use stmt::Stmt::*;

            match self {
                Empty => scope.write(f, "_"),
                Error(_) => todo!(),
                Ask(_) => todo!(),
                Let(_) => todo!(),
                Downgrade(_) => todo!(),
            }
        }
    }

    /// A formatter for [`pattern::Pattern`]. It does
    /// takes an attribute and format it as it would be written
    /// in a source file.
    impl HirFormatter for pattern::Pattern {
        fn write(&self, _: &dyn HirDb, f: &mut Formatter, scope: &Scope) -> std::fmt::Result {
            use pattern::Pattern::*;

            match self {
                Empty => scope.write(f, "_"),
                Literal(_) => todo!(),
                Wildcard(_) => scope.write(f, "_"),
                Rest(_) => scope.write(f, ".."),
                Error(_) => scope.write(f, "!"),
                Constructor(_) => todo!(),
                Binding(_) => todo!(),
            }
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
                QPath(_) => todo!(),
                App(_) => todo!(),
                Arrow(_) => todo!(),
                Downgrade(_) => todo!(),
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
                Call(_) => todo!(),
                Ann(_) => todo!(),
                Abs(_) => todo!(),
                Match(_) => todo!(),
                Upgrade(_) => todo!(),
            }
        }
    }
}
