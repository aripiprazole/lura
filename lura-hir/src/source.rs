#![allow(clippy::too_many_arguments)]

use std::{collections::HashSet, path::PathBuf};

use lura_syntax::Source;

use crate::package::Package;

pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self) -> Location;
}

pub trait HirAnchor: HirElement {
    fn anchor(&self, db: &dyn crate::HirDb) -> Location;
}

#[salsa::tracked]
pub struct HirError {
    pub location: TextRange,
}

#[salsa::tracked]
pub struct HirSource {
    #[id]
    pub id: HirSourceId,
    pub source: Source,
    pub anchor: Location,
    pub package: Package,

    #[return_ref]
    pub contents: Vec<top_level::TopLevel>,
}

#[salsa::interned]
pub struct HirSourceId {
    #[return_ref]
    pub path: PathBuf,
}

#[salsa::tracked]
pub struct QualifiedPath {}

#[salsa::tracked]
pub struct Identifier {}

/// Represents a specific location into the source string
/// as a utf-8 offset.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location(usize);

/// Represents an offset in the source program relative to some anchor.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offset(usize);

impl Offset {
    pub fn location(self, anchor: Location) -> Location {
        Location(self.0 + anchor.0)
    }
}

impl Location {
    pub fn as_usize(self) -> usize {
        self.0
    }

    pub fn start() -> Self {
        Self(0)
    }
}

impl std::ops::Add<Offset> for Location {
    type Output = Location;

    fn add(self, rhs: Offset) -> Self::Output {
        Location(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for Location {
    type Output = Location;

    fn add(self, rhs: usize) -> Self::Output {
        Location(self.0 + rhs)
    }
}

impl std::ops::AddAssign<usize> for Location {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs
    }
}

impl std::ops::Sub<Location> for Location {
    type Output = Offset;

    fn sub(self, rhs: Location) -> Self::Output {
        Offset(self.0 - rhs.0)
    }
}

/// Stores the location of a piece of IR within the source text.
/// Spans are not stored as absolute values but rather relative to some enclosing anchor
/// (some struct that implements the `HirAnchor` trait).
/// This way, although the location of the anchor may change, the spans themselves rarely do.
/// So long as a function doesn't convert the span into its absolute form,
/// and thus read the anchor's precise location, it won't need to re-execute, even if the anchor
/// has moved about in the file.
///
/// **NB:** It is your job, when converting the span into relative positions,
/// to supply the correct anchor! For example, the anchor for the expressions
/// within a function body is the function itself.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TextRange {
    /// Start of the span, relative to the anchor.
    pub start: Offset,

    /// End of the span, relative to the anchor.
    pub end: Offset,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub location: TextRange,
}

pub mod declaration {
    use super::*;

    pub trait Declaration: HirElement {
        fn attributes(&self) -> HashSet<Attribute>;
        fn visibility(&self) -> Visibility;
        fn docs(&self) -> Vec<DocString>;
        fn name(&self) -> QualifiedPath;
        fn parameters(&self) -> Vec<Parameter>;
        fn type_rep(&self) -> Option<type_rep::TypeRep>;
    }

    #[salsa::tracked]
    pub struct Attribute {
        pub name: QualifiedPath,
        pub arguments: Vec<expr::Expr>,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct DocString {
        pub range: TextRange,
    }

    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Visibility {
        #[default]
        Public,
        Sealed,
        Private,
        Internal,
    }

    #[salsa::tracked]
    pub struct Parameter {
        pub binding: pattern::Pattern,
        pub parameter_type: type_rep::TypeRep,

        /// Whether this parameter is implicit, i.e. it's a `forall` parameter.
        pub is_implicit: bool,
        pub location: TextRange,
    }
}

pub mod top_level {
    use super::*;

    #[salsa::tracked]
    pub struct Signature {
        pub attributes: HashSet<declaration::Attribute>,
        pub visibility: Spanned<declaration::Visibility>,
        pub docs: Vec<declaration::DocString>,
        pub name: QualifiedPath,
        pub parameters: Vec<declaration::Parameter>,
        pub return_type: type_rep::TypeRep,
        pub body: stmt::Block,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct Clause {
        pub name: QualifiedPath,
        pub arguments: Vec<pattern::Pattern>,
        pub value: expr::Expr,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum TopLevel {
        Error,
        Clause(Clause),
        Signature(Signature),
    }
}

pub mod pattern {
    use super::*;

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Constructor {
        Array,
        Tuple,
        Unit,
        Path(QualifiedPath),
    }

    #[salsa::tracked]
    pub struct ConstructorPattern {
        pub name: Constructor,
        pub arguments: Vec<pattern::Pattern>,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct BindingPattern {
        pub name: Identifier,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Pattern {
        Error,
        Constructor(ConstructorPattern),
        Binding(BindingPattern),
    }
}

pub mod stmt {
    use super::*;

    #[salsa::tracked]
    pub struct AskStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct LetStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Stmt {
        Error(HirError),
        Ask(AskStmt),
        Let(LetStmt),
        Expr(expr::Expr),
    }

    #[salsa::tracked]
    pub struct Block {
        pub statements: Vec<Stmt>,
        pub return_value: Option<expr::Expr>,
        pub location: TextRange,
    }
}

pub mod expr {
    use super::*;

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum MatchKind {
        If,
        Match,
        StmtLevel(Box<MatchKind>),
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Callee {
        Array,
        Tuple,
        Unit,
        Expr(Box<expr::Expr>),
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum CallKind {
        Error,
        Infix,
        Prefix,
    }

    #[salsa::tracked]
    pub struct AbsExpr {
        pub parameters: Vec<declaration::Parameter>,
        pub value: Box<expr::Expr>,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct AnnExpr {
        pub value: Box<expr::Expr>,
        pub type_rep: type_rep::TypeRep,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct MatchArm {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: TextRange,
    }

    #[salsa::tracked]
    pub struct MatchExpr {
        pub kind: MatchKind,
        pub scrutinee: Box<expr::Expr>,
        pub clauses: Vec<MatchArm>,
    }

    #[salsa::tracked]
    pub struct CallExpr {
        pub kind: CallKind,
        pub callee: Callee,
        pub arguments: Vec<expr::Expr>,
        pub do_notation: stmt::Block,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Expr {
        Error(HirError),
        Path(QualifiedPath),
        Call(CallExpr),
        Ann(AnnExpr),
        Abs(AbsExpr),
        Match(MatchExpr),
        Upgrade(Box<expr::type_rep::TypeRep>),
    }
}

pub mod type_rep {
    use super::*;

    #[salsa::tracked]
    pub struct QPath {
        /// Usually a trait type path with associated type bindings, like `Foo.Bar.Baz`.
        pub qualifier: QualifiedPath,
        pub name: Identifier,
        pub location: TextRange,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum TypeRep {
        Error(HirError),
        Path(QualifiedPath),
        QPath(QPath),
        Downgrade(Box<expr::Expr>),
    }
}
