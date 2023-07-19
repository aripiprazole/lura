#![allow(clippy::too_many_arguments)]

use std::{collections::HashSet, path::PathBuf};

use lura_syntax::Source;

pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self) -> TextRange;
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TextRange {}

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
