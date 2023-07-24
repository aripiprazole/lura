#![allow(clippy::too_many_arguments)]

use std::collections::HashSet;

use lura_diagnostic::{Offset, TextRange};
use lura_syntax::Source;

use crate::{package::Package, scope::Scope};

pub trait OptionExt<T> {
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T;
}

pub trait DefaultWithDb {
    fn default_with_db(db: &dyn crate::HirDb) -> Self;
}

impl<T: DefaultWithDb> OptionExt<T> for Option<T> {
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T {
        self.unwrap_or_else(|| T::default_with_db(db))
    }
}

impl<T: Default> DefaultWithDb for T {
    fn default_with_db(_db: &dyn crate::HirDb) -> Self {
        Self::default()
    }
}

pub fn default_with_db<T: DefaultWithDb>(db: &dyn crate::HirDb) -> T {
    T::default_with_db(db)
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirTextRange {
    pub source: Source,
    pub start: Offset,
    pub end: Offset,

    pub file_name: String,
    pub text: String,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Location {
    TextRange(HirTextRange),
    CallSite,
}

impl Location {
    pub fn new<I>(db: &dyn crate::HirDb, source: Source, start: I, end: I) -> Self
    where
        I: Into<Offset>,
    {
        Self::TextRange(HirTextRange {
            source,
            start: start.into(),
            end: end.into(),
            file_name: source.source_text(db).clone(),
            text: source.file_path(db).to_string_lossy().into_owned(),
        })
    }

    pub fn call_site(_db: &dyn crate::HirDb) -> Self {
        Self::CallSite
    }
}

impl TextRange for Location {
    fn start(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.start,
            Location::CallSite => Offset(0),
        }
    }

    fn end(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.end,
            Location::CallSite => Offset(0),
        }
    }

    fn file_name(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.file_name,
            Location::CallSite => "unresolved",
        }
    }

    fn source(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.text,
            Location::CallSite => "unresolved",
        }
    }
}

pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self, db: &dyn crate::HirDb) -> Location;
}

#[salsa::tracked]
pub struct HirError {
    pub location: Location,
}

#[salsa::tracked]
pub struct HirSource {
    pub source: Source,
    pub package: Package,
    pub scope: Scope,

    #[return_ref]
    pub contents: Vec<top_level::TopLevel>,
}

#[salsa::input]
pub struct HirPath {
    pub location: Location,

    #[return_ref]
    pub segments: Vec<Identifier>,
}

impl DefaultWithDb for HirPath {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        Self::new(db, Location::call_site(db), vec![])
    }
}

#[salsa::tracked]
pub struct Identifier {
    pub contents: String,
    pub refers_symbol: bool,
    pub location: Location,
}

impl Identifier {
    pub fn symbol(db: &dyn crate::HirDb, contents: &str, location: Location) -> Self {
        Self::new(db, contents.into(), true, location)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub location: Option<Location>,
}

impl<T> Spanned<T> {
    pub fn on_call_site(value: T) -> Self {
        Self {
            value,
            location: None,
        }
    }

    pub fn new(value: T, location: Location) -> Self {
        Self {
            value,
            location: Some(location),
        }
    }
}

impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
            location: None,
        }
    }
}

pub mod declaration {
    use crate::resolve::Definition;

    use super::*;

    pub trait Declaration: HirElement {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<Attribute>;
        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<Vis>;
        fn docs(&self, db: &dyn crate::HirDb) -> Vec<DocString>;
        fn name(&self, db: &dyn crate::HirDb) -> Definition;
        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<Parameter>;
        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep>;
        fn upcast(&self, db: &dyn crate::HirDb) -> top_level::DeclDescriptor;
    }

    #[salsa::tracked]
    pub struct Attribute {
        pub name: HirPath,
        pub arguments: Vec<expr::Expr>,
        pub location: Location,
    }

    impl DefaultWithDb for Attribute {
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            let name = default_with_db(db);

            Attribute::new(db, name, vec![], Location::call_site(db))
        }
    }

    #[salsa::tracked]
    pub struct DocString {
        pub range: Location,
    }

    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Vis {
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

        /// Whether this parameter is rigid, if it's not rigid, it can be transformed into a
        /// an implicit parameter.
        pub rigid: bool,

        pub location: Location,
    }

    impl Parameter {
        pub fn explicit(
            db: &dyn crate::HirDb,
            binding: pattern::Pattern,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self::new(db, binding, type_rep, false, true, location)
        }

        pub fn implicit(
            db: &dyn crate::HirDb,
            binding: pattern::Pattern,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self::new(db, binding, type_rep, true, true, location)
        }

        pub fn unrigid(
            db: &dyn crate::HirDb,
            binding: pattern::Pattern,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self::new(db, binding, type_rep, false, false, location)
        }

        /// Creates a new unnamed and explicit parameter, it does have an empty binding, that is
        /// just like an *wildcard* pattern.
        ///
        /// This is useful for constructing parameters with just the types, and not the bindings.
        pub fn unnamed(db: &dyn crate::HirDb, type_rep: type_rep::TypeRep) -> Self {
            let binding = pattern::Pattern::Empty;

            Self::new(
                db,
                binding,
                type_rep.clone(),
                false,
                true,
                type_rep.location(db),
            )
        }
    }

    impl DefaultWithDb for Parameter {
        /// Creates a new unit parameter. Just like `(): ()`, for default and error recovery
        /// purposes.
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            let binding = pattern::Pattern::Empty;
            let type_rep = type_rep::TypeRep::Unit;

            Self::new(db, binding, type_rep, false, false, Location::call_site(db))
        }
    }
}

pub mod top_level {
    use crate::resolve::Definition;

    use super::*;

    #[salsa::tracked]
    pub struct Signature {
        pub attributes: HashSet<declaration::Attribute>,
        pub docs: Vec<declaration::DocString>,
        pub visibility: Spanned<declaration::Vis>,
        pub name: Definition,
        pub parameters: Vec<declaration::Parameter>,
        pub return_type: type_rep::TypeRep,
        pub location: Location,
    }

    impl declaration::Declaration for Signature {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Self::visibility(*self, db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            Self::docs(*self, db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            Self::name(*self, db)
        }

        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            Self::parameters(*self, db)
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            Self::return_type(*self, db).into()
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::Empty
        }
    }

    impl HirElement for Signature {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct Clause {
        pub attributes: HashSet<declaration::Attribute>,
        pub name: Definition,
        pub arguments: Vec<pattern::Pattern>,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl HirElement for Clause {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct BindingGroup {
        pub signature: Signature,
        pub clauses: HashSet<Clause>,
    }

    impl declaration::Declaration for BindingGroup {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            self.signature(db).attributes(db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            self.signature(db).visibility(db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            self.signature(db).docs(db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            self.signature(db).name(db)
        }

        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            self.signature(db).parameters(db)
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            self.signature(db).type_rep(db)
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::BindingGroup(*self)
        }
    }

    impl HirElement for BindingGroup {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            self.signature(db).location(db)
        }
    }

    #[salsa::tracked]
    pub struct UsingTopLevel {
        pub path: Definition,
        pub location: Location,
    }

    impl HirElement for UsingTopLevel {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct CommandTopLevel {
        pub path: HirPath,
        pub arguments: Vec<expr::Expr>,
        pub location: Location,
    }

    impl HirElement for CommandTopLevel {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct ClassDecl {
        pub attributes: HashSet<declaration::Attribute>,
        pub visibility: Spanned<declaration::Vis>,
        pub docs: Vec<declaration::DocString>,
        pub name: Definition,
        pub parameters: Vec<declaration::Parameter>,
        pub return_type: type_rep::TypeRep,
        pub fields: Vec<Signature>,
        pub methods: Vec<BindingGroup>,
        pub location: Location,
    }

    impl declaration::Declaration for ClassDecl {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Self::visibility(*self, db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            Self::docs(*self, db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            Self::name(*self, db)
        }

        fn parameters(&self, _db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            Vec::new()
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            Self::return_type(*self, db).into()
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::ClassDecl(*self)
        }
    }

    impl HirElement for ClassDecl {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct TraitDecl {
        pub attributes: HashSet<declaration::Attribute>,
        pub visibility: Spanned<declaration::Vis>,
        pub docs: Vec<declaration::DocString>,
        pub name: Definition,
        pub parameters: Vec<declaration::Parameter>,
        pub return_type: type_rep::TypeRep,
        pub methods: Vec<Signature>,
        pub location: Location,
    }

    impl declaration::Declaration for TraitDecl {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Self::visibility(*self, db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            Self::docs(*self, db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            Self::name(*self, db)
        }

        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            Self::parameters(*self, db)
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            Self::return_type(*self, db).into()
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::TraitDecl(*self)
        }
    }

    impl HirElement for TraitDecl {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct DataDecl {
        pub attributes: HashSet<declaration::Attribute>,
        pub visibility: Spanned<declaration::Vis>,
        pub docs: Vec<declaration::DocString>,
        pub name: Definition,
        pub parameters: Vec<declaration::Parameter>,
        pub return_type: type_rep::TypeRep,
        pub variants: Vec<Constructor>,
        pub methods: Vec<BindingGroup>,
        pub location: Location,
    }

    impl declaration::Declaration for DataDecl {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Self::visibility(*self, db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            Self::docs(*self, db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            Self::name(*self, db)
        }

        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            Self::parameters(*self, db)
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            Self::return_type(*self, db).into()
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::DataDecl(*self)
        }
    }

    impl HirElement for DataDecl {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct Constructor {
        pub kind: ConstructorKind,
        pub attributes: HashSet<declaration::Attribute>,
        pub visibility: Spanned<declaration::Vis>,
        pub docs: Vec<declaration::DocString>,
        pub name: Definition,
        pub return_type: type_rep::TypeRep,
        pub location: Location,
    }

    impl declaration::Declaration for Constructor {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Self::visibility(*self, db)
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            Self::docs(*self, db)
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            Self::name(*self, db)
        }

        fn parameters(&self, _db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            Vec::new()
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            Self::return_type(*self, db).into()
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            top_level::DeclDescriptor::Empty
        }
    }

    impl HirElement for Constructor {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum ConstructorKind {
        Function,
        Gadt,
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum TopLevel {
        Empty,
        Error(HirError),
        Using(UsingTopLevel),
        Command(CommandTopLevel),
        BindingGroup(BindingGroup),
        ClassDecl(ClassDecl),
        TraitDecl(TraitDecl),
        DataDecl(DataDecl),
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum DeclDescriptor {
        Empty,
        Error(HirError),
        BindingGroup(BindingGroup),
        ClassDecl(ClassDecl),
        TraitDecl(TraitDecl),
        DataDecl(DataDecl),
    }

    impl TryFrom<DeclDescriptor> for TopLevel {
        type Error = ();

        fn try_from(value: DeclDescriptor) -> Result<Self, ()> {
            Ok(match value {
                DeclDescriptor::Empty => Self::Empty,
                DeclDescriptor::Error(downcast) => Self::Error(downcast),
                DeclDescriptor::BindingGroup(downcast) => Self::BindingGroup(downcast),
                DeclDescriptor::ClassDecl(downcast) => Self::ClassDecl(downcast),
                DeclDescriptor::TraitDecl(downcast) => Self::TraitDecl(downcast),
                DeclDescriptor::DataDecl(downcast) => Self::DataDecl(downcast),
            })
        }
    }

    impl TryFrom<TopLevel> for DeclDescriptor {
        type Error = ();

        fn try_from(value: TopLevel) -> Result<Self, ()> {
            Ok(match value {
                TopLevel::Empty => Self::Empty,
                TopLevel::Error(downcast) => Self::Error(downcast),
                TopLevel::Using(_) => return Err(()),
                TopLevel::Command(_) => return Err(()),
                TopLevel::BindingGroup(downcast) => Self::BindingGroup(downcast),
                TopLevel::ClassDecl(downcast) => Self::ClassDecl(downcast),
                TopLevel::TraitDecl(downcast) => Self::TraitDecl(downcast),
                TopLevel::DataDecl(downcast) => Self::DataDecl(downcast),
            })
        }
    }

    impl declaration::Declaration for DeclDescriptor {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            match self {
                Self::Empty => Default::default(),
                Self::Error(_) => Default::default(),
                Self::BindingGroup(downcast) => downcast.attributes(db),
                Self::ClassDecl(downcast) => downcast.attributes(db),
                Self::TraitDecl(downcast) => downcast.attributes(db),
                Self::DataDecl(downcast) => downcast.attributes(db),
            }
        }

        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            match self {
                Self::Empty => Default::default(),
                Self::Error(_) => Default::default(),
                Self::BindingGroup(downcast) => downcast.visibility(db),
                Self::ClassDecl(downcast) => downcast.visibility(db),
                Self::TraitDecl(downcast) => downcast.visibility(db),
                Self::DataDecl(downcast) => downcast.visibility(db),
            }
        }

        fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
            match self {
                Self::Empty => Default::default(),
                Self::Error(_) => Default::default(),
                Self::BindingGroup(downcast) => downcast.docs(db),
                Self::ClassDecl(downcast) => downcast.docs(db),
                Self::TraitDecl(downcast) => downcast.docs(db),
                Self::DataDecl(downcast) => downcast.docs(db),
            }
        }

        fn name(&self, db: &dyn crate::HirDb) -> Definition {
            match self {
                Self::Empty => default_with_db(db),
                Self::Error(_) => default_with_db(db),
                Self::BindingGroup(downcast) => downcast.name(db),
                Self::ClassDecl(downcast) => downcast.name(db),
                Self::TraitDecl(downcast) => downcast.name(db),
                Self::DataDecl(downcast) => downcast.name(db),
            }
        }

        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
            match self {
                Self::Empty => Default::default(),
                Self::Error(_) => Default::default(),
                Self::BindingGroup(downcast) => downcast.parameters(db),
                Self::ClassDecl(downcast) => downcast.parameters(db),
                Self::TraitDecl(downcast) => downcast.parameters(db),
                Self::DataDecl(downcast) => downcast.parameters(db),
            }
        }

        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
            match self {
                Self::Empty => Default::default(),
                Self::Error(_) => Default::default(),
                Self::BindingGroup(downcast) => downcast.type_rep(db),
                Self::ClassDecl(downcast) => downcast.type_rep(db),
                Self::TraitDecl(downcast) => downcast.type_rep(db),
                Self::DataDecl(downcast) => downcast.type_rep(db),
            }
        }

        fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
            self.clone()
        }
    }

    impl HirElement for DeclDescriptor {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::BindingGroup(downcast) => downcast.location(db),
                Self::ClassDecl(downcast) => downcast.location(db),
                Self::TraitDecl(downcast) => downcast.location(db),
                Self::DataDecl(downcast) => downcast.location(db),
            }
        }
    }

    impl HirElement for TopLevel {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::Using(downcast) => downcast.location(db),
                Self::Command(downcast) => downcast.location(db),
                Self::BindingGroup(downcast) => downcast.location(db),
                Self::ClassDecl(downcast) => downcast.location(db),
                Self::TraitDecl(downcast) => downcast.location(db),
                Self::DataDecl(downcast) => downcast.location(db),
            }
        }
    }
}

pub mod pattern {
    use crate::resolve::Definition;

    use super::*;

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Constructor {
        Array,
        Tuple,
        Unit,
        Path(Definition),
    }

    #[salsa::tracked]
    pub struct ConstructorPattern {
        pub name: Constructor,
        pub arguments: Vec<pattern::Pattern>,
        pub location: Location,
    }

    impl HirElement for ConstructorPattern {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct BindingPattern {
        pub name: Identifier,
        pub location: Location,
    }

    impl HirElement for BindingPattern {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Pattern {
        /// The empty parameter should work as a wildcard, it's just like a `_` pattern. But it's
        /// splitted into a different variant to make it easier to work with, when we don't have
        /// an actual pattern.
        ///
        /// It's useful to do code generation. This pattern matches agains't every value, just like
        /// a wildcard.
        ///
        /// Actually, in this compiler, we have a few another patterns that are just like a wildcard
        /// pattern, but they are not represented as a wildcard pattern, because they are actual
        /// patterns, these are:
        /// - [`Pattern::Error`]
        /// - [`Pattern::Binding`]
        Empty,
        Wildcard(Location),
        Error(HirError),
        Constructor(ConstructorPattern),
        Binding(BindingPattern),
    }

    impl HirElement for Pattern {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Wildcard(location) => location.clone(),
                Self::Error(downcast) => downcast.location(db),
                Self::Constructor(downcast) => downcast.location(db),
                Self::Binding(downcast) => downcast.location(db),
            }
        }
    }
}

pub mod stmt {
    use super::*;

    #[salsa::tracked]
    pub struct AskStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl HirElement for AskStmt {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct LetStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl HirElement for LetStmt {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Stmt {
        Error(HirError),
        Ask(AskStmt),
        Let(LetStmt),
        Downgrade(expr::Expr),
    }

    impl HirElement for Stmt {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Error(downcast) => downcast.location(db),
                Self::Ask(downcast) => downcast.location(db),
                Self::Let(downcast) => downcast.location(db),
                Self::Downgrade(downcast) => (*downcast).location(db),
            }
        }
    }

    #[salsa::tracked]
    pub struct Block {
        pub statements: Vec<Stmt>,
        pub return_value: Option<expr::Expr>,
        pub location: Location,
    }

    impl HirElement for Block {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }
}

pub mod expr {
    use lura_diagnostic::{Diagnostics, Report};

    use crate::resolve::{Definition, HirDiagnostic};

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
        Definition(Definition),
        Expr(expr::Expr),
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
        pub value: expr::Expr,
        pub location: Location,
    }

    impl HirElement for AbsExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct AnnExpr {
        pub value: expr::Expr,
        pub type_rep: type_rep::TypeRep,
        pub location: Location,
    }

    impl HirElement for AnnExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct MatchArm {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl HirElement for MatchArm {
        fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    #[salsa::tracked]
    pub struct MatchExpr {
        pub kind: MatchKind,
        pub scrutinee: Box<expr::Expr>,
        pub clauses: Vec<MatchArm>,
        pub location: Location,
    }

    impl HirElement for MatchExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct CallExpr {
        pub kind: CallKind,
        pub callee: Callee,
        pub arguments: Vec<expr::Expr>,
        pub do_notation: Option<stmt::Block>,
        pub location: Location,
    }

    impl HirElement for CallExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Expr {
        Empty,
        Error(HirError),
        Path(Definition),
        Call(CallExpr),
        Ann(AnnExpr),
        Abs(AbsExpr),
        Match(MatchExpr),
        Upgrade(Box<expr::type_rep::TypeRep>),
    }

    impl Expr {
        /// Upgrades this expression to a type representation. This is useful for error recovery and
        /// future dependent types or refinement types integration.
        ///
        /// This function also reports an error currently, because it's not allowed dependent types
        /// on the language, this is the reason because it's good to error recovery.
        pub fn upgrade(self, _db: &dyn crate::HirDb) -> type_rep::TypeRep {
            // TODO: report error

            type_rep::TypeRep::Downgrade(Box::new(self))
        }
    }

    impl DefaultWithDb for Expr {
        /// The default expression is `Empty`. But it's not allowed to be used in any
        /// contexts, so this function should report it as an error, and return `Empty`. For better
        /// error reporting, the location of the `Empty` expression should be the same as
        /// the location of the context where it's used.
        ///
        /// TODO: This is not implemented yet.
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            Diagnostics::push(
                db,
                Report::new(HirDiagnostic {
                    message:
                        "Empty expression representation is not allowed to be used in any contexts"
                            .into(),
                    location: Location::call_site(db),
                }),
            );

            Self::Empty
        }
    }

    impl HirElement for Expr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::Path(downcast) => downcast.location(db),
                Self::Call(downcast) => downcast.location(db),
                Self::Ann(downcast) => downcast.location(db),
                Self::Abs(downcast) => downcast.location(db),
                Self::Match(downcast) => downcast.location(db),
                Self::Upgrade(downcast) => (*downcast).location(db),
            }
        }
    }
}

pub mod type_rep {
    use lura_diagnostic::{Diagnostics, Report};

    use crate::resolve::{Definition, HirDiagnostic};

    use super::*;

    #[salsa::tracked]
    pub struct QPath {
        /// Usually a trait type path with associated type bindings, like `Foo.Bar.Baz`.
        pub qualifier: Definition,

        /// Usually a type name after the `.`, like `Bar` in `Foo.Bar`.
        pub name: Option<Identifier>,
        pub location: Location,
    }

    impl HirElement for QPath {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[salsa::tracked]
    pub struct AppTypeRep {
        pub callee: TypeRep,
        pub arguments: Vec<type_rep::TypeRep>,
        pub location: Location,
    }

    impl HirElement for AppTypeRep {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum TypeRep {
        Unit,
        Empty,
        Error(HirError),
        Path(Definition),
        QPath(QPath),
        App(AppTypeRep),
        Pi {
            parameters: Vec<declaration::Parameter>,
            value: Box<Self>,

            /// The location of the `->` keyword.
            location: Location,
        },
        Sigma {
            parameters: Vec<declaration::Parameter>,
            value: Box<Self>,

            /// The location of the `forall` keyword.
            location: Location,
        },
        Downgrade(Box<expr::Expr>),
    }

    impl TypeRep {
        /// Downgrades this type representation to a expression. This is useful for error recovery and
        /// future dependent types or refinement types integration.
        ///
        /// This function also reports an error currently, because it's not allowed dependent types
        /// on the language, this is the reason because it's good to error recovery.
        pub fn downgrade(self, _db: &dyn crate::HirDb) -> expr::Expr {
            // TODO: report error

            expr::Expr::Upgrade(Box::new(self))
        }
    }

    impl DefaultWithDb for TypeRep {
        /// The default type representation is `Empty`. But it's not allowed to be used in any
        /// contexts, so this function should report it as an error, and return `Empty`. For better
        /// error reporting, the location of the `Empty` type representation should be the same as
        /// the location of the context where it's used.
        ///
        /// TODO: This is not implemented yet.
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            Diagnostics::push(
                db,
                Report::new(HirDiagnostic {
                    message: "Empty type representation is not allowed to be used in any contexts"
                        .into(),
                    location: Location::call_site(db),
                }),
            );

            Self::Empty
        }
    }

    impl HirElement for TypeRep {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Unit => Location::call_site(db),
                Self::Empty => Location::call_site(db),
                Self::Pi { location, .. } => location.clone(),
                Self::Sigma { location, .. } => location.clone(),
                Self::Error(downcast) => downcast.location(db),
                Self::Path(downcast) => downcast.location(db),
                Self::QPath(downcast) => downcast.location(db),
                Self::App(downcast) => downcast.location(db),
                Self::Downgrade(downcast) => (*downcast).location(db),
            }
        }
    }
}
