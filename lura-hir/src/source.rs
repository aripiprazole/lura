//! Defines the High-Level Intermediate Representation of the Lura language. It's intended to be
//! used to create a compiler for the Lura language.
//!
//! The HIR is exposing a object-oriented API, that can be used to create a compiler for the Lura

#![allow(clippy::too_many_arguments)]

use std::collections::HashSet;

use lura_diagnostic::{Offset, TextRange};
use lura_syntax::Source;

use crate::{package::Package, scope::Scope};

pub trait OptionExt<T> {
    /// Returns the contained [`Some`] value or a default.
    ///
    /// Consumes the `self` argument then, if [`Some`], returns the contained
    /// value, otherwise if [`None`], returns the [default value] for that
    /// type.
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T;
}

/// A trait for giving a type a useful default value.
///
/// Sometimes, you want to fall back to some kind of default value, and
/// don't particularly care what it is.
pub trait DefaultWithDb {
    /// Returns the "default value" for a type.
    ///
    /// Default values are often some kind of initial value, identity value, or anything else that
    /// may make sense as a default.
    fn default_with_db(db: &dyn crate::HirDb) -> Self;
}

/// A trait for recovering to default value, but with a database and reporting errors.
pub trait RecoverWithDb {
    /// Returns the "default value" for a type.
    ///
    /// Default values are often some kind of initial value, identity value, or anything else that
    /// may make sense as a default.
    fn recover_with_db(db: &dyn crate::HirDb, value: &str) -> Self;
}

impl<T: DefaultWithDb> OptionExt<T> for Option<T> {
    /// Returns the contained [`Some`] value or a default.
    ///
    /// Consumes the `self` argument then, if [`Some`], returns the contained
    /// value, otherwise if [`None`], returns the [default value] for that
    /// type.
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T {
        self.unwrap_or_else(|| T::default_with_db(db))
    }
}

impl<T: Default> DefaultWithDb for T {
    fn default_with_db(_db: &dyn crate::HirDb) -> Self {
        Self::default()
    }
}

/// Return the default value of a type according to the `DefaultWithDb` trait.
///
/// The type to return is inferred from context; this is equivalent to
/// `DefaultWithDb::default_with_db(db)` but shorter to type.
pub fn default_with_db<T: DefaultWithDb>(db: &dyn crate::HirDb) -> T {
    T::default_with_db(db)
}

/// A text range in a source file with a file name and the source text. This is the concrete
/// implementation of [`Location`].
///
/// [`Location`]: crate::Location
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirTextRange {
    pub source: Source,
    pub start: Offset,
    pub end: Offset,

    pub file_name: String,
    pub text: String,
}

/// A location in a source file. It can be either a text range or a lazy location to be evaluated
/// in the `call_site`.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Location {
    /// A text range in a source file.
    TextRange(HirTextRange),
    CallSite,
}

impl Location {
    /// Creates a new [Location] with the given [`source`] and range of [`start`] and [`end`].
    pub fn new<I>(db: &dyn crate::HirDb, source: Source, start: I, end: I) -> Self
    where
        I: Into<Offset>,
    {
        Self::TextRange(HirTextRange {
            source,
            start: start.into(),
            end: end.into(),
            file_name: source.file_path(db).to_string_lossy().into_owned(),
            text: source.source_text(db).clone(),
        })
    }

    /// Creates a "call site" location, this is a location that is not resolved yet, and will be
    /// resolved later.
    ///
    /// It takes an [`db`] argument, just to be consistent with the other methods. And if sometime
    /// it will need the db argument, it will be already there, and we won't need to change the
    /// method signature, and the call sites.
    pub fn call_site(_db: &dyn crate::HirDb) -> Self {
        Self::CallSite
    }
}

impl TextRange for Location {
    /// Returns the start offset of the source file.
    fn start(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.start,
            Location::CallSite => Offset(0),
        }
    }

    /// Returns the end offset of the source file.
    fn end(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.end,
            Location::CallSite => Offset(0),
        }
    }

    /// Returns the file name of the source file.
    fn file_name(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.file_name,
            Location::CallSite => "unresolved",
        }
    }

    /// Returns the text of the source file.
    fn source(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.text,
            Location::CallSite => "",
        }
    }
}

/// Defines an element of the High-Level Intermediate Representation. It's implemented for all
/// elements of the HIR.
pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self, db: &dyn crate::HirDb) -> Location;
}

/// A diagnostic error node for the HIR. It can be anything that can be reported to the diagnostic
/// database.
#[salsa::tracked]
pub struct HirError {
    /// The location of the error.
    pub location: Location,
}

/// Defines the tracking of a HIR source code file. It's the base struct of the HIR.
#[salsa::tracked]
pub struct HirSource {
    /// The source of the source file.
    pub source: Source,

    /// The package of the source file.
    pub package: Package,

    /// The scope of the source file. It's a [`Scope`] that contains all the definitions of the
    /// source file.
    ///
    /// It's useful for searching for definitions, and for resolving names.
    pub scope: Scope,

    /// The resolved top level definitions of the source file.
    #[return_ref]
    pub contents: Vec<top_level::TopLevel>,
}

/// A unresolved path in the HIR. It's used to represent a path that is not resolved yet, and will
/// be resolved as a [`Definition`] later.
///
/// It does take segments and location.
#[salsa::input]
pub struct HirPath {
    /// The location of the path.
    pub location: Location,

    #[return_ref]
    pub segments: Vec<Identifier>,
}

#[salsa::tracked]
impl HirPath {
    /// Dumps to string the path. It's used to create a string representation of the path for human
    /// readable diagnostics.
    ///
    /// It's just like `a.b.c`, for example.
    #[salsa::tracked]
    pub fn to_string(self, db: &dyn crate::HirDb) -> Option<String> {
        // If the path is empty, it's not a valid path. So, we return `None`.
        if self.segments(db).is_empty() {
            return None;
        }

        self.segments(db)
            .iter()
            .map(|segment| segment.contents(db))
            .collect::<Vec<_>>()
            .join(".")
            .into()
    }
}

impl DefaultWithDb for HirPath {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        Self::new(db, Location::call_site(db), vec![])
    }
}

/// A segment of a [`HirPath`]. It's used to represent a segment of a path, that can be either a
/// symbol or a identifier.
///
/// Both have string contents, and a location.
#[salsa::tracked]
pub struct Identifier {
    pub contents: String,
    pub refers_symbol: bool,

    /// The location of the identifier.
    pub location: Location,
}

impl Identifier {
    /// Creates a new identifier with the given [`contents`] and [`location`].
    pub fn symbol(db: &dyn crate::HirDb, contents: &str, location: Location) -> Self {
        Self::new(db, contents.into(), true, location)
    }
}

/// Represents contents within a location. It's used to represent a generic value that can have a
/// location too. Just like a tuple.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    /// The value of the spanned value.
    pub value: T,

    /// The location of the value. It can be [`None`] if the value is a call site.
    ///
    /// TODO: change directly to `Location`, as it have a `CallSite` variant now.
    pub location: Option<Location>,
}

impl<T> Spanned<T> {
    /// Creates a new spanned value with the given [`value`] and the location pointing to the
    /// `call_site`.
    pub fn on_call_site(value: T) -> Self {
        Self {
            value,
            location: None,
        }
    }

    /// Creates a new spanned value with the given [`value`] and [`location`].
    pub fn new(value: T, location: Location) -> Self {
        Self {
            value,
            location: Some(location),
        }
    }
}

/// A spanned value that can be created with the [`Default`] trait. Pointing to the `call_site`.
impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
            location: None,
        }
    }
}

/// Defines a kind of definition. The declarations are definitions that can have a name, and can be
/// referenced by other definitions.
///
/// The other definitions are just like declarations, but they can't be referenced by other.
pub mod declaration {
    use crate::resolve::Definition;

    use super::*;

    /// Represents a declaration in the HIR. It's a definition that can be referenced by other
    /// definitions.
    pub trait Declaration: HirElement {
        /// Returns the attributes of the declaration. It does rule how the declaration will be
        /// interpreted/generated, after the resolution step.
        ///
        /// TODO: mark reflection as an idea.
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<Attribute>;

        /// Returns the visibility of the declaration. It does rule "who" can access this
        /// declaration.
        fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<Vis>;

        /// Returns the documentation of the declaration. It does rule how the declaration will be
        /// documented, and presented to others in their IDEs.
        fn docs(&self, db: &dyn crate::HirDb) -> Vec<DocString>;

        /// Returns the name of the declaration.
        fn name(&self, db: &dyn crate::HirDb) -> Definition;

        /// Returns the parameters of the declaration. It's not obligatory to have parameters, but
        /// it's obligatory to return a vector of parameters.
        ///
        /// If the declaration doesn't have parameters, it should return an empty vector.
        fn parameters(&self, db: &dyn crate::HirDb) -> Vec<Parameter>;

        /// Returns the type representation of the declaration. It's not obligatory to have a type
        /// representation, but it's obligatory to return an [`Option`] of type representation.
        ///
        /// The type representation is used to type check the declaration, and to infer the type of
        /// the declaration.
        fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep>;

        /// Upcasts the declaration to a [`DeclDescriptor`]. It's used to create a descriptor of
        /// the declaration, that can be used to create a [`Definition`].
        fn upcast(&self, db: &dyn crate::HirDb) -> top_level::DeclDescriptor;
    }

    /// Defines an attribute for a declaration. It's used to rule how the declaration will be
    /// interpreted/generated, after the resolution step.
    ///
    /// TODO: mark reflection as an idea.
    ///
    /// It does have a name, arguments, and a location.
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

    /// Defines a documentation string for a declaration. It's used to rule how the declaration
    /// will be documented, and presented to others in their IDEs.
    ///
    /// It does have a range, and a location.
    #[salsa::tracked]
    pub struct DocString {
        pub range: Location,
    }

    /// Defines a visibility for a declaration. It's used to rule "who" can access this
    /// declaration.
    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Vis {
        /// The declaration is public, and can be accessed by anyone.
        #[default]
        Public,

        /// The declaration is public, but if it's a trait, it can only be implemented by the
        /// package that defines it.
        Sealed,

        /// The declaration is private, and can only be accessed by the file that defines it.
        Private,

        /// The declaration is private to the package that defines it, and can only be accessed by
        /// the package that defines it.
        Internal,
    }

    /// Defines a parameter for a declaration. It's a function parameter, and it's used to type
    /// a binding, and a type representation for the parameter.
    ///
    /// It can be either implicit or explicit, and rigid or unrigid.
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
        /// Creates a new explicit parameter with the given [`binding`], [`type_rep`], that
        /// is rigid.
        pub fn explicit(
            db: &dyn crate::HirDb,
            binding: pattern::Pattern,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self::new(db, binding, type_rep, false, true, location)
        }

        /// Creates a new implicit parameter with the given [`binding`], [`type_rep`], that
        /// is rigid.
        pub fn implicit(
            db: &dyn crate::HirDb,
            binding: pattern::Pattern,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self::new(db, binding, type_rep, true, true, location)
        }

        /// Creates a new explicit parameter with the given [`binding`], [`type_rep`], that
        /// is unrigid. It can be transformed into an implicit parameter, if the type signature
        /// requires it.
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
            Self::new(
                db,
                /* binding     = */ pattern::Pattern::Empty,
                /* type_rep    = */ type_rep.clone(),
                /* is_implicit = */ false,
                /* rigid       = */ true,
                /* location    = */ type_rep.location(db),
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

/// Defines a kind of definition. The top level declarations are like statements for a file in HIR,
/// and they can maybe be referenced by other definitions.
///
/// The other definitions are just like declarations, but they can't be referenced by other.
pub mod top_level {
    use crate::resolve::Definition;

    use super::*;

    /// Defines a top level declaration signature declaration. It's a declaration that can be
    /// referenced by other definitions. And it can have clauses, that are the implementations of
    /// the signature.
    ///
    /// It does have attributes, visibility, documentation, name, parameters, return type, and
    ///
    /// ## Examples
    ///
    /// ```hs
    /// f : String -> String
    /// f x = x
    /// ```
    ///
    /// In this example, `f : ...` is a signature declaration, and `f x = x` is a clause.
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

    /// Defines a top level declaration binding group. It's a declaration that can be referenced by
    /// other definitions. And it can have a single signature and multiple clauses, that are the
    /// implementations of the signature.
    ///
    /// It does have attributes, visibility, documentation, name, parameters, return type, and
    /// clauses.
    ///
    /// ## Examples
    /// ```hs
    /// f x = x
    /// ```
    ///
    /// This is a clause, of a signature declaration.
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

    /// A Binding Group is the combination of a single signature and multiple clauses. It's a
    /// declaration that can be referenced by other definitions.
    ///
    /// It does have attributes, visibility, documentation, name, parameters, return type, and
    /// clauses, that references the signature's ones.
    ///
    /// ## Examples
    /// ```hs
    /// f : String -> String
    /// f x = x
    /// ```
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

    /// Defines a "command-like" top level declaration. It does import a module, or use a top level
    /// definition, into the file scope.
    ///
    /// It does have location, and receives a [`Definition`] and a [`Location`].
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

    /// Defines a "command-like" top level declaration. It does literally send a command to the
    /// compiler, it's used to generate code, and to do other things that are not related to the
    /// main language semantics.
    ///
    /// It does have location, and receives a [`Definition`] and a [`Location`].
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
        pub docs: Vec<declaration::DocString>,
        pub visibility: Spanned<declaration::Vis>,
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
        pub docs: Vec<declaration::DocString>,
        pub visibility: Spanned<declaration::Vis>,
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
        pub docs: Vec<declaration::DocString>,
        pub name: Definition,
        pub return_type: type_rep::TypeRep,
        pub location: Location,
    }

    impl declaration::Declaration for Constructor {
        fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute> {
            Self::attributes(*self, db)
        }

        fn visibility(&self, _db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
            Spanned::on_call_site(declaration::Vis::Public)
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

    /// Defines the style of a constructor. It can be either a function, or a Generalized Algebraic
    /// Data Type. It's used to improve the type checking of the constructors.
    #[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum ConstructorKind {
        Function,

        /// Generalized Algebraic Data Type. It can be just like haskell GADTs:
        ///
        /// ```hs
        /// data Expr (a) {
        ///   Lit : (value: Int) -> Expr Int,
        ///   Add : (lhs: Expr Int) -> (rhs: Expr Int) -> Expr Int;
        /// }
        /// ```
        Gadt,
    }

    /// Defines a top level declaration. It can be either a command to the compiler or a
    /// declaration, that holds a [`Definition`].
    ///
    /// It can have recovery errors, that are used to recover from errors, and to continue the
    /// parsing process.
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

    /// Defines a descriptor of a top level declaration. It's used to create a [`Definition`]. It's
    /// just like a [`TopLevel`], but it doesn't have the "command-like" top level declarations.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum DeclDescriptor {
        Empty,
        Error(HirError),
        BindingGroup(BindingGroup),
        ClassDecl(ClassDecl),
        TraitDecl(TraitDecl),
        DataDecl(DataDecl),
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

    /// A conversion from a [`TopLevel`] to a [`DeclDescriptor`]. It's used to create a descriptor
    /// of the top level declaration, that can be used to create a [`Definition`].
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

    /// A conversion from a [`DeclDescriptor`] to a [`TopLevel`]. It's used to create a top level
    /// declaration, that can be used to create a [`Definition`].
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

    /// Bridges the [`Declaration`] trait with the [`DeclDescriptor`] one. It's used to create a
    /// descriptor of the declaration, that can be used to create a [`Definition`].
    ///
    /// It's implementation functions just pattern match the [`DeclDescriptor`] variants, and
    /// delegates the call to the [`Declaration`] trait.
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
}

/// Defines a kind of terms. It does define patterns that can be used in clauses and pattern
/// matching agains't a value.
///
/// It can be known as the name of eliminating a value.
pub mod pattern {
    use crate::resolve::Definition;

    use super::*;

    /// The constructor kind. It's used to define the kind of a constructor, and to improve the
    /// type checking of the constructors.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Constructor {
        Array,
        Tuple,
        Unit,
        Path(Definition),
    }

    /// Defines a constructor pattern. It's a pattern that can be used to match agains't a
    /// constructor. It's matching agains't a constructor, and it's arguments.
    ///
    /// The constructor can have different kinds, like a tuple, a unit, a path, or a array, but
    /// the signature is the same.
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

    /// Defines a binding pattern. It's a pattern that can be used to bind a value to a name. It's
    /// matching agains't any value, but commiting a name to it's value.
    ///
    /// The semantics are like a wildcard pattern, but it's not a wildcard pattern, because it
    /// defines a name.
    #[salsa::tracked]
    pub struct BindingPattern {
        pub name: Definition,
        pub location: Location,
    }

    impl HirElement for BindingPattern {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    /// Defines the pattern element in the HIR.
    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
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
        #[default]
        Empty,
        Literal(Spanned<literal::Literal>),
        Wildcard(Location),
        Rest(Location),
        Error(HirError),
        Constructor(ConstructorPattern),
        Binding(BindingPattern),
    }

    impl HirElement for Pattern {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Literal(literal) => literal.location.clone().unwrap(),
                Self::Wildcard(location) => location.clone(),
                Self::Rest(location) => location.clone(),
                Self::Error(downcast) => downcast.location(db),
                Self::Constructor(downcast) => downcast.location(db),
                Self::Binding(downcast) => downcast.location(db),
            }
        }
    }
}

/// Defines a kind of statements. It does define statements that can be used in a block, and
/// "do-notations", that are used to do imperative code in a functional language.
pub mod stmt {
    use super::*;

    /// Defines a ask statement, it will bind the value to a pattern, and it will return the value
    /// of the pattern.
    ///
    /// It's a sugar for a (`>>=`) function application. In the first versions of this language,
    /// there will not exist the function to desugar, but in the future, it will be implemented.
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

    /// Defines a let binding statement, it will bind the value to a pattern, and it will return
    /// the value of the pattern. It's a sugar to a function application, and it's just like a
    /// [`AskStmt`], but it's not a sugar to binding the "do-notation".
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

    /// Defines the statement element in the HIR.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Stmt {
        Empty,
        Error(HirError),
        Ask(AskStmt),
        Let(LetStmt),
        Downgrade(expr::Expr),
    }

    impl HirElement for Stmt {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::Ask(downcast) => downcast.location(db),
                Self::Let(downcast) => downcast.location(db),
                Self::Downgrade(downcast) => (*downcast).location(db),
            }
        }
    }

    /// Represents a list of statements within a return value of a block. It's representing a
    /// "do-notation" code block.
    ///
    /// It can be either a last function's arguments or a function body.
    #[salsa::tracked]
    pub struct Block {
        pub statements: Vec<Stmt>,
        pub location: Location,
    }

    impl DefaultWithDb for Block {
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            Self::new(db, vec![], Location::call_site(db))
        }
    }

    impl HirElement for Block {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }
}

/// Defines a kind of primaries. It does define terms that are literally literals, and it's used
/// as numbers, strings, etc... These are the base of the base of the base of the language
pub mod literal {
    use super::*;

    /// Defines a literal element in the HIR.
    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Literal {
        #[default]
        Empty,

        Int8(i8),
        UInt8(u8),
        Int16(i16),
        UInt16(u16),
        Int32(i32),
        UInt32(u32),
        Int64(i64),
        UInt64(u64),

        /// Defines a string literal. It's used to represent a string value.
        String(String),

        /// Defines a boolean literal. It's used to represent a boolean value.
        Boolean(bool),

        /// Defines a character literal. It's used to represent a character value.
        Char(char),
    }

    impl Literal {
        /// Defines the true literal. It's used to represent a boolean value.
        pub const TRUE: Literal = Literal::Boolean(true);

        /// Defines the false literal. It's used to represent a boolean value.
        pub const FALSE: Literal = Literal::Boolean(false);

        /// Creates a literal pattern from a literal. It's used to create a pattern from a literal.
        ///
        /// It's not currently supported by the language, but it will be in the future. So the
        /// compiler will emit an error.
        pub fn upgrade_pattern(self, loc: Location, _db: &dyn crate::HirDb) -> pattern::Pattern {
            // TODO: report error

            pattern::Pattern::Literal(Spanned::new(self, loc))
        }

        /// Creates a literal expression from a literal. It's used to create a expression from a
        /// literal.
        pub fn upgrade_expr(self, loc: Location, _db: &dyn crate::HirDb) -> expr::Expr {
            expr::Expr::Literal(Spanned::new(self, loc))
        }
    }
}

/// Defines a kind of terms. It does define expressions that can be used in a block. These are the
/// base of the language grammar and semantics.
pub mod expr {
    use lura_diagnostic::{Diagnostics, Report};

    use crate::resolve::{Definition, HirDiagnostic};

    use super::*;

    /// Defines a kind of match. It's used to define the kind of a match expression, and to improve
    /// pretty printing, and showing assists on the IDE or CLI.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum MatchKind {
        If,
        Match,

        /// If it's like an "If statement" instead of an "If expression", it will be a statement
        /// level match, and it will not return a value.
        StmtLevel(Box<MatchKind>),
    }

    /// Represents the value a call expression is calling. It can be either a definition, or an
    /// expression. Or it can be a special value, like a tuple, an array, or a unit.
    ///
    /// It's used to improve the type checking of the call expressions.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Callee {
        Array,
        Tuple,
        Unit,
        Pure,
        Do,
        Definition(Definition),
        Expr(expr::Expr),
    }

    /// Defines a kind of call. It's used to define the kind of a call expression, and to improve
    /// pretty printing.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum CallKind {
        Error,
        Infix,
        Prefix,
    }

    /// Represents a lambda abstraction expression, it's always curried, and it's used to define
    /// functions.
    ///
    /// As it is curried, it does mean that the parameters are in the reality alone, and the body
    /// is a function that receives the parameters, and returns a value.
    ///
    /// This is called Abs as a short for Abstraction, because lambda abstraction is the main
    /// principle of abstraction in a functional language.
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

    /// Represents an annotation expression, it works just like a cast operator, but in the type
    /// system, the type system will try to "cast", and if it's unsound, it will report an error.
    ///
    /// It's used to improve the type checking of the expressions.
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

    /// Represents a match arm. It's used to define a match expression, and to improve the type
    /// checking of the match expressions.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct MatchArm {
        /// The leftmost pattern of the match arm. It's used to match agains't the value.
        pub pattern: pattern::Pattern,

        /// The rightmost expression of the match arm. It's used to return a value.
        pub value: expr::Expr,

        pub location: Location,
    }

    impl HirElement for MatchArm {
        fn location(&self, _db: &dyn crate::HirDb) -> Location {
            // As it's not tracked by salsa, it's not possible to get the location from the salsa
            // database, so it's just returning the location of the match arm, cloning it
            self.location.clone()
        }
    }

    /// Represents a match expression. It's used to match agains't a value, and to return a value
    /// based on the match.
    ///
    /// It's used to improve the type checking of the expressions.
    #[salsa::tracked]
    pub struct MatchExpr {
        pub kind: MatchKind,
        pub scrutinee: expr::Expr,
        pub clauses: Vec<MatchArm>,
        pub location: Location,
    }

    impl HirElement for MatchExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    /// Represents a call expression, or a function application, or anything like this... It's used
    /// to call a [`Callee`], or a value that can be called.
    ///
    /// It does holds the kind of the call to improve the pretty printing.
    #[salsa::tracked]
    pub struct CallExpr {
        pub kind: CallKind,
        pub callee: Callee,
        pub arguments: Vec<expr::Expr>,

        /// The do-notation is a syntax sugar for a block, so it's possible to have a block as the
        /// last parameter of a function, and it will be used as the do-notation. It's inspired on
        /// Kotlin's syntax sugar for lambdas.
        pub do_notation: Option<stmt::Block>,

        pub location: Location,
    }

    impl HirElement for CallExpr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    /// Defines the expression element in the HIR. It's the most important element in the HIR, and
    /// in the language itself, as it's defines instructions that can be executed, and values that
    /// can be used.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Expr {
        /// The empty expression should work as a sentinele value, it's just like a `()` value. But
        /// it's splitted into a different variant to make it easier to work with, when we don't
        /// have an actual expression, like in [`Self::Error`].
        Empty,

        Error(HirError),
        Path(Definition),
        Literal(Spanned<literal::Literal>),
        Call(CallExpr),
        Ann(AnnExpr),
        Abs(AbsExpr),
        Match(MatchExpr),

        /// The upgrade type, it's used to upgrade a expression to a type representation. This is
        /// a data strucutre to improve error messages, and to make it easier to implement dependent
        /// types in the future.
        Upgrade(Box<expr::type_rep::TypeRep>),
    }

    impl Expr {
        /// Creates a unit expression. It's used to create a unit expression, that is just like a
        /// `()` value.
        pub fn call_unit_expr(location: Location, db: &dyn crate::HirDb) -> Self {
            Self::Call(CallExpr::new(
                db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Unit,
                /* arguments   = */ vec![],
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        /// Creates a block do-notation expression. It's used to create a block expression, that is
        /// just like a `do { }` value.
        pub fn block(db: &dyn crate::HirDb, do_notation: stmt::Block) -> Self {
            Self::Call(CallExpr::new(
                db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Do,
                /* arguments   = */ vec![],
                /* do_notation = */ Some(do_notation),
                /* location    = */ do_notation.location(db),
            ))
        }

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
                Self::Literal(downcast) => downcast.location.clone().unwrap(),
                Self::Call(downcast) => downcast.location(db),
                Self::Ann(downcast) => downcast.location(db),
                Self::Abs(downcast) => downcast.location(db),
                Self::Match(downcast) => downcast.location(db),
                Self::Upgrade(downcast) => (*downcast).location(db),
            }
        }
    }
}

/// Defines a kind of terms. It does define type representations that can be used in the type level
/// of the language. These are the base of the language grammar and semantics.
pub mod type_rep {
    use crate::resolve::Definition;

    use super::*;

    /// Defines a qualified path. It's used to define a type that is qualified by a trait type, like
    /// `Foo.Bar.Baz`.
    #[salsa::tracked]
    pub struct QPath {
        /// Usually a trait type path with associated type bindings, like `Foo.Bar.Baz`.
        pub qualifier: Definition,

        /// Usually a type name after the `.`, like `Bar` in `Foo.Bar`.
        pub name: Option<Identifier>,

        /// The location of qualified path
        pub location: Location,
    }

    impl HirElement for QPath {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            Self::location(*self, db)
        }
    }

    /// Defines a type application. It's used to apply a type to another type, like `Foo Bar`.
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

    /// Defines the expression element in the HIR. It's the second most important element in the
    /// HIR, and in the language itself, as it's defines instructions that can be executed, and
    /// values that can be used.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum TypeRep {
        /// The unit type
        Unit,

        /// The empty type representation should work as a sentinele value, it's just like a `()`
        /// value. But it's splitted into a different variant to make it easier to work with, when
        /// we don't have an actual type representation, like in [`Self::Error`].
        Empty,

        /// The type representation for Self in Lura language.
        This,

        /// An error type representation, it's used to recover from errors, and to continue the
        /// parsing process.
        Error(HirError),

        /// A path to a type, it can be either a type alias, a type parameter, or a type definition.
        Path(Definition),

        /// A qualified path, it's used to define a type that is qualified by a trait type, like
        /// `Foo.Bar.Baz`.
        QPath(QPath),

        /// A type application, it's used to apply a type to another type, like `Foo Bar`.
        App(AppTypeRep),

        /// The pi type, it's used to define a function type, or a dependent function type.
        ///
        /// TODO: implement dependent types, even tough the name is "pi", the type is not dependent
        Pi {
            parameters: Vec<declaration::Parameter>,
            value: Box<Self>,

            /// The location of the `->` keyword.
            location: Location,
        },

        /// The sigma type, it's used to define a product type, or a dependent product type. Currently
        /// the syntax is purposed to be used as implicit parameters for the language.
        ///
        /// TODO: implement dependent types, even tough the name is "sigma", the type is not dependent
        Sigma {
            parameters: Vec<declaration::Parameter>,
            value: Box<Self>,

            /// The location of the `forall` keyword.
            location: Location,
        },

        /// The downgrade type, it's used to downgrade a type representation to a expression. This is
        /// a data strucutre to improve error messages, and to make it easier to implement dependent
        /// types in the future.
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
        /// The `Empty` should unify with any type representation, so it's useful to create `_` like
        /// types.
        fn default_with_db(_db: &dyn crate::HirDb) -> Self {
            Self::Empty
        }
    }

    impl HirElement for TypeRep {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Unit => Location::call_site(db),
                Self::Empty => Location::call_site(db),
                Self::This => Location::call_site(db),
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
