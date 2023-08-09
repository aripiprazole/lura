use holes::*;
use lura_hir::resolve::empty_definition;
use lura_hir::resolve::Definition;
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

use crate::adhoc::Qual;

pub type Level = usize;

/// Represents a type-level function. This is used
/// to represent a type-level function.
pub type Hoas<M> = dyn Fn(Type<M>) -> Type<M> + Sync + Send;

/// Represents a type that could be sealed to erase mutability.
pub trait Quote {
    /// The sealed type.
    type Sealed;

    /// Seal the type.
    fn seal(self) -> Self::Sealed;
}

pub mod fun {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Clone, Hash)]
    pub struct QuotedFun {
        pub domain: Box<Type<state::Quoted>>,
        pub value: Box<Type<state::Quoted>>,
    }

    impl Display for QuotedFun {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.domain)?;
            write!(f, " -> ")?;
            write!(f, "{}", self.value)
        }
    }

    #[derive(Clone)]
    pub struct HoasFun {
        pub domain: Box<Type<state::Hoas>>,
        pub value: Arc<Hoas<state::Hoas>>,
    }

    impl Eq for HoasFun {}

    impl Display for HoasFun {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    impl Debug for HoasFun {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Fun")
                .field("domain", &self.domain)
                .field("value", &"<function>")
                .finish()
        }
    }

    impl PartialEq for HoasFun {
        fn eq(&self, other: &Self) -> bool {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(Name {
                definition: empty_definition(),
                name: "_".into(),
            }));

            self.domain == other.domain
                && self.value.call((variable.clone(),)) == other.value.call((variable,))
        }
    }

    impl Hash for HoasFun {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(Name {
                definition: empty_definition(),
                name: "_".into(),
            }));

            self.domain.hash(state);
            self.value.call((variable,)).hash(state);
        }
    }
}

/// Represents a primary type. This is used to represent a type that is not a constructor.
///
/// Can be a sentinel value that is used to represent an error.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primary {
    /// The error type. This is used to represent a type that is not valid. It's a sentinel value
    /// that is used to represent an error.
    #[default]
    Error,

    Type,
    Unit,
    Bool,
    String,
    Char,
    Int(u8, bool),
}

impl Primary {
    pub const U8: Self = Self::Int(8, false);
    pub const I8: Self = Self::Int(8, true);
    pub const U16: Self = Self::Int(16, false);
    pub const I16: Self = Self::Int(16, true);
    pub const U32: Self = Self::Int(32, false);
    pub const I32: Self = Self::Int(32, true);
    pub const U64: Self = Self::Int(64, false);
    pub const I64: Self = Self::Int(64, true);
}

pub type Uniq = usize;

/// Represents the rigidness of the type variable. This is used to represent the rigidness of the
/// type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rigidness {
    Rigid,
    Flexible,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub definition: Definition,
    pub name: String,
}

impl Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.name, f)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Bound {
    Flexible(Name),
    Index(Name, Level),
}

/// Represents a type dependent function. This is used to
/// represent a type dependent function.
///
/// This is used to represent a type variable that
/// has not been unified with a type.
///
/// NOTE: This is supposed to be used to implement GADT
/// and type families.
pub mod forall {
    use super::*;

    /// Represents a type-level function. This is used
    /// to represent a type-level function.
    pub type ForallHoas<M> = dyn Fn(Vec<Type<M>>) -> Type<M> + Sync + Send;

    /// Represents a type dependent function. This is used to
    /// represent a type dependent function.
    ///
    /// This is used to represent a type variable that
    /// has not been unified with a type.
    ///
    /// NOTE: This is supposed to be used to implement GADT
    /// and type families.
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub struct QuotedForall {
        pub name: Name,
        pub domain: Vec<(Name, Type<state::Quoted>)>,
        pub codomain: Box<Type<state::Quoted>>,
    }

    impl Display for QuotedForall {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let name = &self.name;
            let domain = &self.domain;
            let codomain = &self.codomain;

            todo!()

            // write!(f, "({name} : {domain}) -> {codomain}")
        }
    }

    #[derive(Clone)]
    pub struct HoasForall {
        pub name: Name,
        pub domain: Vec<(Name, Type<state::Hoas>)>,
        pub codomain: Arc<ForallHoas<state::Hoas>>,
    }

    impl Eq for HoasForall {}

    impl Display for HoasForall {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    impl Debug for HoasForall {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Forall")
                .field("name", &self.name)
                .field("domain", &self.domain)
                .field("codomain", &"<function>")
                .finish()
        }
    }

    impl PartialEq for HoasForall {
        fn eq(&self, other: &Self) -> bool {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let domain = self
                .domain
                .iter()
                .map(|value| Type::Bound(Bound::Flexible(value.0.clone())))
                .collect::<Vec<_>>();

            self.name == other.name
                && self.domain == other.domain
                && self.codomain.call((domain.clone(),)) == other.codomain.call((domain,))
        }
    }

    impl Hash for HoasForall {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let domain = self
                .domain
                .iter()
                .map(|value| Type::Bound(Bound::Flexible(value.0.clone())))
                .collect();

            self.name.hash(state);
            self.domain.hash(state);
            self.codomain.call((domain,)).hash(state);
        }
    }
}

/// Represents a type dependent function. This is used to
/// represent a type dependent function.
///
/// This is used to represent a type variable that
/// has not been unified with a type.
///
/// NOTE: This is supposed to be used to implement GADT
/// and type families.
pub mod pi {
    use super::*;

    /// Represents a type dependent function. This is used to
    /// represent a type dependent function.
    ///
    /// This is used to represent a type variable that
    /// has not been unified with a type.
    ///
    /// NOTE: This is supposed to be used to implement GADT
    /// and type families.
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub struct QuotedPi {
        pub name: Name,
        pub domain: Box<Type<state::Quoted>>,
        pub codomain: Box<Type<state::Quoted>>,
    }

    impl Display for QuotedPi {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let name = &self.name;
            let domain = &self.domain;
            let codomain = &self.codomain;

            write!(f, "({name} : {domain}) -> {codomain}")
        }
    }

    #[derive(Clone)]
    pub struct HoasPi {
        pub name: Name,
        pub domain: Box<Type<state::Hoas>>,
        pub codomain: Arc<Hoas<state::Hoas>>,
    }

    impl Eq for HoasPi {}

    impl Display for HoasPi {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    impl Debug for HoasPi {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Pi")
                .field("name", &self.name)
                .field("domain", &self.domain)
                .field("codomain", &"<function>")
                .finish()
        }
    }

    impl PartialEq for HoasPi {
        fn eq(&self, other: &Self) -> bool {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(self.name.clone()));

            self.name == other.name
                && self.domain == other.domain
                && self.codomain.call((variable.clone(),)) == other.codomain.call((variable,))
        }
    }

    impl Hash for HoasPi {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(self.name.clone()));

            self.name.hash(state);
            self.domain.hash(state);
            self.codomain.call((variable,)).hash(state);
        }
    }
}

/// Represents a sealed type variable. This is used to represent a type variable
/// that has been sealed.
///
/// This means that the type variable has been unified with a type, and that type
/// is now the only type that the type variable can be.
pub type TypeRep = Type<state::Quoted>;

/// Represents a type. This is the core type of the system. It's a recursive type that can be
/// either a primary type, a constructor, a forall, a pi, or a hole.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<S: state::TypeState> {
    Primary(Primary),
    Constructor(Name),
    App(Box<Type<S>>, Box<Type<S>>),
    Forall(Qual<S, S::Forall>),
    Pi(S::Pi),

    /// Represents an arrow function type. This is used
    /// to represent a function type.
    Fun(S::Fun),

    /// Represents a hole. This is used to represent a type
    /// that should be inferred.
    Hole(S::Hole),

    /// Represents a variable. This is used
    /// to represent a type variable.
    ///
    /// The variables can be either rigid or flexible.
    Bound(Bound),
}

impl<M: state::TypeState> Type<M> {
    pub const ERROR: Self = Self::Primary(Primary::Error);
    pub const UNIT: Self = Self::Primary(Primary::Unit);
    pub const TYPE: Self = Self::Primary(Primary::Type);
    pub const BOOL: Self = Self::Primary(Primary::Bool);
    pub const CHAR: Self = Self::Primary(Primary::Char);
    pub const STRING: Self = Self::Primary(Primary::String);
}

impl<M: state::TypeState> Default for Type<M> {
    /// Returns the default value for a type. This is used to represent a type that is not valid.
    /// It's a sentinel value that is used to represent an error.
    fn default() -> Self {
        Type::Primary(Primary::Error)
    }
}

impl Type<state::Hoas> {
    pub fn spine(self) -> (Vec<Self>, Self) {
        todo!()
        // let mut spine = vec![];
        // let mut last_result = self;

        // while let Ty::Fun(Fun { domain, value, .. }) = last_result {
        //     spine.push(*domain);
        //     last_result = *value;
        // }

        // (spine, last_result)
    }

    /// Create a new pi type. This is used to create a new pi type.
    ///
    /// # Parameters
    ///
    /// - `domain`: The domain of the pi type.
    /// - `value`: The value of the pi type.
    pub fn from_pi<I>(mut parameters: I, ty: Self) -> Self
    where
        I: Iterator<Item = Self>,
    {
        todo!()
        // let Some(first) = parameters.next() else {
        //     return ty
        // };
        // let mut result = Self::Fun(Fun {
        //     domain: first.into(),
        //     value: ty.into(),
        //     phantom: PhantomData,
        // });

        // for parameter in parameters {
        //     result = Ty::Fun(Fun {
        //         domain: Box::new(parameter),
        //         value: Box::new(result),
        //         phantom: PhantomData,
        //     });
        // }

        // result
    }
}

/// Implements display traits tor the types. This is used to display the
/// types, debug or otherwise.
///
/// Also used on diagnostics.
mod display {
    use super::*;

    use std::fmt::Display;

    impl Display for Primary {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Primary::Error => write!(f, "!"),
                Primary::Type => write!(f, "Type"),
                Primary::Unit => write!(f, "()"),
                Primary::Bool => write!(f, "Bool"),
                Primary::String => write!(f, "String"),
                Primary::Char => write!(f, "Char"),
                Primary::Int(size, signed) => {
                    if !signed {
                        write!(f, "U")?;
                    }

                    write!(f, "Int")?;
                    write!(f, "{}", size)
                }
            }
        }
    }

    impl Display for Bound {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Bound::Flexible(name) => write!(f, "{name}"),
                Bound::Index(name, _) => write!(f, "{name}"),
            }
        }
    }

    impl<M: state::TypeState> Display for Type<M> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Type::Primary(primary) => write!(f, "{primary}"),
                Type::Constructor(constructor) => write!(f, "{constructor}"),
                Type::App(app, argument) => write!(f, "({} {})", app, argument),
                Type::Forall(forall) => write!(f, "{forall}"),
                Type::Fun(fun) => write!(f, "{fun}"),
                Type::Pi(pi) => write!(f, "{pi}"),
                Type::Hole(hole) => write!(f, "{hole}"),
                Type::Bound(level) => write!(f, "`{}", level),
            }
        }
    }

    impl<M: state::TypeState> Display for Hole<M> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.kind {
                HoleKind::Error => write!(f, "!"),
                HoleKind::Empty { scope } => write!(f, "?{scope}"),
                HoleKind::Filled(ref ty) => write!(f, "{ty}"),
            }
        }
    }

    impl<M: state::TypeState> Display for HoleRef<M> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.data.borrow())
        }
    }
}

/// Represents a to-be-filled type. This is used to represent a type that is not filled yet.
pub mod holes {
    use std::{ops::Deref, rc::Rc};

    use super::*;

    /// Represents a hole. This is used to represent a hole.
    #[derive(Default, Debug, Clone)]
    pub struct Hole<M: state::TypeState> {
        pub kind: HoleKind<M>,
    }

    impl<M: state::TypeState> Hole<M> {
        pub fn kind(&self) -> &HoleKind<M> {
            &self.kind
        }

        pub fn set_kind(&mut self, kind: HoleKind<M>) {
            self.kind = kind;
        }
    }

    impl<M: state::TypeState> Eq for Hole<M> {}

    impl<M: state::TypeState> PartialEq for Hole<M> {
        fn eq(&self, other: &Self) -> bool {
            self.kind.eq(&other.kind)
        }
    }

    #[derive(Default, Debug, PartialEq, Eq, Clone, Hash)]
    pub enum HoleKind<M: state::TypeState> {
        /// The error type. This is used to represent a type that is not valid. It's a sentinel value
        /// that is used to represent an error.
        #[default]
        Error,

        /// A hole that is not filled yet. This is used to represent a hole that is not filled yet.
        Empty { scope: Level },

        /// A hole that is filled with a type. This is used to represent a hole that is filled with a
        /// type.
        Filled(Type<M>),
    }

    impl<M: state::TypeState> Hash for Hole<M> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.kind.hash(state);
        }
    }

    /// A reference to a [`Hole`]. This is used to represent a reference to a [`Hole`].
    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct HoleRef<M: state::TypeState> {
        pub data: Rc<RefCell<Hole<M>>>,
    }

    impl HoleRef<state::Hoas> {
        pub(crate) fn new(value: Hole<state::Hoas>) -> Self {
            Self {
                data: Rc::new(RefCell::new(value)),
            }
        }

        pub fn kind(&self) -> HoleKind<state::Hoas> {
            self.data.borrow().kind.clone()
        }
    }

    impl Deref for HoleRef<state::Hoas> {
        type Target = Rc<RefCell<Hole<state::Hoas>>>;

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    impl<M: state::TypeState> Eq for HoleRef<M> {}

    impl<M: state::TypeState> PartialEq for HoleRef<M> {
        fn eq(&self, other: &Self) -> bool {
            self.data.borrow().eq(&other.data.borrow())
        }
    }

    impl<M: state::TypeState> Hash for HoleRef<M> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.data.borrow().hash(state);
        }
    }
}

/// Takes a mut type and returns a ready type. This is used to seal a type, and make it ready to
/// be used as [`Send`] and [`Sync`].
pub mod seals {
    use super::{
        forall::{HoasForall, QuotedForall},
        fun::{HoasFun, QuotedFun},
        pi::{HoasPi, QuotedPi},
        *,
    };

    impl Quote for Hole<state::Hoas> {
        type Sealed = Hole<state::Quoted>;

        /// Takes a mut hole and returns a ready hole. This is used to seal a hole, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        fn seal(self) -> Self::Sealed {
            match self.kind {
                HoleKind::Error => Hole {
                    kind: HoleKind::Error,
                },
                HoleKind::Empty { scope } => Hole {
                    kind: HoleKind::Empty { scope },
                },
                HoleKind::Filled(ty) => Hole {
                    kind: HoleKind::Filled(ty.seal()),
                },
            }
        }
    }

    impl Quote for HoasFun {
        type Sealed = QuotedFun;

        fn seal(self) -> Self::Sealed {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(Name {
                definition: empty_definition(),
                name: "_".into(),
            }));

            QuotedFun {
                domain: self.domain.seal().into(),
                value: self.value.call((variable,)).seal().into(),
            }
        }
    }

    impl Quote for HoasForall {
        type Sealed = QuotedForall;

        fn seal(self) -> Self::Sealed {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let domain = self
                .domain
                .iter()
                .map(|value| Type::Bound(Bound::Flexible(value.0.clone())))
                .collect();

            QuotedForall {
                name: self.name,
                domain: self
                    .domain
                    .iter()
                    .cloned()
                    .map(|(name, type_rep)| (name, type_rep.seal().into()))
                    .collect(),
                codomain: self.codomain.call((domain,)).seal().into(),
            }
        }
    }

    impl Quote for HoasPi {
        type Sealed = QuotedPi;

        fn seal(self) -> Self::Sealed {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(Bound::Flexible(self.name));

            QuotedPi {
                name: self.name,
                domain: self.domain.seal().into(),
                codomain: self.codomain.call((variable,)).seal().into(),
            }
        }
    }

    impl Quote for Type<state::Hoas> {
        type Sealed = Type<state::Quoted>;

        /// Takes a mut type and returns a ready type. This is used to seal a type, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        fn seal(self) -> Self::Sealed {
            match self {
                Type::Primary(primary) => Type::Primary(primary),
                Type::Constructor(constructor) => Type::Constructor(constructor),
                Type::Forall(forall) => Type::Forall(forall.seal()),
                Type::Fun(pi) => Type::Fun(pi.seal()),
                Type::Bound(debruijin) => Type::Bound(debruijin),
                Type::Hole(hole) => Type::Hole(hole.data.borrow().clone().seal().into()),
                Type::Pi(pi) => Type::Pi(pi.seal()),
                Type::App(a, b) => Type::App(a.seal().into(), b.seal().into()),
            }
        }
    }
}

/// This trait is sealed and cannot be implemented outside of this crate. This is to prevent
/// users from implementing this trait for their own types.
pub mod state {
    use std::{fmt::Debug, hash::Hash};

    use super::*;

    pub trait TypeKind = Display + PartialEq + Eq + Clone + Hash + Debug;

    /// Represents a mode of a type. This is used to distinguish between different
    /// kinds of modes, such as `built` and `ready`.
    pub trait TypeState: Default + TypeKind {
        type Forall: TypeKind;
        type Fun: TypeKind;
        type Pi: TypeKind;
        type Hole: TypeKind;
    }

    /// Ready is the type of build in types.
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Quoted;

    impl Display for Quoted {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Quoted")
        }
    }

    impl TypeState for Quoted {
        type Hole = Box<crate::type_rep::Hole<Quoted>>;
        type Forall = forall::QuotedForall;
        type Fun = fun::QuotedFun;
        type Pi = pi::QuotedPi;
    }

    /// Mut is the type of mutable types.
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Hoas;

    impl Display for Hoas {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Hoas")
        }
    }

    impl TypeState for Hoas {
        type Hole = HoleRef<Hoas>;
        type Forall = forall::HoasForall;
        type Fun = fun::HoasFun;
        type Pi = pi::HoasPi;
    }
}

fn _assert_sync_send() {
    fn f<T: Sync + Send>() {}

    f::<Type<state::Quoted>>();
    f::<Type<state::Quoted>>();
}
