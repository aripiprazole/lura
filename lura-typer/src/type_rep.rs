use holes::*;
use lura_hir::solver::Definition;
use std::fmt::Formatter;
use std::rc::Rc;
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::adhoc::Qual;
use crate::type_rep::pi::HoasPi;

pub type Level = usize;

/// Represents a type-level function. This is used
/// to represent a type-level function.
pub type Hoas<S> = dyn Fn(Type<S>) -> Type<S>;

/// Represents a type that could be sealed to erase mutability.
pub trait Quote {
    /// The sealed type.
    type Sealed;

    /// Quote the type.
    fn quote(&self) -> Self::Sealed
    where
        Self: Clone,
    {
        self.clone().seal()
    }

    /// Seal the type.
    fn seal(self) -> Self::Sealed;
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

/// Represents a type variable. This is used to represent a type variable.
///
/// It can contain a debug name string, and a definition, that
/// is what is being defined by the type variable.
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
    Hole,
    Flexible(Name),
    Index(Name, Level),
}

/// Represents a sealed type variable. This is used to represent a type variable
/// that has been sealed.
///
/// This means that the type variable has been unified with a type, and that type
/// is now the only type that the type variable can be.
pub type TypeRep = Type<state::Quoted>;

/// Represents a type. This is the core type of the system. It's a recursive type that can be
/// either a primary type, a constructor, a forall, a pi, or a hole.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type<S: state::TypeState> {
    Type,
    Primary(Primary),
    Constructor(Name),
    App(Box<Type<S>>, Box<Type<S>>),
    Forall(S::Forall),
    Pi(S::Pi),

    /// Represents a stuck type. This is used to represent a type that is stuck.
    ///
    /// A stuck type is a type that is not fully evaluated, and can't be evaluated
    Stuck(stuck::Stuck<S>),

    /// Represents a hole. This is used to represent a type
    /// that should be inferred.
    Hole(S::Hole),

    /// Represents a variable. This is used
    /// to represent a type variable.
    ///
    /// The variables can be either rigid or flexible.
    Bound(Bound),
}

impl Type<state::Hoas> {
    pub(crate) fn spine(self) -> (Vec<Self>, Self) {
        let mut spine = Vec::new();
        let mut current = self.force();

        // Unwraps the forall types
        if let Type::Forall(forall) = current {
            let domain = forall
                .domain
                .iter()
                .map(|_| Type::Bound(Bound::Hole))
                .collect();

            current = forall.instantiate(domain).data.force();
        }

        while let Type::Pi(ref pi) = current {
            let parameter = (*pi.domain).clone();
            current = pi.codomain(Type::Bound(Bound::Hole));
            spine.push(parameter);
        }

        (spine, current)
    }

    /// Force the type to be evaluated.
    ///
    /// This is used to force the type to be evaluated.
    pub(crate) fn force(self) -> Type<state::Hoas> {
        match self {
            Type::Hole(ref hole) => match hole.kind() {
                HoleKind::Error => Type::Hole(HoleRef::new(Hole {
                    kind: HoleKind::Error,
                })),
                HoleKind::Empty { scope } => Type::Hole(HoleRef::new(Hole {
                    kind: HoleKind::Empty { scope },
                })),
                HoleKind::Filled(value) => value.force(),
            },
            _ => self,
        }
    }

    /// Checks if the type is an empty hole
    pub(crate) fn is_unbound(&self) -> bool {
        let Type::Hole(hole) = self else { return false };

        match hole.kind() {
            HoleKind::Error => false,
            HoleKind::Empty { .. } => true,
            HoleKind::Filled(_) => false,
        }
    }
}

/// Add debug implementation for better data presentation when
/// debugging the type system
mod debug {
    use super::*;

    impl<S: state::TypeState> Debug for Type<S> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Type::Type => write!(f, "Type"),
                Type::Primary(primary) => write!(f, "{primary:?}"),
                Type::Constructor(constructor) => write!(f, "Constructor({constructor:?})"),
                Type::App(callee, value) => write!(f, "App({callee:?}, {value:?})"),
                Type::Forall(forall) => Debug::fmt(forall, f),
                Type::Pi(pi) => Debug::fmt(pi, f),
                Type::Hole(hole) => Debug::fmt(hole, f),
                Type::Bound(bound) => Debug::fmt(bound, f),
                Type::Stuck(stuck) => Debug::fmt(stuck, f),
            }
        }
    }
}

impl<S: state::TypeState> Type<S> {
    pub const TYPE: Self = Self::Type;
    pub const ERROR: Self = Self::Primary(Primary::Error);
    pub const UNIT: Self = Self::Primary(Primary::Unit);
    pub const BOOL: Self = Self::Primary(Primary::Bool);
    pub const CHAR: Self = Self::Primary(Primary::Char);
    pub const STRING: Self = Self::Primary(Primary::String);
}

impl<S: state::TypeState> Default for Type<S> {
    /// Returns the default value for a type. This is used to represent a type that is not valid.
    /// It's a sentinel value that is used to represent an error.
    fn default() -> Self {
        Type::Primary(Primary::Error)
    }
}

/// Represents a stuck computation, like a -> List a, should return
/// a List of a, and not just a list.
pub mod stuck {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub struct Stuck<S: state::TypeState> {
        pub base: Box<Type<S>>,
        pub spine: Vec<Type<S>>,
    }

    impl<S: state::TypeState> Stuck<S> {
        /// Adds new arguments to the spine of the stuck type.
        pub fn extend_spine(&self, value: Type<S>) -> Self {
            Self {
                base: self.base.clone(),
                spine: self.spine.iter().cloned().chain(Some(value)).collect(),
            }
        }
    }

    impl Quote for Stuck<state::Hoas> {
        type Sealed = Stuck<state::Quoted>;

        fn seal(self) -> Self::Sealed {
            Stuck {
                base: self.base.seal().into(),
                spine: self.spine.into_iter().map(Quote::seal).collect(),
            }
        }
    }

    impl<S: state::TypeState> Display for Stuck<S>
    where
        S: Debug,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "({}", self.base)?;
            for arg in &self.spine {
                write!(f, " {}", arg)?;
            }
            write!(f, ")")
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
pub mod forall {
    use super::*;
    use std::rc::Rc;

    /// Represents a type-level function. This is used
    /// to represent a type-level function.
    pub type ForallHoas<S> = dyn Fn(Vec<Type<S>>) -> Qual<S, Type<S>>;

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
        pub domain: Vec<(Name, Type<state::Quoted>)>,
        pub codomain: Box<Qual<state::Quoted, Type<state::Quoted>>>,
    }

    impl Display for QuotedForall {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "∀")?;
            for (name, type_rep) in &self.domain {
                write!(f, " ({name} : {type_rep})")?;
            }
            write!(f, ". {}", self.codomain)
        }
    }

    #[derive(Clone)]
    pub struct HoasForall {
        pub domain: Vec<(Name, Type<state::Hoas>)>,
        pub codomain: Rc<ForallHoas<state::Hoas>>,
    }

    /// Represents a type-level function. This is used
    /// to represent a type-level function.
    type Types = Vec<Type<state::Hoas>>;

    impl HoasForall {
        // Instantiates a forall type with the given types.
        //
        // This is used to instantiate a forall type with the given types.
        pub fn instantiate(&self, types: Types) -> Qual<state::Hoas, Type<state::Hoas>> {
            (self.codomain)(types)
        }
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

            self.domain == other.domain
                && self.instantiate(domain.clone()) == other.instantiate(domain)
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

            self.domain.hash(state);
            self.instantiate(domain).hash(state);
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
    use std::rc::Rc;

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
        pub name: Option<Name>,
        pub domain: Box<Type<state::Quoted>>,
        pub codomain: Box<Type<state::Quoted>>,
    }

    impl Display for QuotedPi {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let name = match &self.name {
                Some(name) => format!("({name} : {})", self.domain),
                None => format!("{}", self.domain),
            };
            let codomain = &self.codomain;

            write!(f, "{name} -> {codomain}")
        }
    }

    #[derive(Clone)]
    pub struct HoasPi {
        pub name: Option<Name>,
        pub domain: Box<Type<state::Hoas>>,
        pub codomain: Rc<Hoas<state::Hoas>>,
    }

    impl HoasPi {
        pub fn codomain(&self, ty: Type<state::Hoas>) -> Type<state::Hoas> {
            (self.codomain)(ty)
        }
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
            let variable = Type::Bound(match self.name {
                None => Bound::Hole,
                Some(ref name) => Bound::Flexible(name.clone()),
            });

            self.name == other.name
                && self.domain == other.domain
                && self.codomain(variable.clone()) == other.codomain(variable)
        }
    }

    impl Hash for HoasPi {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            // Create a dummy value to use as the variable.
            //
            // This is needed because we need to create a
            // variable that is not bound to anything.
            let variable = Type::Bound(match self.name {
                None => Bound::Hole,
                Some(ref name) => Bound::Flexible(name.clone()),
            });

            self.name.hash(state);
            self.domain.hash(state);
            self.codomain(variable).hash(state);
        }
    }
}

impl Type<state::Hoas> {
    /// Create a new pi type. This is used to create a new pi type.
    ///
    /// # Parameters
    ///
    /// - `domain`: The domain of the pi type.
    /// - `value`: The value of the pi type.
    pub fn from_pi<I>(mut parameters: I, return_type: Self) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let Some(first) = parameters.next() else {
            return return_type;
        };

        let mut result = Self::Pi(HoasPi {
            name: None,
            domain: first.into(),
            codomain: Rc::new(move |_| return_type.clone()),
        });

        for domain in parameters {
            result = Type::Pi(HoasPi {
                name: None,
                domain: domain.into(),
                codomain: Rc::new(move |_| result.clone()),
            });
        }

        result
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
                Bound::Hole => write!(f, "_"),
                Bound::Flexible(name) => write!(f, "{name}"),
                Bound::Index(name, _) => write!(f, "{name}"),
            }
        }
    }

    impl<M: state::TypeState> Display for Type<M> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Type::Type => write!(f, "*"),
                Type::Primary(primary) => write!(f, "{primary}"),
                Type::Constructor(constructor) => write!(f, "{constructor}"),
                Type::App(app, argument) => write!(f, "({} {})", app, argument),
                Type::Forall(forall) => write!(f, "{forall}"),
                Type::Pi(pi) => write!(f, "{pi}"),
                Type::Hole(hole) => write!(f, "{hole}"),
                Type::Stuck(stuck) => write!(f, "{stuck}"),
                Type::Bound(Bound::Hole) => write!(f, "_"),
                Type::Bound(level) => write!(f, "'{}", level),
            }
        }
    }

    impl<M: state::TypeState> Display for Hole<M> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.kind {
                HoleKind::Error => write!(f, "!"),
                HoleKind::Empty { scope } => write!(f, "?{scope}"),
                HoleKind::Filled(ty) => write!(f, "{ty}"),
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
    #[derive(Default, Clone)]
    pub struct Hole<S: state::TypeState> {
        pub kind: HoleKind<S>,
    }

    impl<S: state::TypeState> Debug for Hole<S>
    where
        S: Debug,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.kind)
        }
    }

    impl<S: state::TypeState> Hole<S> {
        pub fn kind(&self) -> &HoleKind<S> {
            &self.kind
        }

        pub fn set_kind(&mut self, kind: HoleKind<S>) {
            self.kind = kind;
        }
    }

    impl<S: state::TypeState> Eq for Hole<S> {}

    impl<S: state::TypeState> PartialEq for Hole<S> {
        fn eq(&self, other: &Self) -> bool {
            self.kind.eq(&other.kind)
        }
    }

    #[derive(Default, Debug, PartialEq, Eq, Clone, Hash)]
    pub enum HoleKind<S: state::TypeState> {
        /// The error type. This is used to represent a type that is not valid. It's a sentinel value
        /// that is used to represent an error.
        #[default]
        Error,

        /// A hole that is not filled yet. This is used to represent a hole that is not filled yet.
        Empty { scope: Level },

        /// A hole that is filled with a type. This is used to represent a hole that is filled with a
        /// type.
        Filled(Type<S>),
    }

    impl<M: state::TypeState> Hash for Hole<M> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.kind.hash(state);
        }
    }

    /// A reference to a [`Hole`]. This is used to represent a reference to a [`Hole`].
    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct HoleRef<S: state::TypeState> {
        pub data: Rc<RefCell<Hole<S>>>,
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
                domain: self
                    .domain
                    .iter()
                    .cloned()
                    .map(|(name, type_rep)| (name, type_rep.seal().into()))
                    .collect(),
                codomain: self.instantiate(domain).seal().into(),
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
            let variable = Type::Bound(match self.name {
                None => Bound::Hole,
                Some(ref name) => Bound::Flexible(name.clone()),
            });

            QuotedPi {
                name: self.name.clone(),
                domain: self.domain.clone().seal().into(),
                codomain: self.codomain(variable).seal().into(),
            }
        }
    }

    impl Quote for Type<state::Hoas> {
        type Sealed = Type<state::Quoted>;

        /// Takes a mut type and returns a ready type. This is used to seal a type, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        fn seal(self) -> Self::Sealed {
            match self {
                Type::Type => Type::Type,
                Type::Primary(primary) => Type::Primary(primary),
                Type::Constructor(constructor) => Type::Constructor(constructor),
                Type::Forall(forall) => Type::Forall(forall.seal()),
                Type::Bound(debruijin) => Type::Bound(debruijin),
                Type::Stuck(stuck) => Type::Stuck(stuck.seal()),
                Type::Hole(hole) => Type::Hole(hole.data.borrow().clone().seal().into()),
                Type::Pi(pi) => Type::Pi(pi.seal()),
                Type::App(a, b) => Type::App(a.seal().into(), b.seal().into()),
            }
        }
    }
}

/// This trait is sealed and cannot be implemented outside of this crate. This is to prevent
/// users from implementing this trait for their own types.
///
/// Implements a state for a type. This is used to distinguish between different
/// kinds of types, such as `hoas` and `quoted`.
///
/// Quoted types are used to print types. They are the serialized version of a type. And
/// the only way to create a quoted type is to quote a type.
///
/// Now, the HOAS type is used to represent a type that is not ready to be printed. This
/// is because it contains holes, which are not ready to be printed. This is because holes
/// are not ready to be printed, and they need to be filled before they can be printed.
pub mod state {
    use std::{fmt::Debug, hash::Hash};

    use super::*;

    pub trait TypeKind = Display + PartialEq + Eq + Clone + Hash + Debug;

    /// Represents a mode of a type. This is used to distinguish between different
    /// kinds of modes, such as `hoas` and `quoted`.
    pub trait TypeState: Default + TypeKind {
        /// The type of a forall type.
        type Forall: TypeKind;

        /// The type of a pi type.
        type Pi: TypeKind;

        /// The type of a hole type.
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
        type Hole = Box<Hole<Quoted>>;
        type Forall = forall::QuotedForall;
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
        type Pi = pi::HoasPi;
    }
}

fn _assert_sync_send() {
    fn f<T: Sync + Send>() {}

    f::<Type<state::Quoted>>();
    f::<Type<state::Quoted>>();
}
