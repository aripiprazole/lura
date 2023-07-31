use holes::*;
use std::{cell::RefCell, hash::Hash, marker::PhantomData};

pub type Level = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arrow<K: kinds::ArrowKind, M: modes::TypeMode> {
    pub paramater: Box<Ty<M>>,
    pub ty: Box<Ty<M>>,

    /// Represents the kind of arrow. This is used to distinguish between different
    /// kinds of arrows, such as `forall`, `pi`, and `sigma`.
    pub _phantom: PhantomData<K>,
}

/// Represents a primary type. This is used to represent a type that is not a constructor.
///
/// Can be a sentinel value that is used to represent an error.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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

/// Represents a constructor. This is used to represent a type constructor.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeConstructor {
    pub name: String,
}

pub type Uniq = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyVar {
    Bound(String),
    Skolem(String, Uniq),
}

/// Represents a type. This is the core type of the system. It's a recursive type that can be
/// either a primary type, a constructor, a forall, a pi, or a hole.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty<M: modes::TypeMode> {
    Primary(Primary),
    Constructor(TypeConstructor),
    Forall(Arrow<kinds::Forall, M>),
    Pi(Arrow<kinds::Pi, M>),
    Hole(M::Hole),
    Bound(TyVar),
}

/// Represents a type. This is the core type of the system. It's a recursive type that can be
/// either a primary type, a constructor, a forall, a pi, or a hole.
///
/// This is a tracked type, which means that it can be used as a key in the salsa database. It's
/// a wrapper around [`Ty`], which is not tracked.
#[salsa::tracked]
pub struct ThirTy {
    /// The kind of the type. This is used to distinguish between different kinds of types, such as
    /// `forall`, `pi`, and `sigma`.
    pub kind: Ty<modes::Ready>,
}

impl<M: modes::TypeMode> Default for Ty<M> {
    /// Returns the default value for a type. This is used to represent a type that is not valid.
    /// It's a sentinel value that is used to represent an error.
    fn default() -> Self {
        Ty::Primary(Primary::Error)
    }
}

impl Ty<modes::Mut> {
    pub fn from_pi<I>(mut parameters: I, ty: Self) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let first = parameters.next().unwrap();
        let mut result = Self::Pi(Arrow {
            paramater: first.into(),
            ty: ty.into(),
            _phantom: PhantomData,
        });

        for parameter in parameters {
            result = Ty::Pi(Arrow {
                paramater: Box::new(parameter),
                ty: Box::new(result),
                _phantom: PhantomData,
            });
        }

        result
    }
}

/// Represents a to-be-filled type. This is used to represent a type that is not filled yet.
pub mod holes {
    use super::*;

    /// Represents a hole. This is used to represent a hole.
    #[derive(Default, Debug, Clone)]
    pub struct Hole<M: modes::TypeMode> {
        pub kind: HoleKind<M>,
    }

    impl<M: modes::TypeMode + PartialEq> Eq for Hole<M> {}

    impl<M: modes::TypeMode + PartialEq> PartialEq for Hole<M> {
        fn eq(&self, other: &Self) -> bool {
            self.kind.eq(&other.kind)
        }
    }

    #[derive(Default, Debug, PartialEq, Eq, Clone, Hash)]
    pub enum HoleKind<M: modes::TypeMode> {
        /// The error type. This is used to represent a type that is not valid. It's a sentinel value
        /// that is used to represent an error.
        #[default]
        Error,

        /// A hole that is not filled yet. This is used to represent a hole that is not filled yet.
        Empty { scope: Level },

        /// A hole that is filled with a type. This is used to represent a hole that is filled with a
        /// type.
        Filled(Ty<M>),
    }

    impl<M: modes::TypeMode + Hash> Hash for Hole<M> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.kind.hash(state);
        }
    }

    /// A reference to a [`Hole`]. This is used to represent a reference to a [`Hole`].
    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct HoleRef<M: modes::TypeMode> {
        pub data: RefCell<Hole<M>>,
    }

    impl<M: modes::TypeMode + PartialEq> Eq for HoleRef<M> {}

    impl<M: modes::TypeMode + PartialEq> PartialEq for HoleRef<M> {
        fn eq(&self, other: &Self) -> bool {
            self.data.borrow().eq(&other.data.borrow())
        }
    }

    impl<M: modes::TypeMode + PartialEq + Hash> Hash for HoleRef<M> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.data.borrow().hash(state);
        }
    }
}

/// Takes a mut type and returns a ready type. This is used to seal a type, and make it ready to
/// be used as [`Send`] and [`Sync`].
pub mod seals {
    use super::*;

    impl Hole<modes::Mut> {
        /// Takes a mut hole and returns a ready hole. This is used to seal a hole, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        pub fn seal(self) -> Hole<modes::Ready> {
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

    impl<K: kinds::ArrowKind> Arrow<K, modes::Mut> {
        /// Takes a mut arrow and returns a ready arrow. This is used to seal an arrow, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        pub fn seal(self) -> Arrow<K, modes::Ready> {
            Arrow {
                paramater: self.paramater.seal().into(),
                ty: self.ty.seal().into(),
                _phantom: PhantomData,
            }
        }
    }

    impl Ty<modes::Mut> {
        /// Takes a mut type and returns a ready type. This is used to seal a type, and make it
        /// ready to be used as [`Send`] and [`Sync`].
        pub fn seal(self) -> Ty<modes::Ready> {
            match self {
                Ty::Primary(primary) => Ty::Primary(primary),
                Ty::Constructor(constructor) => Ty::Constructor(constructor),
                Ty::Forall(forall) => Ty::Forall(forall.seal()),
                Ty::Pi(pi) => Ty::Pi(pi.seal()),
                Ty::Bound(debruijin) => Ty::Bound(debruijin),
                Ty::Hole(hole) => Ty::Hole(hole.data.borrow().clone().seal().into()),
            }
        }
    }
}

/// This trait is sealed and cannot be implemented outside of this crate. This is to prevent
/// users from implementing this trait for their own types.
pub mod modes {
    use std::{fmt::Debug, hash::Hash, rc::Rc};

    use super::HoleRef;

    /// Represents a mode of a type. This is used to distinguish between different
    /// kinds of modes, such as `built` and `ready`.
    pub trait TypeMode {
        type Hole: Debug + PartialEq + Eq + Clone + Hash;
    }

    /// Ready is the type of build in types.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Ready;

    impl TypeMode for Ready {
        type Hole = Box<crate::ty::Hole<Ready>>;
    }

    /// Mut is the type of mutable types.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Mut;

    impl TypeMode for Mut {
        type Hole = Rc<HoleRef<Mut>>;
    }
}

/// This trait is sealed and cannot be implemented outside of this crate. This is to prevent
/// users from implementing this trait for their own types.
mod kinds {
    /// Represents a kind of arrow for a type. This is used to distinguish between different
    /// kinds of arrows, such as `forall`, `pi`, and `sigma`.
    ///
    /// This trait is sealed and cannot be implemented outside of this crate. This is to prevent
    /// users from implementing this trait for their own types.
    pub trait ArrowKind {}

    /// Forall is the type of polymorphic generalization.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Forall;

    /// Sigma is the type of dependent tuples.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Sigma;

    /// Pi is the type of functions.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Pi;

    impl ArrowKind for Forall {}
    impl ArrowKind for Sigma {}
    impl ArrowKind for Pi {}
}

fn _assert_sync_send() {
    fn f<T: Sync + Send>() {}

    f::<Ty<modes::Ready>>();
    f::<Ty<modes::Ready>>();
}
