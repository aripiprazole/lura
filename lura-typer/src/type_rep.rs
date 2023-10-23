use std::{
  fmt::{Debug, Display, Formatter},
  hash::Hash,
};

use holes::*;
use lura_hir::solver::Definition;

use crate::type_rep::pi::Pi;

pub type Level = usize;
pub type Ix = usize;

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
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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
  Constructor(Name),
}

impl Primary {
  pub const I16: Self = Self::Int(16, true);
  pub const I32: Self = Self::Int(32, true);
  pub const I64: Self = Self::Int(64, true);
  pub const I8: Self = Self::Int(8, true);
  pub const U16: Self = Self::Int(16, false);
  pub const U32: Self = Self::Int(32, false);
  pub const U64: Self = Self::Int(64, false);
  pub const U8: Self = Self::Int(8, false);
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
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self.name, f)
  }
}

impl Display for Name {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Display::fmt(&self.name, f)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
  Universe,
  Primary(Primary),
  Ix(usize),
  Lam {
    name: Option<Name>,
    value: Box<Term>,
  },
  Forall {
    name: Option<Name>,
    domain: Box<Term>,
    codomain: Box<Term>,
  },
  Pi {
    name: Option<Name>,
    domain: Box<Term>,
    codomain: Box<Term>,
  },
  Var(Name),

  /// Represents a stuck type. This is used to represent a type that is stuck.
  ///
  /// A stuck type is a type that is not fully evaluated, and can't be evaluated
  Rigid(Ix, Vec<Type>),

  /// Represents a stuck type. This is used to represent a type that is stuck.
  ///
  /// A stuck type is a type that is not fully evaluated, and can't be evaluated
  Flexible(holes::HoleRef, Vec<Type>),
}

/// Represents a type. This is the core type of the system. It's a recursive type that can be
/// either a primary type, a constructor, a forall, a pi, or a hole.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Universe,
  Primary(Primary),
  Ix(Ix),
  Lam(closure::Closure),
  Forall(Pi),
  Pi(Pi),

  /// Represents a stuck type. This is used to represent a type that is stuck.
  ///
  /// A stuck type is a type that is not fully evaluated, and can't be evaluated
  Rigid(Level, Vec<Type>),

  /// Represents a stuck type. This is used to represent a type that is stuck.
  ///
  /// A stuck type is a type that is not fully evaluated, and can't be evaluated
  Flexible(HoleRef, Vec<Type>),
}

impl Type {
  /// Force the type to be evaluated.
  ///
  /// This is used to force the type to be evaluated.
  pub fn force(self) -> Type {
    match self.clone() {
      Type::Flexible(ref hole, _) => match hole.kind() {
        HoleKind::Filled(value) => value.force(),
        _ => self,
      },
      _ => self,
    }
  }

  /// Checks if the type is an empty hole
  pub fn is_unbound(&self) -> bool {
    let Type::Flexible(hole, _) = self else {
      return false;
    };

    match hole.kind() {
      HoleKind::Error => false,
      HoleKind::Empty { .. } => true,
      HoleKind::Filled(_) => false,
    }
  }
}

impl Quote for Type {
  type Sealed = Term;

  fn seal(self) -> Self::Sealed {
    todo!()
  }
}

/// Add debug implementation for better data presentation when
/// debugging the type system
mod debug {
  use super::*;

  impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
      match self {
        Type::Universe => write!(f, "Type"),
        Type::Primary(arg0) => write!(f, "{arg0:?}"),
        Type::Ix(arg0) => write!(f, "Ix({arg0:?})"),
        Type::Lam(arg0) => write!(f, "Lam({arg0:?})"),
        Type::Forall(arg0) => write!(f, "Forall({arg0:?})"),
        Type::Pi(arg0) => write!(f, "Pi({arg0:?})"),
        Type::Rigid(arg0, arg1) => write!(f, "Rigid({arg0:?}, {arg1:?})"),
        Type::Flexible(arg0, arg1) => write!(f, "Flexible({arg0:?}, {arg1:?})"),
      }
    }
  }
}

impl Type {
  pub const BOOL: Self = Self::Primary(Primary::Bool);
  pub const CHAR: Self = Self::Primary(Primary::Char);
  pub const ERROR: Self = Self::Primary(Primary::Error);
  pub const STRING: Self = Self::Primary(Primary::String);
  pub const TYPE: Self = Self::Universe;
  pub const UNIT: Self = Self::Primary(Primary::Unit);
}

impl Default for Type {
  /// Returns the default value for a type. This is used to represent a type that is not valid.
  /// It's a sentinel value that is used to represent an error.
  fn default() -> Self {
    Type::Primary(Primary::Error)
  }
}

/// Defines the type of a hole. This is used to define the type of a hole.
pub mod closure {
  use super::*;

  /// Type level HOAS closure
  #[derive(Debug, PartialEq, Eq, Hash, Clone)]
  pub struct Closure {
    pub level: Level,
    pub environment: Vec<Type>,
    pub body: Term,
  }

  impl Closure {
    /// Apply the closure to an argument. This is used to apply the closure to an argument.
    pub fn apply(mut self, argument: Type) -> Type {
      self.environment.push(argument);
      self.body.eval(self.environment)
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
  #[derive(Clone)]
  pub struct Pi {
    pub name: Option<Name>,
    pub domain: Box<Type>,
    pub codomain: closure::Closure,
  }

  impl Eq for Pi {}

  impl Display for Pi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      Debug::fmt(self, f)
    }
  }

  impl Debug for Pi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.debug_struct("Pi")
        .field("name", &self.name)
        .field("domain", &self.domain)
        .field("codomain", &self.codomain)
        .finish()
    }
  }

  impl PartialEq for Pi {
    fn eq(&self, other: &Self) -> bool {
      self.name == other.name && self.domain == other.domain && self.codomain == other.codomain
    }
  }

  impl Hash for Pi {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      self.name.hash(state);
      self.domain.hash(state);
      self.codomain.hash(state);
    }
  }
}

/// Implements display traits tor the types. This is used to display the
/// types, debug or otherwise.
///
/// Also used on diagnostics.
mod display {
  use std::fmt::Display;

  use super::*;

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
        Primary::Constructor(constructor) => write!(f, "{constructor}"),
      }
    }
  }

  impl Display for Type {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      todo!()
    }
  }

  impl Display for Term {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      todo!()
    }
  }

  impl Display for Hole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match &self.kind {
        HoleKind::Error => write!(f, "!"),
        HoleKind::Empty { scope } => write!(f, "?{scope}"),
        HoleKind::Filled(ty) => write!(f, "{ty}"),
      }
    }
  }

  impl Display for HoleRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}", self.data.read().unwrap())
    }
  }
}

/// Represents a to-be-filled type. This is used to represent a type that is not filled yet.
pub mod holes {
  use std::{
    ops::Deref,
    sync::{Arc, RwLock},
  };

  use super::*;

  /// Represents a hole. This is used to represent a hole.
  #[derive(Default, Clone)]
  pub struct Hole {
    pub kind: HoleKind,
  }

  impl Debug for Hole {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
      write!(f, "{:?}", self.kind)
    }
  }

  impl Hole {
    pub fn kind(&self) -> &HoleKind {
      &self.kind
    }

    pub fn set_kind(&mut self, kind: HoleKind) {
      self.kind = kind;
    }
  }

  impl Eq for Hole {}

  impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
      self.kind.eq(&other.kind)
    }
  }

  #[derive(Default, Debug, PartialEq, Eq, Clone, Hash)]
  pub enum HoleKind {
    /// The error type. This is used to represent a type that is not valid. It's a sentinel value
    /// that is used to represent an error.
    #[default]
    Error,

    /// A hole that is not filled yet. This is used to represent a hole that is not filled yet.
    Empty { scope: Level },

    /// A hole that is filled with a type. This is used to represent a hole that is filled with a
    /// type.
    Filled(Type),
  }

  impl Hash for Hole {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      self.kind.hash(state);
    }
  }

  /// A reference to a [`Hole`]. This is used to represent a reference to a [`Hole`].
  #[derive(Debug, Clone)]
  #[repr(transparent)]
  pub struct HoleRef {
    pub data: Arc<RwLock<Hole>>,
  }

  impl HoleRef {
    pub fn new(value: Hole) -> Self {
      Self {
        data: Arc::new(RwLock::new(value)),
      }
    }

    /// Clones out the kind of a hole reference
    /// Get the kind of the hole.
    pub fn kind(&self) -> HoleKind {
      self.data.read().unwrap().kind().clone()
    }

    /// Set the kind of the hole.
    pub fn set_kind(&self, kind: HoleKind) {
      self.data.write().unwrap().kind = kind;
    }
  }

  impl Deref for HoleRef {
    type Target = Arc<RwLock<Hole>>;

    fn deref(&self) -> &Self::Target {
      &self.data
    }
  }

  impl Eq for HoleRef {}

  impl PartialEq for HoleRef {
    fn eq(&self, other: &Self) -> bool {
      self.data.read().unwrap().eq(&other.data.read().unwrap())
    }
  }

  impl Hash for HoleRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      self.data.read().unwrap().hash(state);
    }
  }
}
