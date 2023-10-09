use fxhash::FxBuildHasher;
use im_rc::HashSet;

use crate::type_rep::{
  holes::{HoleKind, HoleRef},
  pi, Type,
};

pub type Fvs = im_rc::Vector<String>;

pub trait Ftv {
  /// Collects all variables that are free in the type.
  ///
  /// Free variables, are variables that are not bound by a context.
  /// For example: `Eq a`, a is a free variable.
  fn ftv(&self) -> Fvs;
}

impl Ftv for Type {
  fn ftv(&self) -> Fvs {
    todo!()
    // match self {
    //   Type::Universe => Default::default(),
    //   Type::Primary(_) => Default::default(),
    //   Type::Constructor(_) => Default::default(),
    //   Type::Forall(forall) => forall.ftv(),
    //   Type::Pi(pi) => pi.ftv(),
    //   Type::Stuck(stuck) => stuck.ftv(),
    //   Type::Hole(hole) => hole.ftv(),
    //   Type::App(callee, value) => remove_duplicates(callee.ftv().into_iter().chain(value.ftv()).collect()),
    //   Type::Bound(bound) => {
    //     // NOTE: it doesn't use the [`hashset!`] macro, because
    //     // it uses the default hasher, which is not compatible
    //     // with the [`FxBuildHasher`].
    //     let mut ftv = Fvs::default();
    //     ftv.push_back(bound.clone());
    //     ftv
    //   }
    // }
  }
}

impl Ftv for pi::Pi {
  fn ftv(&self) -> Fvs {
    todo!()
    // // NOTE: it doesn't use the [`hashset!`] macro, because
    // // it uses the default hasher, which is not compatible
    // // with the [`FxBuildHasher`].
    // let ftv = self
    //   .domain
    //   .ftv()
    //   .into_iter()
    //   .chain(self.codomain(Type::Bound(Bound::Hole)).ftv())
    //   .collect();
    //
    // remove_duplicates(ftv)
  }
}

/// Removes all duplicates from the vector. Curating the list to
/// only contain unique values.
fn remove_duplicates(ftv: Fvs) -> Fvs {
  let mut new_ftv = Fvs::default();
  let mut seen: im_rc::HashSet<_, FxBuildHasher> = HashSet::default();

  for value in ftv {
    if !seen.contains(&value) {
      new_ftv.push_back(value.clone());
      seen.insert(value);
    }
  }

  new_ftv
}
