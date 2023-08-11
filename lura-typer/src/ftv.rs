use crate::type_rep::holes::{HoleKind, HoleRef};
use crate::type_rep::{forall, pi, state, stuck, Bound, Type};
use fxhash::FxBuildHasher;
use im_rc::HashSet;

/// Represents a variable that isn't bound to any context.
pub type Fv = Bound;

pub type Fvs = im_rc::Vector<Fv>;

pub trait Ftv {
    /// Collects all variables that are free in the type.
    ///
    /// Free variables, are variables that are not bound by a context.
    /// For example: `Eq a`, a is a free variable.
    fn ftv(&self) -> Fvs;
}

impl Ftv for Type<state::Hoas> {
    fn ftv(&self) -> Fvs {
        match self {
            Type::Type => Default::default(),
            Type::Primary(_) => Default::default(),
            Type::Constructor(_) => Default::default(),
            Type::Forall(forall) => forall.ftv(),
            Type::Pi(pi) => pi.ftv(),
            Type::Stuck(stuck) => stuck.ftv(),
            Type::Hole(hole) => hole.ftv(),
            Type::App(callee, value) => {
                remove_duplicates(callee.ftv().into_iter().chain(value.ftv()).collect())
            }
            Type::Bound(bound) => {
                // NOTE: it doesn't use the [`hashset!`] macro, because
                // it uses the default hasher, which is not compatible
                // with the [`FxBuildHasher`].
                let mut ftv = Fvs::default();
                ftv.push_back(bound.clone());
                ftv
            }
        }
    }
}

impl Ftv for HoleRef<state::Hoas> {
    fn ftv(&self) -> Fvs {
        match self.kind() {
            HoleKind::Error => Default::default(),
            HoleKind::Empty { .. } => Default::default(),
            HoleKind::Filled(value) => value.ftv(),
        }
    }
}

impl Ftv for stuck::Stuck<state::Hoas> {
    fn ftv(&self) -> Fvs {
        let ftv = self
            .spine
            .iter()
            .flat_map(|value| value.ftv())
            .chain(self.base.ftv())
            .collect();

        remove_duplicates(ftv)
    }
}

impl Ftv for forall::HoasForall {
    fn ftv(&self) -> Fvs {
        let domain = self.domain.iter().map(|_| Type::Bound(Bound::Hole));

        // NOTE: it doesn't use the [`hashset!`] macro, because
        // it uses the default hasher, which is not compatible
        // with the [`FxBuildHasher`].
        let ftv = self
            .domain
            .iter()
            .flat_map(|(_, kind)| kind.ftv())
            .chain(self.instantiate(domain.collect()).ftv())
            .collect();

        remove_duplicates(ftv)
    }
}

impl Ftv for pi::HoasPi {
    fn ftv(&self) -> Fvs {
        // NOTE: it doesn't use the [`hashset!`] macro, because
        // it uses the default hasher, which is not compatible
        // with the [`FxBuildHasher`].
        let ftv = self
            .domain
            .ftv()
            .into_iter()
            .chain(self.codomain(Type::Bound(Bound::Hole)).ftv())
            .collect();

        remove_duplicates(ftv)
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
