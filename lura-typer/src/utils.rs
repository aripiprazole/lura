use std::{
  cell::RefCell,
  hash::{BuildHasher, Hash},
  rc::Rc,
};

use fxhash::FxBuildHasher;
use im_rc::HashMap;

#[derive(Clone)]
pub struct LocalDashMap<K, V, S = FxBuildHasher> {
  value: Rc<RefCell<HashMap<K, V, S>>>,
}

impl<K, V, S: BuildHasher + Default> Default for LocalDashMap<K, V, S> {
  fn default() -> Self {
    Self {
      value: Rc::new(RefCell::new(HashMap::default())),
    }
  }
}

impl<K, V, S> LocalDashMap<K, V, S> {
  /// Test whether a hash map is empty.
  ///
  /// Time: O(1)
  #[inline]
  #[must_use]
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Get the size of a hash map.
  ///
  /// Time: O(1)
  #[inline]
  #[must_use]
  pub fn len(&self) -> usize {
    self.value.borrow().len()
  }

  /// Test whether two maps refer to the same content in memory.
  ///
  /// This is true if the two sides are references to the same map,
  /// or if the two maps refer to the same root node.
  ///
  /// This would return true if you're comparing a map to itself, or
  /// if you're comparing a map to a fresh clone of itself.
  ///
  /// Time: O(1)
  pub fn ptr_eq(&self, other: &Self) -> bool {
    self.value.borrow().ptr_eq(&other.value.borrow())
  }

  /// Discard all elements from the map.
  ///
  /// This leaves you with an empty map, and all elements that
  /// were previously inside it are dropped.
  ///
  /// Time: O(n)
  pub fn clear(&self) {
    self.value.borrow_mut().clear()
  }
}

impl<K, V, S> LocalDashMap<K, V, S>
where
  K: Hash + Eq + Clone,
  V: Clone,
  S: BuildHasher,
{
  /// Insert a key/value mapping into a map.
  ///
  /// If the map already has a mapping for the given key, the
  /// previous value is overwritten.
  ///
  /// Time: O(log n)
  #[inline]
  pub fn insert(&self, k: K, v: V) -> Option<V> {
    self.value.borrow_mut().insert(k, v)
  }

  /// Get the value for a key from a hash map.
  ///
  /// Time: O(log n)
  #[must_use]
  pub fn get<BK>(&self, key: &BK) -> Option<V>
  where
    BK: Hash + Eq + ?Sized,
    K: std::borrow::Borrow<BK>,
  {
    self.value.borrow().get(key).cloned()
  }
}
