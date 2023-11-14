//! Wrapper to eyre and miette, it will be used to provide a common error type for those two crates
//! and to provide a common error handling API.

#[doc(hidden)]
pub use eyre as private;
pub use eyre::*;

#[derive(Debug)]
pub enum Report {
  Eyre(eyre::Report),
  Miette(miette::Report),
}

impl<E: std::error::Error + Send + Sync + 'static> From<E> for Report {
  fn from(error: E) -> Self {
    Self::Eyre(error.into())
  }
}

/// Return early with an error.
///
/// This macro is equivalent to `return Err(From::from($err))`.
///
/// # Example
///
/// ```
/// # use lura_eyre::{bail, Result};
/// #
/// # fn has_permission(user: usize, resource: usize) -> bool {
/// #     true
/// # }
/// #
/// # fn main() -> Result<()> {
/// #     let user = 0;
/// #     let resource = 0;
/// #
/// if !has_permission(user, resource) {
///   bail!("permission denied for accessing {}", resource);
/// }
/// #     Ok(())
/// # }
/// ```
///
/// ```
/// # use eyre::{bail, Result};
/// # use thiserror::Error;
/// #
/// # const MAX_DEPTH: usize = 1;
/// #
/// #[derive(Error, Debug)]
/// enum ScienceError {
///     #[error("recursion limit exceeded")]
///     RecursionLimitExceeded,
///     # #[error("...")]
///     # More = (stringify! {
///     ...
///     # }, 1).1,
/// }
///
/// # fn main() -> Result<()> {
/// #     let depth = 0;
/// #     let err: &'static dyn std::error::Error = &ScienceError::RecursionLimitExceeded;
/// #
/// if depth > MAX_DEPTH {
///     bail!(ScienceError::RecursionLimitExceeded);
/// }
/// #     Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! bail {
  ($msg:literal $(,)?) => {
      return Err($crate::Report::Eyre($crate::private::eyre!($msg)));
  };
  ($err:expr $(,)?) => {
      return Err($crate::Report::Eyre($crate::private::eyre!($err)));
  };
  ($fmt:expr, $($arg:tt)*) => {
      return :Err($crate::Report::Eyre($crate::private::eyre!($fmt, $($arg)*)));
  };
}

/// Return early with an error if a condition is not satisfied.
///
/// This macro is equivalent to `if !$cond { return Err(From::from($err)); }`.
///
/// Analogously to `assert!`, `ensure!` takes a condition and exits the function
/// if the condition fails. Unlike `assert!`, `ensure!` returns an `Error`
/// rather than panicking.
///
/// # Example
///
/// ```
/// # use eyre::{ensure, Result};
/// #
/// # fn main() -> Result<()> {
/// #     let user = 0;
/// #
/// ensure!(user == 0, "only user 0 is allowed");
/// #     Ok(())
/// # }
/// ```
///
/// ```
/// # use lura_eyre::{ensure, Result};
/// # use thiserror::Error;
/// #
/// # const MAX_DEPTH: usize = 1;
/// #
/// #[derive(Error, Debug)]
/// enum ScienceError {
///     #[error("recursion limit exceeded")]
///     RecursionLimitExceeded,
///     # #[error("...")]
///     # More = (stringify! {
///     ...
///     # }, 1).1,
/// }
///
/// # fn main() -> Result<()> {
/// #     let depth = 0;
/// #
/// ensure!(depth <= MAX_DEPTH, ScienceError::RecursionLimitExceeded);
/// #     Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! ensure {
  ($cond:expr, $msg:literal $(,)?) => {
    if !$cond {
      return Err($crate::eyre!($msg));
    }
  };
  ($cond:expr, $err:expr $(,)?) => {
    if !$cond {
      return Err($crate::eyre!($err));
    }
  };
  ($cond:expr, $fmt:expr, $($arg:tt)*) => {
    if !$cond {
      return Err($crate::eyre!($fmt, $($arg)*));
    }
  };
}

/// Construct an ad-hoc error from a string.
///
/// This evaluates to an `Error`. It can take either just a string, or a format
/// string with arguments. It also can take any custom type which implements
/// `Debug` and `Display`.
///
/// # Example
///
/// ```
/// # type V = ();
/// #
/// use lura_eyre::{eyre, Result};
///
/// fn lookup(key: &str) -> Result<V> {
///   if key.len() != 16 {
///     return Err(eyre!("key length must be 16 characters, got {:?}", key));
///   }
///
///   // ...
///     # Ok(())
/// }
/// ```
#[macro_export]
macro_rules! eyre {
  ($msg:literal $(,)?) => ({
    $crate::Report::Eyre($crate::private::eyre!($msg))
  });
  ($err:expr $(,)?) => ({
    $crate::Report::Eyre($crate::private::eyre!($err))
  });
  ($fmt:expr, $($arg:tt)*) => ({
    $crate::Report::Eyre($crate::private::eyre!($fmt, $($arg)*))
  });
}

/// type alias for `Result<T, Report>`
///
/// This is a reasonable return type to use throughout your application but also for `fn main`; if
/// you do, failures will be printed along with a backtrace if one was captured.
///
/// `lura_eyre::Result` may be used with one *or* two type parameters.
///
/// ```rust
/// use lura_eyre::Result;
///
/// # const IGNORE: &str = stringify! {
/// fn demo1() -> Result<T> {...}
///            // ^ equivalent to std::result::Result<T, eyre::Report>
///
/// fn demo2() -> Result<T, OtherError> {...}
///            // ^ equivalent to std::result::Result<T, OtherError>
/// # };
/// ```
///
/// # Example
///
/// ```
/// # pub trait Deserialize {}
/// #
/// # mod serde_json {
/// #     use super::Deserialize;
/// #     use std::io;
/// #
/// #     pub fn from_str<T: Deserialize>(json: &str) -> io::Result<T> {
/// #         unimplemented!()
/// #     }
/// # }
/// #
/// # #[derive(Debug)]
/// # struct ClusterMap;
/// #
/// # impl Deserialize for ClusterMap {}
/// #
/// use lura_eyre::Result;
///
/// fn main() -> Result<()> {
///     # return Ok(());
///   let config = std::fs::read_to_string("cluster.json")?;
///   let map: ClusterMap = serde_json::from_str(&config)?;
///   println!("cluster info: {:#?}", map);
///   Ok(())
/// }
/// ```
pub type Result<T, E = Report> = core::result::Result<T, E>;
