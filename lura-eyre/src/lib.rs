//! Wrapper to eyre and miette, it will be used to provide a common error type for those two crates
//! and to provide a common error handling API.

use std::fmt::Display;

#[doc(hidden)]
pub use eyre as private;
pub use eyre::{set_hook, DefaultHandler};
pub use WrapErr as Context;

#[derive(Debug)]
pub enum Report {
  Eyre(eyre::Report),
  Miette(miette::Report),
}

impl Display for Report {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Report::Eyre(eyre) => Display::fmt(eyre, f),
      Report::Miette(miette) => Display::fmt(miette, f),
    }
  }
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
/// # use lura_eyre::{bail, Result};
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
      return Err($crate::Report::Eyre($crate::private::eyre!($fmt, $($arg)*)));
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
/// # use lura_eyre::{ensure, Result};
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

/// Provides the `wrap_err` method for `Result`.
///
/// This trait is sealed and cannot be implemented for types outside of
/// `eyre`.
///
/// # Example
///
/// ```
/// use lura_eyre::{WrapErr, Result};
/// use std::fs;
/// use std::path::PathBuf;
///
/// pub struct ImportantThing {
///     path: PathBuf,
/// }
///
/// impl ImportantThing {
///     # const IGNORE: &'static str = stringify! {
///     pub fn detach(&mut self) -> Result<()> {...}
///     # };
///     # fn detach(&mut self) -> Result<()> {
///     #     unimplemented!()
///     # }
/// }
///
/// pub fn do_it(mut it: ImportantThing) -> Result<Vec<u8>> {
///     it.detach().wrap_err("Failed to detach the important thing")?;
///
///     let path = &it.path;
///     let content = fs::read(path)
///         .wrap_err_with(|| format!("Failed to read instrs from {}", path.display()))?;
///
///     Ok(content)
/// }
/// ```
///
/// When printed, the outermost error would be printed first and the lower
/// level underlying causes would be enumerated below.
///
/// ```console
/// Error: Failed to read instrs from ./path/to/instrs.json
///
/// Caused by:
///     No such file or directory (os error 2)
/// ```
///
/// # Wrapping Types That Don't impl `Error` (e.g. `&str` and `Box<dyn Error>`)
///
/// Due to restrictions for coherence `Report` cannot impl `From` for types that don't impl
/// `Error`. Attempts to do so will give "this type might implement Error in the future" as an
/// error. As such, `wrap_err`, which uses `From` under the hood, cannot be used to wrap these
/// types. Instead we encourage you to use the combinators provided for `Result` in `std`/`core`.
///
/// For example, instead of this:
///
/// ```rust,compile_fail
/// use std::error::Error;
/// use lura_eyre::{WrapErr, Report};
///
/// fn wrap_example(err: Result<(), Box<dyn Error + Send + Sync + 'static>>) -> Result<(), Report> {
///     err.wrap_err("saw a downstream error")
/// }
/// ```
///
/// We encourage you to write this:
///
/// ```rust
/// use std::error::Error;
///
/// use lura_eyre::{eyre, Report, WrapErr};
///
/// fn wrap_example(err: Result<(), Box<dyn Error + Send + Sync + 'static>>) -> Result<(), Report> {
///   err.map_err(|e| eyre!(e)).wrap_err("saw a downstream error")
/// }
/// ```
///
/// # Effect on downcasting
///
/// After attaching a message of type `D` onto an error of type `E`, the resulting
/// `lura_eyre::Report` may be downcast to `D` **or** to `E`.
///
/// That is, in codebases that rely on downcasting, Eyre's wrap_err supports
/// both of the following use cases:
///
///   - **Attaching messages whose type is insignificant onto errors whose type
///     is used in downcasts.**
///
///     In other error libraries whose wrap_err is not designed this way, it can
///     be risky to introduce messages to existing code because new message might
///     break existing working downcasts. In Eyre, any downcast that worked
///     before adding the message will continue to work after you add a message, so
///     you should freely wrap errors wherever it would be helpful.
///
///   - **Attaching message whose type is used in downcasts onto errors whose
///     type is insignificant.**
///
///     Some codebases prefer to use machine-readable messages to categorize
///     lower level errors in a way that will be actionable to higher levels of
///     the application.
pub trait WrapErr<T, E>: sealed::Sealed {
  /// Wrap the error value with a new adhoc error
  #[cfg_attr(track_caller, track_caller)]
  fn wrap_err<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static;

  /// Wrap the error value with a new adhoc error that is evaluated lazily
  /// only once an error does occur.
  #[cfg_attr(track_caller, track_caller)]
  fn wrap_err_with<D, F>(self, f: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D;

  /// Compatibility re-export of wrap_err for interopt with `anyhow`
  #[cfg_attr(track_caller, track_caller)]
  fn context<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static;

  /// Compatibility re-export of wrap_err_with for interopt with `anyhow`
  #[cfg_attr(track_caller, track_caller)]
  fn with_context<D, F>(self, f: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D;
}

/// Provides the `context` method for `Option` when porting from `anyhow`
///
/// This trait is sealed and cannot be implemented for types outside of
/// `eyre`.
///
/// ## Why Doesn't `Eyre` impl `WrapErr` for `Option`?
///
/// `eyre` doesn't impl `WrapErr` for `Option` because `wrap_err` implies that you're creating a
/// new error that saves the previous error as its `source`. Calling `wrap_err` on an `Option` is
/// meaningless because there is no source error. `anyhow` avoids this issue by using a different
/// mental model where you're adding "context" to an error, though this not a mental model for
/// error handling that `eyre` agrees with.
///
/// Instead, `eyre` encourages users to think of each error as distinct, where the previous error
/// is the context being saved by the new error, which is backwards compared to anyhow's model. In
/// this model you're encouraged to use combinators provided by `std` for `Option` to convert an
/// option to a `Result`
///
/// # Example
///
/// Instead of:
///
/// ```rust
/// use lura_eyre::ContextCompat;
///
/// fn get_thing(mut things: impl Iterator<Item = u32>) -> lura_eyre::Result<u32> {
///   things
///     .find(|&thing| thing == 42)
///     .context("the thing wasnt in the list")
/// }
/// ```
///
/// We encourage you to use this:
///
/// ```rust
/// use lura_eyre::eyre;
///
/// fn get_thing(mut things: impl Iterator<Item = u32>) -> lura_eyre::Result<u32> {
///   things
///     .find(|&thing| thing == 42)
///     .ok_or_else(|| eyre!("the thing wasnt in the list"))
/// }
/// ```
pub trait ContextCompat<T>: sealed::Sealed {
  /// Compatibility version of `wrap_err` for creating new errors with new source on `Option`
  /// when porting from `anyhow`
  #[cfg_attr(track_caller, track_caller)]
  fn context<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static;

  /// Compatibility version of `wrap_err_with` for creating new errors with new source on `Option`
  /// when porting from `anyhow`
  #[cfg_attr(track_caller, track_caller)]
  fn with_context<D, F>(self, f: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D;

  /// Compatibility re-export of `context` for porting from `anyhow` to `eyre`
  #[cfg_attr(track_caller, track_caller)]
  fn wrap_err<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static;

  /// Compatibility re-export of `with_context` for porting from `anyhow` to `eyre`
  #[cfg_attr(track_caller, track_caller)]
  fn wrap_err_with<D, F>(self, f: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D;
}

mod sealed {
  use super::*;
  pub trait Sealed {}

  pub trait StdError {
    #[cfg_attr(track_caller, track_caller)]
    fn ext_report<D>(self, msg: D) -> crate::Report
    where
      D: Display + Send + Sync + 'static;
  }

  impl<E> StdError for E
  where
    E: std::error::Error + Send + Sync + 'static,
  {
    fn ext_report<D>(self, msg: D) -> crate::Report
    where
      D: Display + Send + Sync + 'static,
    {
      crate::Report::Eyre(eyre::Report::from(self).wrap_err(msg))
    }
  }

  impl StdError for crate::Report {
    fn ext_report<D>(self, msg: D) -> crate::Report
    where
      D: Display + Send + Sync + 'static,
    {
      match self {
        crate::Report::Eyre(eyre) => crate::Report::Eyre(eyre.wrap_err(msg)),
        crate::Report::Miette(miette) => crate::Report::Miette(miette),
      }
    }
  }

  impl<T, E> Sealed for Result<T, E> where E: StdError {}
  impl<T> Sealed for Option<T> {}
}

impl<T, E> WrapErr<T, E> for Result<T, E>
where
  E: sealed::StdError + Send + Sync + 'static,
{
  fn wrap_err<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
  {
    match self {
      Ok(t) => Ok(t),
      Err(e) => Err(e.ext_report(msg)),
    }
  }

  fn wrap_err_with<D, F>(self, msg: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D,
  {
    match self {
      Ok(t) => Ok(t),
      Err(e) => Err(e.ext_report(msg())),
    }
  }

  fn context<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
  {
    self.wrap_err(msg)
  }

  fn with_context<D, F>(self, msg: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D,
  {
    self.wrap_err_with(msg)
  }
}

impl<T> ContextCompat<T> for Option<T> {
  fn wrap_err<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
  {
    self.context(msg)
  }

  fn wrap_err_with<D, F>(self, msg: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D,
  {
    self.with_context(msg)
  }

  fn context<D>(self, msg: D) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
  {
    eyre::ContextCompat::context(self, msg).map_err(Report::Eyre)
  }

  fn with_context<D, F>(self, msg: F) -> Result<T, Report>
  where
    D: Display + Send + Sync + 'static,
    F: FnOnce() -> D,
  {
    eyre::ContextCompat::with_context(self, msg).map_err(Report::Eyre)
  }
}
