use std::fmt::Display;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Stack {
  pub functions: im::HashMap<String, crate::domain::ForeignFunction>,
}

impl Stack {
  pub fn push_frame(&self) {}

  pub fn pop_frame(&self) {}

  /// Unwinds the stack frame
  pub fn unwind<S: Display>(&self, message: S) -> ! {
    std::panic::panic_any(crate::domain::RuntimeError {
      message: Some(message.to_string()),
      stacktrace: self.clone(),
    })
  }
}
