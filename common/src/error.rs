#[derive(Debug)]
pub enum RuntimeErrorKind {
  ReferenceError,
  SyntaxError,
}

#[derive(Debug)]
pub struct RuntimeError {
  pub kind: RuntimeErrorKind,
  pub message: String,
}

impl RuntimeError {
  pub fn new(kind: RuntimeErrorKind, message: String) -> Self {
    RuntimeError {
      kind,
      message
    }
  }

  pub fn reference_error(var_name: &String) -> Self {
    Self::new(
      RuntimeErrorKind::ReferenceError,
      format!("Reference error: variable \"{}\" is not defined.", var_name))
  }

  pub fn syntax_error(details: String) -> Self {
    Self::new(
      RuntimeErrorKind::SyntaxError,
      format!("Syntax error: \"{}\".", details))
  }
}
