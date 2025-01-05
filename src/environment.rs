use std::collections::HashMap;
use crate::error::{RuntimeErrorKind,RuntimeError};
use crate::eva::{Expr, Value};

pub struct Environment {
  record: HashMap<String, Value>
}

impl Environment {
  pub fn new() -> Self {
    Environment { record: HashMap::new() }
  }

  // (var x 10)
  pub fn define(&mut self, name: String, value: Value) -> Result<&Value, RuntimeError> {
    // validate variable name
    if !name.starts_with(|c| -> bool {
      let code = u32::from(c);
      println!("{}", code);
      return code >= 97 && code <= 122; 
    }) {
      return Err(RuntimeError::syntax_error(format!("invalid identifier name \"{}\"", &name)));
    }
    match self.record.insert(name.clone(), value) {
      Some(_) => Err(RuntimeError::syntax_error(format!("identifier  \"{}\" has already been defined.", name))),
      None => self.record.get(&name).ok_or(RuntimeError::syntax_error(format!("fatal. \"{}\" cannot be defined.", name))), 
    }
  }

  // x
  pub fn lookup(&self, id: &String) -> Result<&Value, RuntimeError> {
    match self.record.get(id) {
      Some(value) => Ok(value),
      None => Err(RuntimeError::reference_error(id))
    }
  }

  // (set x 20)
  pub fn set(mut self, name: &String, value: Value) -> Result<Value, RuntimeError> {
    match self.lookup(name) {
      Ok(_) => {
        match self.record.insert(name.clone(), value) {
          Some(inserted_value) => Ok(inserted_value),
          None => panic!("Cannot define variable \"{}\"", name)
        }
      },
      Err(err) => Err(err),
    }
  }
}

#[cfg(test)]
mod tests {
    use crate::{error::RuntimeError, eva::Value};

    use super::Environment;

    #[test]
    fn define_and_lookup() {
      let mut env = Environment::new();
      
      assert!(env.record.is_empty());
      
      let x = env.define("x".to_string(), Value::Int(10));
      assert!(matches!(x, Ok(Value::Int(10))));

      let x = env.define("x".to_string(), Value::Int(10));
      assert!(matches!(x, Err(RuntimeErrorKind)));

      let x = env.lookup(&"x".to_string());
      assert!(matches!(x, Ok(Value::Int(10))));
    }
}
