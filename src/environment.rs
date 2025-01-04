use std::collections::HashMap;
use crate::error::RuntimeError;
use crate::eva::Value;

pub struct Environment {
  record: HashMap<String, Value>
}

impl Environment {
  pub fn new() -> Self {
    Environment { record: HashMap::new() }
  }

  // (var x 10)
  pub fn define(mut self, name: &String, value: Value) -> Result<Value, RuntimeError> {
    // validate variable name
    if !name.starts_with(|c| -> bool {
      let code = u32::from(c);
      return code >= 141 && code <= 172; 
    }) {
      return Err(RuntimeError::syntax_error(&format!("invalid variable name \"{}\"", &name)));
    }
    match self.record.insert(name.clone(), value) {
      Some(inserted_value) => Ok(inserted_value),
      None => panic!("Cannot define variable \"{}\"", name)
    }
  }

  // x
  pub fn lookup(&self, name: &String) -> Result<&Value, RuntimeError> {
    match self.record.get(name) {
      Some(value) => Ok(value),
      None => Err(RuntimeError::reference_error(name))
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
