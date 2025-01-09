use std::collections::HashMap;
use crate::error::RuntimeError;
use crate::eva::Value;

#[derive(Clone, Default)]
pub struct Environment<'a> {
  record: HashMap<String, Value>,
  parent: Option<&'a Environment<'a>>
}

impl<'a> Environment<'a> {
  pub fn new(parent: Option<&'a Environment<'a>>) -> Self {
    Environment { record: HashMap::new(), parent }
  }

  // (var x 10)
  pub fn define(&mut self, id: String, value: Value) -> Result<&Value, RuntimeError> {
    // validate id name
    if !id.starts_with(|c| -> bool {
      let code = u32::from(c);
      println!("{}", code);
      return code >= 97 && code <= 122; 
    }) {
      return Err(RuntimeError::syntax_error(format!("invalid identifier name \"{}\"", &id)));
    }
    match self.record.insert(id.clone(), value) {
      Some(_) => Err(RuntimeError::syntax_error(format!("identifier  \"{}\" has already been defined.", id))),
      None => self.record.get(&id).ok_or(RuntimeError::syntax_error(format!("fatal. \"{}\" cannot be defined.", id))), 
    }
  }

  // x
  pub fn lookup(&self, id: &String) -> Result<&Value, RuntimeError> {
    let mut current_env = self;
    match current_env.record.get(id) {
      Some(value) => Ok(value),
      None => {
        while current_env.parent.is_some() {
          current_env = current_env.parent.unwrap();
          return current_env.lookup(id);
        }
        Err(RuntimeError::reference_error(id))
      }
    }
  }

  // (set x 20)
  pub fn set(&mut self, id: String, value: Value) -> Result<Value, RuntimeError> {
    match self.lookup(&id) {
      Ok(_) => {
        match self.record.insert(id.clone(), value) {
          Some(inserted_value) => Ok(inserted_value),
          None => panic!("Cannot set identificator \"{}\".", id)
        }
      },
      Err(err) => Err(err),
    }
  }
}

#[cfg(test)]
mod tests {
    use crate::eva::Value;

    use super::Environment;

    #[test]
    fn define_and_lookup() {
      let mut env = Environment::new(None);
      
      assert!(env.record.is_empty());
      
      let x = env.define("x".to_string(), Value::Int(10));
      assert!(matches!(x, Ok(Value::Int(10))));

      let x = env.define("x".to_string(), Value::Int(10));
      assert!(matches!(x, Err(RuntimeErrorKind)));

      let x = env.lookup(&"x".to_string());
      assert!(matches!(x, Ok(Value::Int(10))));
    }

    #[test]
    fn set_value() {
      let mut env = Environment::new(None);
      let _ = env.define("x".to_string(), Value::Int(10));
      let _ = env.set("x".to_string(), Value::Int(20));
      let x = env.lookup(&"x".to_string()).expect("Cannot get value x");
      assert!(matches!(x, Value::Int(20)));
    }
}
