use std::collections::HashMap;
use crate::error::RuntimeError;
use crate::eva::Value;

#[derive(Clone, Debug, Default)]
pub struct Environment {
  record: HashMap<String, Value>,
  parent: Option<Box<Environment>>
}

impl Environment {
  pub fn new(parent: Option<Environment>) -> Self {
    Environment { 
      record: HashMap::new(),
      parent: parent.map(|p| Box::new(p)) 
    }
  }

  // (var x 10)
  pub fn define(&mut self, id: String, value: Value) -> Result<&Value, RuntimeError> {
    // validate id name starts with a-z
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
  pub fn lookup(&mut self, id: String) -> Result<&Value, RuntimeError> {
    match self.resolve_env_mut(&id) {
      Some(env) => {
        match env.record.get(&id.clone()) {
          Some(value) => Ok(value),
          None => Err(RuntimeError::reference_error(&id))
        }
      }
      None => Err(RuntimeError::reference_error(&id))
    }
  }

  // (set x 20)
  pub fn set(&mut self, id: String, value: Value) -> Result<Value, RuntimeError> {
    match self.resolve_env_mut(&id) {
      Some(env) => {
        match env.record.insert(id.clone(), value) {
          Some(inserted_value) => Ok(inserted_value),
          None => panic!("Cannot set identificator \"{}\".", id.clone())
        }
      },
      None =>Err(RuntimeError::reference_error(&id))
    }
  }

  fn resolve_env_mut(&mut self, id: &String) -> Option<&mut Environment> {
    if self.record.contains_key(id) {
      Some(self)
    } else if let Some(parent_env) = &mut self.parent {
      parent_env.resolve_env_mut(id)
    } else {
      None
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

      let x = env.lookup("x".to_string());
      assert!(matches!(x, Ok(Value::Int(10))));
    }

    #[test]
    fn set_value() {
      let mut env = Environment::new(None);
      let _ = env.define("x".to_string(), Value::Int(10));
      let _ = env.set("x".to_string(), Value::Int(20));
      let x = env.lookup("x".to_string()).expect("Cannot get value x");
      assert!(matches!(x, Value::Int(20)));
    }
}
