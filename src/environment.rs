use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use syntax::Value;

use crate::error::RuntimeError;

#[derive(Clone, Debug, Default)]
pub struct Environment {
  record: HashMap<String, Value>,
  parent: Option<Rc<RefCell<Environment>>>
}

impl Environment {
  pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
    Environment { 
      record: HashMap::new(),
      parent
    }
  }

  // (var x 10)
  pub fn define(&mut self, id: &String, value: Value) -> Result<&Value, RuntimeError> {
    match self.record.insert(id.clone(), value) {
      Some(_) => Err(RuntimeError::syntax_error(format!("identifier  \"{}\" has already been defined.", id))),
      None => self.record.get(id).ok_or(RuntimeError::syntax_error(format!("fatal. \"{}\" cannot be defined.", id))), 
    }
  }

  // x
  pub fn lookup(&self, id: &String) -> Result<Value, RuntimeError> {
    if let Some(value) = self.record.get(id) {
      Ok(value.clone())
    } else if let Some(parent_env) = &self.parent {
      parent_env.borrow().lookup(id)
    } else {
      Err(RuntimeError::reference_error(&id))
    }
  }

  // (set x 20)
  pub fn set(&mut self, id: &String, value: Value) -> Result<Value, RuntimeError> {
    if self.record.contains_key(id) {
      let result = value.clone();
      self.record.insert(id.to_string(), value);
      Ok(result)
    } else if let Some(parent_env) = &self.parent {
      parent_env.borrow_mut().set(id, value)
    } else {
      Err(RuntimeError::reference_error(&id))
    }
  }
}

#[cfg(test)]
mod tests {
    use syntax::Value;
    use super::Environment;

    #[test]
    fn define_and_lookup() {
      let mut env = Environment::new(None);
      
      assert!(env.record.is_empty());
      
      let x = env.define(&"x".to_string(), Value::Int(10));
      assert!(matches!(x, Ok(Value::Int(10))));

      let x = env.define(&"x".to_string(), Value::Int(10));
      assert!(matches!(x, Err(RuntimeErrorKind)));

      let x = env.lookup(&"x".to_string());
      assert!(matches!(x, Ok(Value::Int(10))));
    }

    #[test]
    fn set_value() {
      let mut env = Environment::new(None);
      let _ = env.define(&"x".to_string(), Value::Int(10));
      let _ = env.set(&"x".to_string(), Value::Int(20));
      let x = env.lookup(&"x".to_string()).expect("Cannot get value x");
      assert!(matches!(x, Value::Int(20)));
    }
}
