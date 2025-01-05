use std::fmt::Debug;
use unicode_segmentation::{self, UnicodeSegmentation};

use crate::environment::Environment;


#[derive(Debug,PartialEq,Clone)]
pub enum Value {
  Int(isize),
  Str(String),
  Null
}

#[derive(Debug,PartialEq)]
pub enum Expr {
  Literal(Value),
  Operation(char, Box<Expr>, Box<Expr>),
  VariableDeclaration(String, String, Value),
  Identifier(String)
}

pub struct Eva {
  pub global: Environment
}

impl Eva {
  // Create global environment
  // Predefine values: null, true, false
  fn new() -> Self {
    Eva {
      global: Environment::new()
    }
  }

  fn eval(&mut self, exp: Expr) -> Option<Value> {
    match exp {
        Expr::Literal(value) => {
          match value {
            Value::Int(n) => {
              return Some(Value::Int(n));
            },
            Value::Str(s) => {
              if s.graphemes(true).nth(0).unwrap() == r#"""# && s.graphemes(true).last().unwrap() == r#"""# {
                let with_quotation_trimmed = s[1..s.len()-1].to_string();
                return Some(Value::Str(with_quotation_trimmed));
              }
              return None
            },
            Value::Null => None
          }
        }
        Expr::Operation(operator, exp1,exp2 ) => {
          match operator {
            '+' => {
              // Q: How to reuse the parts from each arm?
              let n1 = self.eval(*exp1).expect("Invalid operand.");
              let n2 = self.eval(*exp2).expect("Invalid operand.");
              match (n1, n2) {
                (Value::Int(num1), Value::Int(num2)) => Some(Value::Int(num1 + num2)),
                _ => None
              }
            },
            '*' => {
              let n1 = self.eval(*exp1).expect("Invalid operand.");
              let n2 = self.eval(*exp2).expect("Invalid operand.");
              match (n1, n2) {
                (Value::Int(num1), Value::Int(num2)) => Some(Value::Int(num1 * num2)),
                _ => None
              }
            },
            _ => None
          }
        }
        Expr::VariableDeclaration(var_keyword, id_name, value) => {
          if var_keyword != "var".to_string() {
            panic!("Unknown keyword: {}", var_keyword);
          }
          let _ = &self.global.define(id_name, value.clone())
            .map_err(|e| panic!("{:?}", e));
          Some(value) 
        }
        Expr::Identifier(id) => {
          // TODO: Check for reserved words
          match self.global.lookup(&id) {
            Ok(value) => Some(value.clone()),
            Err(err) => panic!("{:?}", err)
          }
        }
    }
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn self_evaluating_expressions() {
    let mut eva = Eva::new();

    assert_eq!(eva.eval(Expr::Literal(Value::Null)), None);
    assert_eq!(eva.eval(Expr::Literal(Value::Int(1))), Some(Value::Int(1)));
    assert_eq!(eva.eval(Expr::Literal(Value::Int(-10))), Some(Value::Int(-10)));

    assert_eq!(eva.eval(Expr::Literal(Value::Str(r#""hello""#.to_string()))), Some(Value::Str("hello".to_string())));
    assert_eq!(eva.eval(Expr::Literal(Value::Str(r#""🎅 新年快乐!""#.to_string()))), Some(Value::Str("🎅 新年快乐!".to_string())));
  }

  #[test]
  fn addition() {
    let mut eva = Eva::new();

    assert_eq!(eva.eval(
      Expr::Operation('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
      Some(Value::Int(5))
    );
    assert_eq!(eva.eval(
      Expr::Operation(
        '+', 
        Box::new(Expr::Operation('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
        Box::new(Expr::Literal(Value::Int(3))),
      ),
    ), Some(Value::Int(8)));
    assert_eq!(eva.eval(
      Expr::Operation(
        '+', 
        Box::new(Expr::Operation('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
        Box::new(Expr::Operation('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
      ),
    ), Some(Value::Int(10)));
  }

  #[test]
  fn multiplication() {
    let mut eva = Eva::new();

    assert_eq!(eva.eval(
      Expr::Operation('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
      Some(Value::Int(6))
    );
    assert_eq!(eva.eval(
      Expr::Operation(
        '*', 
        Box::new(Expr::Operation('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
        Box::new(Expr::Literal(Value::Int(3))),
      ),
    ), Some(Value::Int(18)));
    assert_eq!(eva.eval(
      Expr::Operation(
        '*', 
        Box::new(Expr::Operation('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
        Box::new(Expr::Operation('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
      ),
    ), Some(Value::Int(36)));
  }

  #[test]
  fn variable_declaration() {
    let mut eva = Eva::new();

    assert_eq!(eva.eval(Expr::VariableDeclaration("var".to_string(), "x".to_string(), Value::Int(10))), Some(Value::Int(10)));
    assert_eq!(eva.eval(Expr::Identifier("x".to_string())), Some(Value::Int(10)));
  }
}
