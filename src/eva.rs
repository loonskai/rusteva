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
  BinaryExpression(char, Box<Expr>, Box<Expr>),
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

  fn eval(&mut self, exp: Expr, env: &mut Environment) -> Option<Value> {
    // let env_ref = env;
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
        Expr::BinaryExpression(operator, exp1,exp2 ) => {
          match operator {
            '+' => self.binary_operation(env, exp1, exp2, |n1, n2| n1 + n2),
            '-' => self.binary_operation(env, exp1, exp2, |n1, n2| n1 - n2),
            '*' => self.binary_operation(env, exp1, exp2, |n1, n2| n1 * n2),
            '/' => self.binary_operation(env, exp1, exp2, |n1, n2| {
              if n2 == 0 {
                panic!("Attempt to divide by zero.")
              }
              n1 / n2
            }),
            _ => None
          }
        }
        Expr::VariableDeclaration(var_keyword, id_name, value) => {
          if var_keyword != "var".to_string() {
            panic!("Unknown keyword: {}", var_keyword);
          }
          let _ = &env.define(id_name, value.clone())
            .map_err(|e| panic!("{:?}", e));
          Some(value) 
        }
        Expr::Identifier(id) => {
          // TODO: Check for reserved words
          match env.lookup(&id) {
            Ok(value) => Some(value.clone()),
            Err(err) => panic!("{:?}", err)
          }
        }
    }
  }

  fn binary_operation<F>(&mut self, env: &mut Environment, exp1: Box<Expr>, exp2: Box<Expr>, operation: F) -> Option<Value> 
  where
    F: Fn(isize, isize) -> isize
  {
    let v1 = self.eval(*exp1, env).expect("Invalid operand.");
    let v2 = self.eval(*exp2, env).expect("Invalid operand.");
    match (v1, v2) {
      (Value::Int(n1), Value::Int(n2)) => {
        Some(Value::Int(operation(n1, n2)))
      },
      _ => None
    }

  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn self_evaluating_expressions() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(Expr::Literal(Value::Null), &mut eva.global.clone()), 
      None
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Int(1)), &mut eva.global.clone()), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Int(-10)), &mut eva.global.clone()), 
      Some(Value::Int(-10))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Str(r#""hello""#.to_string())), &mut eva.global.clone()), 
      Some(Value::Str("hello".to_string()))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Str(r#""🎅 新年快乐!""#.to_string())), 
      &mut eva.global.clone()), 
      Some(Value::Str("🎅 新年快乐!".to_string()))
    );
  }

  #[test]
  fn addition() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '+', 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ), 
        &mut eva.global.clone()
      ), 
      Some(Value::Int(5))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '+', 
          Box::new(Expr::BinaryExpression('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::Literal(Value::Int(3))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(8))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '+', 
          Box::new(Expr::BinaryExpression('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(10))
    );
  }

  #[test]
  fn extraction() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '-', 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ), 
        &mut eva.global.clone()
      ), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '-', 
          Box::new(Expr::BinaryExpression('-', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::Literal(Value::Int(3))),
        ), 
        &mut eva.global.clone()
      ), 
      Some(Value::Int(-2))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '-', 
          Box::new(Expr::BinaryExpression('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression('+', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(0))
    );
  }

  #[test]
  fn multiplication() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '*', 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(6))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '*', 
          Box::new(Expr::BinaryExpression(
            '*', 
            Box::new(Expr::Literal(Value::Int(3))), 
            Box::new(Expr::Literal(Value::Int(2))))
          ), 
          Box::new(Expr::Literal(Value::Int(3))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(18))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '*', 
          Box::new(Expr::BinaryExpression('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression('*', Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(36))
    );
  }

  #[test]
  fn division() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '/', 
          Box::new(Expr::Literal(Value::Int(25))), 
          Box::new(Expr::Literal(Value::Int(5)))
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(5))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '/', 
          Box::new(Expr::BinaryExpression('/', Box::new(Expr::Literal(Value::Int(25))), Box::new(Expr::Literal(Value::Int(5))))), 
          Box::new(Expr::Literal(Value::Int(5))),
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '/', 
          Box::new(Expr::Literal(Value::Int(0))),
          Box::new(Expr::Literal(Value::Int(3))), 
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(0))
    );
    
  }

  #[test]
  #[should_panic]
  fn division_by_zero() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          '/', 
          Box::new(Expr::Literal(Value::Int(3))),
          Box::new(Expr::Literal(Value::Int(0))), 
        ),
        &mut eva.global.clone()
      ), 
      Some(Value::Int(0))
    );
  }

  #[test]
  fn variable_declaration() {
    let mut eva = Eva::new();
    let mut env = std::mem::take(&mut eva.global); 

    assert_eq!(
      eva.eval(
        Expr::VariableDeclaration(
          "var".to_string(), 
          "x".to_string(), 
          Value::Int(10)
        ),
        &mut env,
      ), 
      Some(Value::Int(10))
    );
    // assert_eq!(
    //   eva.eval(
    //     Expr::Identifier("x".to_string()),
    //     &mut eva.global
    //   ), 
    //   Some(Value::Int(10))
    // );
  }
}
