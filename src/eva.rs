use std::fmt::Debug;
use unicode_segmentation::{self, UnicodeSegmentation};

use crate::environment::Environment;


#[derive(Debug,PartialEq,Clone)]
pub enum Value {
  Int(isize),
  Str(String),
  Null,
  Boolean(bool)
}

#[derive(Debug,PartialEq)]
pub enum Expr {
  Literal(Value),
  BinaryExpression(char, Box<Expr>, Box<Expr>),
  VariableDeclaration(String, String, Box<Expr>),
  Identifier(String),
  BlockStatement(String, Vec<Expr>)
}

pub struct Eva {
  pub global: Environment
}

impl Eva {
  // Create global environment
  // Predefine values: null, true, false
  pub fn new() -> Self {
    let mut global_env = Environment::new();
    let _ = global_env.define("null".to_string(), Value::Null);
    let _ = global_env.define("true".to_string(), Value::Boolean(true));
    let _ = global_env.define("false".to_string(), Value::Boolean(false)); 
    Eva {
      global: global_env,
    }
  }

  pub fn eval(&mut self, exp: Expr, env: &mut Environment) -> Option<Value> {
    match exp {
        Expr::Literal(value) => {
          match value {
            Value::Int(n) => Some(Value::Int(n)),
            Value::Str(s) => {
              if s.graphemes(true).nth(0).unwrap() == r#"""# && s.graphemes(true).last().unwrap() == r#"""# {
                let with_quotation_trimmed = s[1..s.len()-1].to_string();
                return Some(Value::Str(with_quotation_trimmed));
              }
              return None
            },
            Value::Boolean(b) => Some(Value::Boolean(b)),
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
        Expr::VariableDeclaration(var_keyword, id_name, exp) => {
          if var_keyword != "var".to_string() {
            panic!("Unknown keyword: {}", var_keyword);
          }
          let result = match self.eval(*exp, env) {
            Some(value) => {
              env.define(id_name, value)
            },
            None => panic!("Unable to declare variable")
          };
          match result {
              Ok(value) => Some(value.clone()),
              Err(err) => panic!("{:?}", err),
          }
        }
        Expr::Identifier(id) => {
          // TODO: Check for reserved words
          match env.lookup(&id) {
            Ok(value) => Some(value.clone()),
            Err(err) => panic!("{:?}", err)
          }
        },
        Expr::BlockStatement(keyword, expressions) => {
          if keyword != "begin" {
            panic!("Invalid block statement")
          }
          let mut block_env = Environment::new();
          self.eval_block(expressions, &mut block_env)
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

  fn eval_block(&mut self, expressions: Vec<Expr>, env: &mut Environment) -> Option<Value> {
    let mut result = None;
    for exp in expressions {
      result = self.eval(exp, env);
    }
    result
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn default_globals() {
    let eva = Eva::new();

    assert!(
      matches!(
        eva.global.lookup(&"null".to_string()),
        Ok(Value::Null)
      )
    );
    assert!(
      matches!(
        eva.global.lookup(&"true".to_string()),
        Ok(Value::Boolean(true))
      )
    );
    assert!(
      matches!(
        eva.global.lookup(&"false".to_string()),
        Ok(Value::Boolean(false))
      )
    );
  }

  #[test]
  fn self_evaluating_expressions() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(Expr::Literal(Value::Null), &mut eva.global.clone()), 
      None
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Boolean(true)), &mut eva.global.clone()), 
      Some(Value::Boolean(true))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Boolean(false)), &mut eva.global.clone()), 
      Some(Value::Boolean(false))
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
         Box::new(Expr::Literal(Value::Int(10)))
        ),
        &mut env,
      ), 
      Some(Value::Int(10))
    );
    assert!(
      matches!(
        env.lookup(&"x".to_string()),
        Ok(Value::Int(10))
      )
    );

    eva.eval(
      Expr::VariableDeclaration(
        "var".to_string(),
        "z".to_string(),
        Box::new(
          Expr::BinaryExpression(
            '+',
            Box::new(Expr::Literal(Value::Int(2))),
            Box::new(Expr::Literal(Value::Int(4))),
        ),
      )),
      &mut env
    );
    assert!(
      matches!(
        env.lookup(&"z".to_string()),
        Ok(Value::Int(6))
      )
    );
  }

  #[test]
  fn block_statement() {
    let mut eva = Eva::new();
    let expr1 =  Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(Expr::Literal(Value::Int(10))));
    let expr2 = Expr::VariableDeclaration("var".to_string(), "y".to_string(), Box::new(Expr::Literal(Value::Int(20))));
    let expr3 = Expr::BinaryExpression(
      '+',
      Box::new(Expr::BinaryExpression(
        '*', 
        Box::new(Expr::Identifier("x".to_string())),
        Box::new(Expr::Identifier("y".to_string())))
      ),
      Box::new(Expr::Literal(Value::Int(30))),
    );

    assert_eq!(
      eva.eval(
        Expr::BlockStatement("begin".to_string(), vec![expr1, expr2, expr3]), 
        &mut eva.global.clone()
      ),
      Some(Value::Int(230))
    )
  }
}
