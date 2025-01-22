use std::{cell::RefCell, fmt::Debug, rc::Rc};
use common::{FuncObj, environment::Environment, Expr , Value};
use unicode_segmentation::{self, UnicodeSegmentation};

macro_rules! define_global_variables {
    ($global:expr, $($key_value:expr),*) => {
        $(
          let (key, value) = $key_value;
          let _ = $global.borrow_mut().define(&key.to_string(), value);
        )*
    };
}

macro_rules! define_binary_operators {
    ($global:expr, $($op:expr),* ) => {
        $(
          let n1 = Value::Str("n1".to_string());
          let n2 = Value::Str("n2".to_string());
          let _ = $global.borrow_mut().define(&$op.to_string(), Value::Function(FuncObj::new(
            vec![n1.clone(), n2.clone()],
            Box::new(Expr::BinaryExpression($op.to_string(), Box::new(Expr::Literal(n1)), Box::new(Expr::Literal(n2)))),
            Rc::clone(&$global)
            )
          ));
        )*
    };
}

#[derive(Debug)]
pub struct Eva {
  pub global: Rc<RefCell<Environment>>
}

impl Eva {
  // Create global environment
  // Predefined values: null, true, false, math operations
  pub fn new() -> Self {
    let global = Rc::new(RefCell::new(Environment::new(None)));
    define_global_variables!(
      global, 
      ("null", Value::Null), 
      ("true", Value::Boolean(true)), 
      ("false", Value::Boolean(false))
    );
    define_binary_operators!(global, "+", "-", "*", "/");

    Eva { global }
  }

  pub fn eval(&mut self, exp: Expr, current_env: Option<Rc<RefCell<Environment>>>) -> Option<Value> {
    let env = current_env.map_or(Rc::clone(&self.global), |e| e);
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
          Value::Function(f) => Some(Value::Function(f)),
          Value::Null => None
        }
      }
      Expr::BinaryExpression(operator, exp1,exp2 ) => {
        let v1 = self.eval(*exp1, Some(Rc::clone(&env))).expect("Invalid operand");
        let v2 = self.eval(*exp2, Some(Rc::clone(&env))).expect("Invalid operand");
        match (v1, v2) {
          (Value::Int(n1), Value::Int(n2)) => {
            if matches!(operator.as_str(), ">" | "<" | ">=" | "<=" | "==") {
              let result = match operator.as_str() {
                ">" => n1 > n2,
                "<" => n1 < n2,
                ">=" => n1 >= n2,
                "<=" => n1 <= n2,
                "==" => n1 == n2,
                _ => panic!("Unknown operator")
              };
              return Some(Value::Boolean(result));
            } else if matches!(operator.as_str(), "+" | "-" | "*" | "/") {
              let result = match operator.as_str() {
                "+" => n1 + n2,
                "-" => n1 - n2,
                "*" => n1 * n2,
                "/" => {
                  if n2 == 0 {
                    panic!("Attempt to divide by zero.")
                  }
                  n1 / n2
                },
                _ => panic!("Unknown operator")
              };
              return Some(Value::Int(result));
            } else {
              panic!("Invalid operator")
            }
          }
          _ => None,
        }
      }
      Expr::VariableDeclaration(id_name, exp) => {
        if let Some(value) = self.eval(*exp, Some(Rc::clone(&env))) {
          if let Ok(result) = env.borrow_mut().define(&id_name, value) {
            return Some(result.clone())
          }
        }
        panic!("Unable to declare variable")
      }
      Expr::Identifier(id) => {
        // TODO: Check for reserved words
        match env.borrow().lookup(&id) {
          Ok(value) => Some(value.clone()),
          Err(err) => panic!("{:?}", err)
        }
      },
      Expr::BlockStatement(expressions) => {
        let block_env = Environment::new(Some(env)); // BROKEN - we must not clone the parent env
        self.eval_block(expressions, Rc::new(RefCell::new(block_env)))
      },
      Expr::Assignment(id, expr) => {
        let result = self.eval(*expr, Some(Rc::clone(&env))).map_or(Value::Null, |value| value);
        let _ = env.borrow_mut().set(&id, result.clone());
        Some(result)
      },
      Expr::IfExpression(condition_expr, consequent_expr, alternate_exp) => {
        let result = self.eval(*condition_expr, Some(Rc::clone(&env)));
        if let Some(cond) = result {
          match cond {
              Value::Boolean(value) => {
                if value {
                  return self.eval(*consequent_expr, Some(Rc::clone(&env)));
                } else {
                  return self.eval(*alternate_exp, Some(Rc::clone(&env)));
                }
              },
              _ => panic!("Condition expression must return boolean")
          }
        }
        None
      },
      Expr::WhileStatement(condition_expr, consequent_expr) => {
        let condition = *condition_expr;
        let consequent = *consequent_expr;
        let mut condition_result = self.eval(condition.clone(), Some(Rc::clone(&env))); // 1
        let mut consequent_result = None;
        while let Some(true) = condition_result.as_ref().map(|v| {
          match v {
              Value::Boolean(bool_result) => bool_result,
              _ => panic!("While condition must return boolean")
          }
        }) {
          // Q: Why the order of both evals matters?
          consequent_result = self.eval(consequent.clone(), Some(Rc::clone(&env)));
          condition_result = self.eval(condition.clone(), Some(Rc::clone(&env)));
        }
        return consequent_result;
      },
      Expr::CallExpression(func_name, args) => {
        // Lookup the function by name
        let func_obj = self.eval(Expr::Identifier(func_name.clone()), Some(Rc::clone(&env))).map(|v| {
          match v {
            Value::Function(f) => f,
            _ => panic!("Invalid function format: {}", func_name),
          }
        }).expect("Function not found");
        if args.len() != func_obj.params.len() {
          panic!("Function arguments mismatch!")
        }
        let mut activation_env = Environment::new(Some(func_obj.env));
        for param in func_obj.params.into_iter().enumerate() {
          match &param {
            (index, Value::Str(param_name)) => {
              let evaluated_arg = self.eval(args[*index].clone(), Some(Rc::clone(&env))).expect("Unable to evaluate argument");
              let _ = activation_env.define(param_name, evaluated_arg);
            },
            _ => panic!("Invalid function parameter format")
          }
        }
        self.eval(*func_obj.body, Some(Rc::new(RefCell::new(activation_env))))
      },
      Expr::ApplyExpression(func_expr, args) => {
        let func_obj = self.eval(*func_expr, Some(Rc::clone(&env))).map(|v| {
          match v {
            Value::Function(f) => f,
            _ => panic!("Invalid function call"),
          }
        }).expect("Expected a function");
        if args.len() != func_obj.params.len() {
          panic!("Function arguments mismatch!");
        }
        let mut activation_env = Environment::new(Some(func_obj.env));
        for param in func_obj.params.into_iter().enumerate() {
          match &param {
            (index, Value::Str(param_name)) => {
              let evaluated_arg = self.eval(args[*index].clone(), Some(Rc::clone(&env))).expect("Unable to evaluate argument");
              activation_env.define(param_name, evaluated_arg).unwrap();
            },
            _ => panic!("Invalid function parameter format")
          }
        }
        self.eval(*func_obj.body, Some(Rc::new(RefCell::new(activation_env))))
      },
      Expr::FunctionDeclaration(func_name, params, body) => {
        let func_object = FuncObj::new(
          params,
          body,
           Rc::clone(&env), // Closure
        );
        match env.borrow_mut().define(&func_name,  Value::Function(func_object)) {
          Ok(value) => Some(value.clone()),
          Err(err) => panic!("{:?}", err)
        }
      },
      Expr::LambdaExpression(params, body) => {
        let func_obj = FuncObj::new(
          params,
          body,
          Rc::clone(&env),
        );
        Some(Value::Function(func_obj))
      }
    }
  }

  fn eval_block(&mut self, expressions: Vec<Expr>, env: Rc<RefCell<Environment>>) -> Option<Value> {
    let mut result = None;
    for exp in expressions {
      result = self.eval(exp, Some(Rc::clone(&env)));
    }
    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use syntax::Parser;

  #[test]
  fn default_globals() {
    let eva = Eva::new();

    assert!(
      matches!(
        eva.global.borrow().lookup(&"null".to_string()),
        Ok(Value::Null)
      )
    );
    assert!(
      matches!(
        eva.global.borrow().lookup(&"true".to_string()),
        Ok(Value::Boolean(true))
      )
    );
    assert!(
      matches!(
        eva.global.borrow().lookup(&"false".to_string()),
        Ok(Value::Boolean(false))
      )
    );
  }

  #[test]
  fn self_evaluating_expressions() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(Expr::Literal(Value::Null), None), 
      None
    );
    assert_eq!(
      Eva::new().eval(Expr::Literal(Value::Boolean(true)), None), 
      Some(Value::Boolean(true))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Boolean(false)), None), 
      Some(Value::Boolean(false))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Int(1)), None), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Int(-10)), None), 
      Some(Value::Int(-10))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Str(r#""hello""#.to_string())), None), 
      Some(Value::Str("hello".to_string()))
    );
    assert_eq!(
      eva.eval(Expr::Literal(Value::Str(r#""🎅 新年快乐!""#.to_string())), None), 
      Some(Value::Str("🎅 新年快乐!".to_string()))
    );
  }

  #[test]
  fn built_in_functions() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    // Math functions:
    assert_eq!(eva.eval(parser.parse("(+ 1 5)"), None), Some(Value::Int(6)));
    assert_eq!(eva.eval(parser.parse("(+ (+ 2 3) 5)"), None), Some(Value::Int(10)));
    assert_eq!(eva.eval(parser.parse("(+ (* 2 3) 5)"), None), Some(Value::Int(11)));
    assert_eq!(eva.eval(parser.parse("(+ (/ 10 2) 5)"), None), Some(Value::Int(10)));

    // // Comparison - still as binary expression:
    assert_eq!(eva.eval(parser.parse("(> 1 5)"), None), Some(Value::Boolean(false)));
    assert_eq!(eva.eval(parser.parse("(< 1 5)"), None), Some(Value::Boolean(true)));
    assert_eq!(eva.eval(parser.parse("(>= 1 5)"), None), Some(Value::Boolean(false)));
    assert_eq!(eva.eval(parser.parse("(<= 1 5)"), None), Some(Value::Boolean(true)));
    assert_eq!(eva.eval(parser.parse("(== 5 5)"), None), Some(Value::Boolean(true)));
  }

  #[test]
  #[should_panic]
  fn division_by_zero() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("(/ 1 0)"), None), Some(Value::Int(0)));
  }

  #[test]
  fn variable_declaration() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("(var x 10)"), None),
      Some(Value::Int(10))
    );
    assert_eq!(
      eva.eval(parser.parse("x"), None),
      Some(Value::Int(10))
    );
    eva.eval(parser.parse("(var z (+ 2 4))"), None);
    assert_eq!(
      eva.eval(parser.parse("z"), None),
      Some(Value::Int(6))
    );
  }

  #[test]
  fn block_statement() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var x 10)
          (var y 20)
          (+ (* x y) 30)
        )
      "),
        None
      ),
      Some(Value::Int(230))
    );
  }

  #[test]
  fn nested_blocks() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var x 10)
          (begin
            (var x 20))
          x
        )
      "), None),
      Some(Value::Int(10)),
    );
  }

  #[test]
  fn outer_scope_reference() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var x 10)
          (var y (begin
            x))
          y
        )
      "), None),
      Some(Value::Int(10))
    )
  }

  #[test]
  fn scope_chain_traversal() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var value 10)
          (var result (begin
            (var x (+ value 10))
            x)
          )
          result
        )
      "), None),
      Some(Value::Int(20)),
    )
  }

  #[test]
  fn same_scope_assignment() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var data 10)
          (set data 100)
          data
        )
      "), None),
      Some(Value::Int(100))
    );
  }

  #[test]
  fn outer_scope_assignment() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var data 10)
          (begin
            (set data 100)
          )
          data
        )
      "), None),
      Some(Value::Int(100))
    );
  }

  #[test]
  fn if_expression() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var x 10)
          (var y 0)
          (if
            (> x 10)
            (set y 20)
            (set y 30)
          )
        )
      "), None),
      Some(Value::Int(30))
    )
  }

  #[test]
  fn while_statement() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(
      eva.eval(parser.parse("
        (begin
          (var counter 0)
          (var result 0)
          (while
            (< counter 10)
            (begin
              (set counter (+ result 1))
              (set result (+ result 1))
            )
          )
          result
        )
      "), None),
      Some(Value::Int(10))
    )
  }

  #[test]
  fn user_defined_functions() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("
      (begin 
        (def square (x) (* x x))
        (square 2)
      )
      "), None),
      Some(Value::Int(4))
    );
    assert_eq!(eva.eval(parser.parse("
      (begin 
        (def calc (x y) 
          (begin
            (var z 30)
            (+ (* x y) z)
          )
        )
        (calc 10 20)
      )
      "), None),
      Some(Value::Int(230))
    );
    assert_eq!(eva.eval(parser.parse("
      (begin
        (var value 100)
        (def calc (x y) 
          (begin
            (var z (+ x y))
            (def inner (foo)
              (+ (+ foo z) value)
            )
            inner
          )
        )
        (var fn (calc 10 20))
        (fn 30)
      )
      "), None),
      Some(Value::Int(160))
    );
  }

  #[test]
  fn lambda_functions() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("
      (begin
        (def onClick (callback) 
          (begin
            (var x 10)
            (var y 20)
            (callback (+ x y))
          )
        )
        (onClick (lambda (data) (* data 10)))
      )
    "), None), Some(Value::Int(300)))
  }

  #[test]
  fn immediately_invoked_lambda_expression() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("((lambda (x) (* x x)) 2)"), None), Some(Value::Int(4)));
  }
}
