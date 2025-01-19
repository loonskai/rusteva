use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};
use common::{Func, environment::Environment, Expr , Value};
use unicode_segmentation::{self, UnicodeSegmentation};

#[derive(Debug)]
pub struct Eva {
  pub global: Rc<RefCell<Environment>>
}

impl Eva {
  // Create global environment
  // Predefined values: null, true, false, math operations
  pub fn new() -> Self {
    let mut global_env = Environment::new(None);
    let _ = global_env.define(&"null".to_string(), Value::Null);
    let _ = global_env.define(&"true".to_string(), Value::Boolean(true));
    let _ = global_env.define(&"false".to_string(), Value::Boolean(false));
    let _ = global_env.define(&"+".to_string(), Value::Function(Func::new(|args| {
      if args.len() != 2 {
        return Err("Addition operation must have 2 arguments".to_string());
      }
      return match (&args[0], &args[1]) {
        (Value::Int(n1), Value::Int(n2)) => Ok(Value::Int(n1+n2)),
        _ => Err("Invalid operand type".to_string())
      }
    })));
    let _ = global_env.define(&"*".to_string(), Value::Function(Func::new(|args| {
      if args.len() != 2 {
        return Err("Multiplication operation must have 2 arguments".to_string());
      }
      return match (&args[0], &args[1]) {
        (Value::Int(n1), Value::Int(n2)) => Ok(Value::Int(n1*n2)),
        _ => Err("Invalid operand type".to_string())
      }
    })));
    let _ = global_env.define(&"-".to_string(), Value::Function(Func::new(|args| {
      if args.len() != 2 {
        return Err("Multiplication operation must have 2 arguments".to_string());
      }
      return match (&args[0], &args[1]) {
        (Value::Int(n1), Value::Int(n2)) => Ok(Value::Int(n1-n2)),
        _ => Err("Invalid operand type".to_string())
      }
    })));
    let _ = global_env.define(&"/".to_string(), Value::Function(Func::new(|args| {
      if args.len() != 2 {
        return Err("Multiplication operation must have 2 arguments".to_string());
      }
      
      return match (&args[0], &args[1]) {
        (Value::Int(n1), Value::Int(n2)) => {
          if *n2 == 0 {
            return Err("Attempt to divide by zero.".to_string())
          }
          Ok(Value::Int(n1/n2))
        },
        _ => Err("Invalid operand type".to_string())
      }
    })));
    
    Eva {
      global: Rc::new(RefCell::new(global_env)),
    }
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
      Expr::CallExpression(func_name, func_obj) => {
        // Lookup the function by name
        let func_definition = self.eval(Expr::Identifier(func_name.clone()), Some(Rc::clone(&env))).map(|v| {
          match v {
            Value::Function(f) => f,
            _ => panic!("Invalid function format: {}", func_name),
          }
        });
        let activation_env = Environment::new(func_obj.env);
        return match func_definition {
          Some(func) => {
            let values = args.iter().map(|exp| self.eval(exp.clone(), Some(Rc::clone(&env))).expect("Unable to evaluate arguments")).collect();
            match func.call(values) {
                Ok(v) =>  Some(v),
                Err(message) => panic!("{}", message)
            }
          },
          None => None
        }
      },
      Expr::FunctionDeclaration(func_name, params, body) => {
        let func_object = HashMap::from([
          ("params", params),
          ("body", body),
          ("env", Rc::clone(&env)) // Closure
        ]);
        env.borrow().define(&func_name,  Value::Function(Func::new()));
        return None
      },
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
  fn function_declaration() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("
      (begin 
        (def square (x) (* x x))
        (square 2)
      )
      "), None),
      Some(Value::Int(2))
    );
  }
}
