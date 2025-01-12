use std::{cell::RefCell, fmt::Debug, rc::Rc};
use unicode_segmentation::{self, UnicodeSegmentation};
use crate::environment::Environment;

#[derive(Debug,PartialEq,Clone)]
pub enum Value {
  Int(isize),
  Str(String),
  Null,
  Boolean(bool)
}

#[derive(Debug,PartialEq,Clone)]
pub enum Expr {
  Literal(Value),
  BinaryExpression(String, Box<Expr>, Box<Expr>),
  VariableDeclaration(String, String, Box<Expr>),
  Identifier(String),
  BlockStatement(String, Vec<Expr>),
  Assignment(String, String, Box<Expr>),
  IfExpression(String, Box<Expr>, Box<Expr>, Box<Expr>),
  WhileStatement(String, Box<Expr>, Box<Expr>)
}

#[derive(Debug)]
pub struct Eva {
  pub global: Rc<RefCell<Environment>>
}

impl Eva {
  // Create global environment
  // Predefine values: null, true, false
  pub fn new() -> Self {
    let mut global_env = Environment::new(None);
    let _ = global_env.define(&"null".to_string(), Value::Null);
    let _ = global_env.define(&"true".to_string(), Value::Boolean(true));
    let _ = global_env.define(&"false".to_string(), Value::Boolean(false)); 
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
            Value::Null => None
          }
        }
        Expr::BinaryExpression(operator, exp1,exp2 ) => {
          let v1 = self.eval(*exp1, Some(Rc::clone(&env))).expect("Invalid operand");
          let v2 = self.eval(*exp2, Some(Rc::clone(&env))).expect("Invalid operand");
          match (v1, v2) {
            (Value::Int(n1), Value::Int(n2)) => {
              let operator_str = operator.as_str();
              if matches!(operator_str, "+" | "-" | "*" | "/") {
                let result = match operator_str {
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
              } else if matches!(operator.as_str(), ">" | "<" | ">=" | "<=" | "==") {
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
        Expr::VariableDeclaration(var_keyword, id_name, exp) => {
          if var_keyword != "var".to_string() {
            panic!("Unknown keyword: {}", var_keyword);
          }
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
        Expr::BlockStatement(keyword, expressions) => {
          if keyword != "begin" {
            panic!("Invalid block statement")
          }
          let block_env = Environment::new(Some(env)); // BROKEN - we must not clone the parent env
          self.eval_block(expressions, Rc::new(RefCell::new(block_env)))
        },
        Expr::Assignment(keyword, id, expr) => {
          if keyword != "set" {
            panic!("Invalid assignment")
          }
          let result = self.eval(*expr, Some(Rc::clone(&env))).map_or(Value::Null, |value| value);
          let _ = env.borrow_mut().set(&id, result.clone());
          Some(result)
        },
        Expr::IfExpression(keyword, condition_expr, consequent_expr, alternate_exp) => {
          if keyword != "if" {
            panic!("Invalid assignment");
          }
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
        Expr::WhileStatement(keyword, condition_expr, consequent_expr) => {
          if keyword != "while" {
            panic!("Invalid while statement");
          }
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
  fn addition() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "+".to_string(), 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ), 
        None
      ), 
      Some(Value::Int(5))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "+".to_string(), 
          Box::new(Expr::BinaryExpression("+".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::Literal(Value::Int(3))),
        ),
        None
      ), 
      Some(Value::Int(8))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "+".to_string(), 
          Box::new(Expr::BinaryExpression("+".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression("+".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        None
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
          "-".to_string(), 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ), 
        None
      ), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "-".to_string(), 
          Box::new(Expr::BinaryExpression("-".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::Literal(Value::Int(3))),
        ), 
        None
      ), 
      Some(Value::Int(-2))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "-".to_string(), 
          Box::new(Expr::BinaryExpression("+".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression("+".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        None
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
          "*".to_string(), 
          Box::new(Expr::Literal(Value::Int(3))), 
          Box::new(Expr::Literal(Value::Int(2)))
        ),
        None
      ), 
      Some(Value::Int(6))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "*".to_string(), 
          Box::new(Expr::BinaryExpression(
            "*".to_string(), 
            Box::new(Expr::Literal(Value::Int(3))), 
            Box::new(Expr::Literal(Value::Int(2))))
          ), 
          Box::new(Expr::Literal(Value::Int(3))),
        ),
        None
      ), 
      Some(Value::Int(18))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "*".to_string(), 
          Box::new(Expr::BinaryExpression("*".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))), 
          Box::new(Expr::BinaryExpression("*".to_string(), Box::new(Expr::Literal(Value::Int(3))), Box::new(Expr::Literal(Value::Int(2))))),
        ),
        None
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
          "/".to_string(), 
          Box::new(Expr::Literal(Value::Int(25))), 
          Box::new(Expr::Literal(Value::Int(5)))
        ),
        None
      ), 
      Some(Value::Int(5))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "/".to_string(), 
          Box::new(Expr::BinaryExpression("/".to_string(), Box::new(Expr::Literal(Value::Int(25))), Box::new(Expr::Literal(Value::Int(5))))), 
          Box::new(Expr::Literal(Value::Int(5))),
        ),
        None
      ), 
      Some(Value::Int(1))
    );
    assert_eq!(
      eva.eval(
        Expr::BinaryExpression(
          "/".to_string(), 
          Box::new(Expr::Literal(Value::Int(0))),
          Box::new(Expr::Literal(Value::Int(3))), 
        ),
        None
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
          "/".to_string(), 
          Box::new(Expr::Literal(Value::Int(3))),
          Box::new(Expr::Literal(Value::Int(0))), 
        ),
        None
      ), 
      Some(Value::Int(0))
    );
  }

  #[test]
  fn variable_declaration() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::VariableDeclaration(
          "var".to_string(), 
          "x".to_string(), 
         Box::new(Expr::Literal(Value::Int(10)))
        ),
        None,
      ), 
      Some(Value::Int(10))
    );
    assert!(
      matches!(
        eva.global.borrow().lookup(&"x".to_string()),
        Ok(Value::Int(10))
      )
    );

    eva.eval(
      Expr::VariableDeclaration(
        "var".to_string(),
        "z".to_string(),
        Box::new(
          Expr::BinaryExpression(
            "+".to_string(),
            Box::new(Expr::Literal(Value::Int(2))),
            Box::new(Expr::Literal(Value::Int(4))),
        ),
      )),
      None,
    );
    assert!(
      matches!(
        eva.global.borrow().lookup(&"z".to_string()),
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
      "+".to_string(),
      Box::new(Expr::BinaryExpression(
        "*".to_string(), 
        Box::new(Expr::Identifier("x".to_string())),
        Box::new(Expr::Identifier("y".to_string())))
      ),
      Box::new(Expr::Literal(Value::Int(30))),
    );

    assert_eq!(
      eva.eval(
        Expr::BlockStatement("begin".to_string(), vec![expr1, expr2, expr3]), 
        None
      ),
      Some(Value::Int(230))
    )
  }

  #[test]
  fn nested_blocks() {
    let mut eva = Eva::new();

    let expr1 = Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(Expr::Literal(Value::Int(10))));
    let block_lvl_2 = Expr::BlockStatement("begin".to_string(), vec![
      Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(Expr::Literal(Value::Int(20)))),
      Expr::Identifier("x".to_string()),
    ]);
    let expr2 = Expr::Identifier("x".to_string());

    assert_eq!(
      eva.eval(
        Expr::BlockStatement("begin".to_string(), vec![expr1, block_lvl_2, expr2]),
        None,
      ),
      Some(Value::Int(10)),
    );
  }

  #[test]
  fn outer_scope_reference() {
    let mut eva = Eva::new();

    let expr1 = Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(Expr::Literal(Value::Int(10))));
    let expr2 = Expr::VariableDeclaration("var".to_string(), "y".to_string(), Box::new(
      Expr::BlockStatement("begin".to_string(), vec![
        Expr::Identifier("x".to_string())
      ]),
    ));
    let expr3 = Expr::Identifier("y".to_string());

    assert_eq!(
      eva.eval(Expr::BlockStatement("begin".to_string(), vec![expr1, expr2, expr3]), None),
      Some(Value::Int(10))
    )
  }

  #[test]
  fn scope_chain_traversal() {
    let mut eva = Eva::new();

    let inner_decl = Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(
      Expr::BinaryExpression(
        "+".to_string(), 
        Box::new(Expr::Identifier("value".to_string())), 
        Box::new(Expr::Literal(Value::Int(10)))
      )
    ));
    let nested_block = Expr::BlockStatement("begin".to_string(), vec![
      inner_decl,
      Expr::Identifier("x".to_string())
    ]);
    let decl_2 = Expr::VariableDeclaration("var".to_string(), "result".to_string(), Box::new(nested_block));
    let decl_1 = Expr::VariableDeclaration("var".to_string(), "value".to_string(), Box::new(Expr::Literal(Value::Int(10))));
    let outer_block = Expr::BlockStatement("begin".to_string(), vec![
      decl_1, 
      decl_2,
      Expr::Identifier("result".to_string())
    ]);

    assert_eq!(
      eva.eval(outer_block, None),
      Some(Value::Int(20)),
    )
  }

  #[test]
  fn same_scope_assignment() {
    let mut eva = Eva::new();
    
    assert_eq!(
      eva.eval(
        Expr::BlockStatement(
          "begin".to_string(), 
          vec![
            Expr::VariableDeclaration("var".to_string(), "data".to_string(), Box::new(Expr::Literal(Value::Int(10)))), 
            Expr::Assignment("set".to_string(), "data".to_string(), Box::new(Expr::Literal(Value::Int(100)))), 
            Expr::Identifier("data".to_string())
          ],
        ), 
        None
      ), 
      Some(Value::Int(100))
    );
  }

  #[test]
  fn outer_scope_assignment() {
    let mut eva = Eva::new();
    
    let outer_decl_1 = Expr::VariableDeclaration("var".to_string(), "data".to_string(), Box::new(Expr::Literal(Value::Int(10))));
    let inner_block = Expr::BlockStatement(
      "begin".to_string(), 
      vec![Expr::Assignment("set".to_string(), "data".to_string(), Box::new(Expr::Literal(Value::Int(100))))]
    );
    let id_reference = Expr::Identifier("data".to_string());
    let outer_block = Expr::BlockStatement("begin".to_string(), vec![outer_decl_1, inner_block, id_reference]);
    assert_eq!(eva.eval(outer_block, None), Some(Value::Int(100)));
  }

  #[test]
  fn if_expression() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BlockStatement(
          "begin".to_string(),
          vec![
            Expr::VariableDeclaration("var".to_string(), "x".to_string(), Box::new(Expr::Literal(Value::Int(10)))),
            Expr::VariableDeclaration("var".to_string(), "y".to_string(), Box::new(Expr::Literal(Value::Int(0)))),
            Expr::IfExpression(
              "if".to_string(), 
              Box::new(
                Expr::BinaryExpression(
                  ">".to_string(), 
                  Box::new(Expr::Identifier("x".to_string())),
                  Box::new(Expr::Literal(Value::Int(10)))
                ),
              ),
              Box::new(
                Expr::Assignment(
                  "set".to_string(), 
                  "y".to_string(), 
                  Box::new(Expr::Literal(Value::Int(20)))
                ),
              ),
              Box::new(
                Expr::Assignment(
                  "set".to_string(), 
                  "y".to_string(), 
                  Box::new(Expr::Literal(Value::Int(30)))
                ),
              ), 
            )
          ]
        ),
        None
      ),
      Some(Value::Int(30))
    )
  }

  #[test]
  fn while_statement() {
    let mut eva = Eva::new();

    assert_eq!(
      eva.eval(
        Expr::BlockStatement(
          "begin".to_string(),
          vec![
            Expr::VariableDeclaration(
              "var".to_string(),
              "counter".to_string(),
              Box::new(Expr::Literal(Value::Int(0)))
            ),
            Expr::VariableDeclaration(
              "var".to_string(),
              "result".to_string(),
              Box::new(Expr::Literal(Value::Int(0)))
            ),
            Expr::WhileStatement(
              "while".to_string(),
              Box::new(Expr::BinaryExpression(
                "<".to_string(),
                Box::new(Expr::Identifier("counter".to_string())),
                Box::new(Expr::Literal(Value::Int(10))),
              )),
              Box::new(Expr::BlockStatement(
                "begin".to_string(),
                vec![
                  Expr::Assignment(
                    "set".to_string(), 
                    "result".to_string(),
                    Box::new(Expr::BinaryExpression(
                      "+".to_string(), 
                      Box::new(Expr::Identifier("result".to_string())), 
                      Box::new(Expr::Literal(Value::Int(1))))
                    ), 
                  ),
                  Expr::Assignment(
                    "set".to_string(), 
                    "counter".to_string(),
                    Box::new(Expr::BinaryExpression(
                      "+".to_string(), 
                      Box::new(Expr::Identifier("counter".to_string())), 
                      Box::new(Expr::Literal(Value::Int(1))))
                    ), 
                  )
                ]
              ))
            ),
            Expr::Identifier("result".to_string())
          ]
        ), 
        None
      ),
      Some(Value::Int(10))
    )
  }
}
