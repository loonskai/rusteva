use std::{cell::RefCell, fmt::Debug, rc::Rc};
use common::{environment::Environment, Expr, FuncObj, ParsedExpr, Value};
use unicode_segmentation::{self, UnicodeSegmentation};

use crate::transformer::Transformer;

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
          let n1 = "n1".to_string();
          let n2 = "n2".to_string();
          let _ = $global.borrow_mut().define(&$op.to_string(), Value::Function(FuncObj::new(
            vec![n1.clone(), n2.clone()],
            Box::new(Expr::BinaryExpression($op.to_string(), Box::new(Expr::Identifier(n1)), Box::new(Expr::Identifier(n2)))),
            Rc::clone(&$global)
            )
          ));
        )*
    };
}

#[derive(Debug)]
pub struct Eva {
  pub global_env: Rc<RefCell<Environment>>,
  pub execution_stack: Vec<Rc<RefCell<Environment>>>,
  transformer: Transformer
}

impl Eva {
  // Create global environment
  // Predefined values: null, true, false, math operations
  pub fn new() -> Self {
    let global_env = Rc::new(RefCell::new(Environment::new(None)));
    define_global_variables!(
      global_env, 
      ("null", Value::Null), 
      ("true", Value::Boolean(true)), 
      ("false", Value::Boolean(false))
    );
    define_binary_operators!(global_env, "+", "-", "*", "/",  ">", "<", ">=", "<=", "==");
    let execution_stack: Vec<Rc<RefCell<Environment>>> = vec![];
    let transformer = Transformer::new();
    Eva { global_env, execution_stack, transformer }
  }

  pub fn eval(&mut self, parsed_exp: Rc<RefCell<ParsedExpr>>, current_env: Option<Rc<RefCell<Environment>>>) -> Option<Value> {
    let env = current_env.map_or(Rc::clone(&self.global_env), |e| e);
    let expr = self.eval_parsed_expr(parsed_exp, Rc::clone(&env));
    self.eval_expr(expr, Rc::clone(&env))
  }

  fn eval_expr(&mut self, expr: Expr, env: Rc<RefCell<Environment>>) -> Option<Value> {
    match expr {
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
          Value::Env(e) => Some(Value::Env(e)),
          Value::Null => Some(Value::Null)
        }
      },
      Expr::Identifier(id) => {
        match env.borrow().lookup(&id) {
          Ok(value) => Some(value.clone()),
          Err(err) => panic!("{:?}", err)
        }
      },
      Expr::BinaryExpression(operator, exp1, exp2 ) => {
        let v1 = self.eval_expr(*exp1, Rc::clone(&env)).expect("Invalid operand");
        let v2 = self.eval_expr(*exp2, Rc::clone(&env)).expect("Invalid operand");
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
      },
      Expr::BlockStatement(expressions) => {
        let block_env = Rc::new(RefCell::new(Environment::new(Some(env))));
        let mut result = None;
        for expr in expressions {
          result = self.eval_expr(expr, Rc::clone(&block_env));
        }
        result
      },
      Expr::VariableDeclaration(id_name, exp) => {
        if let Some(value) = self.eval_expr(*exp, Rc::clone(&env)) {
          if let Ok(result) = env.borrow_mut().define(&id_name, value) {
            return Some(result.clone())
          }
        }
        panic!("Unable to declare variable")
      },
      Expr::Assignment(id, expr) => {
        let result = self.eval_expr(*expr, Rc::clone(&env)).map_or(Value::Null, |value| value);
        let _ = env.borrow_mut().set(&id, result.clone());
        Some(result)
      },
      Expr::IfExpression(condition_expr, consequent_expr, alternate_exp) => {
        let result = self.eval_expr(*condition_expr, Rc::clone(&env));
        if let Some(cond) = result {
          match cond {
              Value::Boolean(value) => {
                if value {
                  return self.eval_expr(*consequent_expr, Rc::clone(&env));
                } else {
                  return self.eval_expr(*alternate_exp, Rc::clone(&env));
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
        let mut condition_result = self.eval_expr(condition.clone(), Rc::clone(&env));
        let mut consequent_result = None;
        while let Some(true) = condition_result.as_ref().map(|v| {
          match v {
              Value::Boolean(bool_result) => bool_result,
              _ => panic!("While condition must return boolean")
          }
        }) {
          // Q: Why the order of both evals matters?
          consequent_result = self.eval_expr(consequent.clone(), Rc::clone(&env));
          condition_result = self.eval_expr(condition.clone(), Rc::clone(&env));
        }
        consequent_result
      },
      Expr::LambdaExpression(params, body) => {
        let func_obj = FuncObj::new(
          params,
          body,
          Rc::clone(&env),
        );
        Some(Value::Function(func_obj))
      },
      Expr::FunctionDeclaration(func_name, func_params, func_body) => {
        // JIT-transpile to a variable declaration
        let var_expr = self.transformer.transform_def_to_var_lambda(func_name, func_params, func_body);
        self.eval_expr(
          var_expr,
          Rc::clone(&env) // Closure
        )
      },
      Expr::ApplyExpression(func_expr, args) => {
        self.apply(*func_expr, args, Rc::clone(&env))
      },
      Expr::ClassDeclaration(class_name, parent_class_expr, class_body) => {
        let parent_env = self.eval_expr(*parent_class_expr, Rc::clone(&env))
          .map(|parent_class| match parent_class {
            Value::Function(parent_func_obj) => parent_func_obj.env,
            Value::Null => Rc::clone(&env),
            _ => panic!("Invalid parent class value. Must be a function object or null.")
          })
          .unwrap_or_else(|| Rc::clone(&env));
        let class_env = Rc::new(RefCell::new(Environment::new(Some(parent_env))));
        // Set methods in class environment without executing them
        if let Expr::BlockStatement(method_func_decl_expressions) = *class_body {
          for class_method_expr in method_func_decl_expressions {
            if let Expr::FunctionDeclaration(func_name, func_args, func_body) = class_method_expr {
              class_env
                .borrow_mut()
                .define(
                  &func_name.to_string(), 
                  Value::Function(FuncObj { 
                    params: func_args, 
                    body: func_body, 
                    env: Rc::clone(&class_env) 
                  }))
                .expect("Unable to define method on class");
            }
            // self.eval_expr(class_method_expr, Rc::clone(&class_env)).expect("Cannot evaluate method definition");
          }
        } else {
          panic!("Class desclaration body must be a block")
        }
        class_env.borrow().lookup(&"constructor".to_string()).expect("Class declaration must have constructor method");
        let class_env_value = Value::Env(Rc::clone(&class_env));
        env.borrow_mut().define(&class_name, class_env_value).expect("Cannot define class in current env");
        None
      },
      Expr::NewExpression(class_name, mut args) => {
        println!("class_name: {:?}", class_name);
        println!("args: {:?}", args);
        if let Value::Env(class_env)  = self.eval_expr(Expr::Identifier(class_name), Rc::clone(&env)).expect("Class is not defined") {
          let instance_env = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&class_env)))));
          let mut args_with_self = vec![Expr::Literal(Value::Env(instance_env))];
          args_with_self.append(&mut args);
          self.apply(Expr::Identifier("constructor".to_string()), args_with_self, class_env)
        } else {
          panic!("Invalid class value")
        }
      },
      Expr::MemberExpression(instance_name, id) => {
        if let Value::Env(instance_env) = self.eval_expr(Expr::Identifier(instance_name), Rc::clone(&env)).expect("Instance not founc") {
          match instance_env.borrow().lookup(&id) {
            Ok(value) => Some(value.clone()),
            Err(err) => panic!("{:?}", err)
          }
        } else {
          panic!("Invalid instance value")
        }
      }
    }
  }

  fn eval_parsed_expr(&mut self, parsed_exp: Rc<RefCell<ParsedExpr>>, env: Rc<RefCell<Environment>>) -> Expr {
    match &*parsed_exp.borrow() {
      ParsedExpr::Number(n) => Expr::Literal(Value::Int(*n)),
      ParsedExpr::String(s) => Expr::Literal(Value::Str(s.to_string())),
      ParsedExpr::Symbol(symbol) => Expr::Identifier(symbol.to_string()),
      ParsedExpr::List(expr_list) => {
        match &*expr_list[0].borrow() {
          ParsedExpr::Symbol(symbol) => {
            match symbol.as_str() {
              "begin" => {
                let body_expressions: Vec<Expr> = expr_list[1..]
                  .iter()
                  .map(|expr| self.eval_parsed_expr(Rc::clone(&expr), Rc::clone(&env)))
                  .collect();
                return Expr::BlockStatement(body_expressions);
              },
              "var" => {
                let var_name_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                if let Expr::Identifier(var_name) = var_name_expr {
                  let var_value_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                  return Expr::VariableDeclaration(var_name, Box::new(var_value_expr));
                }
                panic!("Invalid variable declaration")
              },
              "set" => {
                match &*expr_list[1].borrow() {
                  ParsedExpr::Symbol(var_name) => {
                    let value_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                    return Expr::Assignment(var_name.clone(), Box::new(value_expr));
                  },
                  ParsedExpr::List(parsed_expr_list) => {
                    let prop_tag_expr = self.eval_parsed_expr(Rc::clone(&parsed_expr_list[0]), Rc::clone(&env));
                    if let Expr::Literal(Value::Str(prop_tag)) = prop_tag_expr {
                      if prop_tag != "prop".to_string() {
                        panic!("Invalid member expression")
                      }
                    }
                    let self_tag_expr = self.eval_parsed_expr(Rc::clone(&parsed_expr_list[1]), Rc::clone(&env));
                    let prop_id_expr = self.eval_parsed_expr(Rc::clone(&parsed_expr_list[2]), Rc::clone(&env));
                    println!("self_tag_expr: {:?}", self_tag_expr);
                    println!("prop_id_expr: {:?}", prop_id_expr);
                    Expr::Literal(Value::Null)
                       // (prop this foo)
                        // this
                        // let value_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                        // foo
                        // let instance_id_expr = self.eval_parsed_expr(Rc::clone(&parsed_expr_list[1]), Rc::clone(&env));
                        // if let Value::Env(instance_env) = self.eval_expr(instance_id_expr, env).expect("Instance not found") {
                        // if let Expr::Literal(instance_member_expr) = self.eval_parsed_expr(Rc::clone(&parsed_expr_list[2]), Rc::clone(&instance_env)) {
                          // if let Value::Str(instance_member_name) = instance_member_expr {
                            // let value = self.eval_expr(value_expr, Rc::clone(&instance_env)).expect("Invalid member value");
                            // let foo = instance_env.borrow_mut().set(&instance_member_name, value).ok().expect("Unable to set member value");
                            // return Expr::Literal(foo);
                            // return Expr::Assignment(instance_member_name.clone(), Box::new(value_expr), Rc::clone(&instance_env));
                          // } else {
                            // panic!("Invalid assignment")
                          // }
                        // } else {
                        //   panic!("Invalid assignment")
                        // }
                        // } else {
                        //   panic!("Invalid assignment: Expr::Literal(instance_value)")
                        // }
                    // } else {
                      // panic!("Invalid assignment: Expr::Identifier(parsed_id)")
                    // }
                  },
                  _ => panic!("Invalid assignment: invalid expr_list[1]")
                }
              },
              "if" => {
                let condition_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                let consequent_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                let alternate_expr = self.eval_parsed_expr(Rc::clone(&expr_list[3]), Rc::clone(&env));
                Expr::IfExpression(Box::new(condition_expr), Box::new(consequent_expr), Box::new(alternate_expr))
              },
              "switch" => {
                let if_parsed_expr = self.transformer.transform_switch_to_if(expr_list[1..].to_vec());
                self.eval_parsed_expr(Rc::new(RefCell::new(if_parsed_expr)), env)
              },
              "while" => {
                let condition_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                let consequent_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                Expr::WhileStatement(Box::new(condition_expr), Box::new(consequent_expr))
              },
              "for" => {
                let while_parsed_expr = self.transformer.transform_for_to_while(expr_list[1..].to_vec());
                self.eval_parsed_expr(Rc::new(RefCell::new(while_parsed_expr)), env)
              },
              "lambda" => {
                if let ParsedExpr::List(ref params_parsed_expr) = *expr_list[1].borrow_mut() {
                  let func_params = self.transformer.transform_parsed_params_to_strings(&params_parsed_expr);
                  let func_body_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                  return Expr::LambdaExpression(func_params, Box::new(func_body_expr));
                }
                panic!("Invalid lambda expression")
              },
              "def" => {
                let func_name_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                if let Expr::Identifier(func_name) = func_name_expr {
                  if let ParsedExpr::List(ref params_parsed_expr) = *expr_list[2].borrow() {
                    let func_params = self.transformer.transform_parsed_params_to_strings(&params_parsed_expr);
                    let func_body_expr = self.eval_parsed_expr(Rc::clone(&expr_list[3]), Rc::clone(&env));
                    return Expr::FunctionDeclaration(func_name, func_params, Box::new(func_body_expr));
                  }
                }
                panic!("Invalid function declaration")
              },
              "class" => {
                let class_name_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                let parent_class_name_expr = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env));
                let class_body_expr = self.eval_parsed_expr(Rc::clone(&expr_list[3]), Rc::clone(&env));
                if let Expr::Identifier(class_name ) = class_name_expr {
                  return Expr::ClassDeclaration(class_name, Box::new(parent_class_name_expr), Box::new(class_body_expr));
                }
                panic!("Invalid class declaration!")
              },
              "new" => {
                let class_name_expr = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env));
                if let Expr::Identifier(class_name ) = class_name_expr {
                  let arg_expressions: Vec<Expr> = expr_list[2..]
                    .iter()
                    .map(|expr| self.eval_parsed_expr(Rc::clone(&expr), Rc::clone(&env)))
                    .collect();
                  return Expr::NewExpression(class_name, arg_expressions);
                }
                panic!("Invalid class instantiation!")
              },
              "prop" => {
                if let Expr::Identifier(instance_name) = self.eval_parsed_expr(Rc::clone(&expr_list[1]), Rc::clone(&env)) {
                  if let Expr::Identifier(id) = self.eval_parsed_expr(Rc::clone(&expr_list[2]), Rc::clone(&env)) {
                    return Expr::MemberExpression(instance_name, id);
                  }
                }
                panic!("Invalid member expression!")
              },
              "++" => {
                let assignment_expr = self.transformer.transform_math_to_assignment(
                  Rc::clone(&expr_list[1]),
                  "+".to_string()
                );
                self.eval_parsed_expr(Rc::new(RefCell::new(assignment_expr)), env)
              },
              "--" => {
                let assignment_expr = self.transformer.transform_math_to_assignment(
                  Rc::clone(&expr_list[1]),
                  "-".to_string()
                );
                self.eval_parsed_expr(Rc::new(RefCell::new(assignment_expr)), env)
              },
              _ => self.call(&expr_list, Rc::clone(&env))
            }
          },
          _ => self.call(&expr_list, Rc::clone(&env))
          }
        }
      }
  }

  fn call(&mut self, expr_list: &Vec<Rc<RefCell<ParsedExpr>>>, env: Rc<RefCell<Environment>>) -> Expr {
    let func_expr = self.eval_parsed_expr(Rc::clone(&expr_list[0]), Rc::clone(&env));
    let args_expr: Vec<Expr> = 
      expr_list[1..]
      .iter()
      .map(|arg_expr| self.eval_parsed_expr(Rc::clone(&arg_expr), Rc::clone(&env)))
      .collect();
    Expr::ApplyExpression(Box::new(func_expr), args_expr)
  }

  fn apply(&mut self, expr: Expr, args: Vec<Expr>, env: Rc<RefCell<Environment>>) -> Option<Value> {
    let func_obj = self.eval_expr(expr, Rc::clone(&env))
      .map(|v| match v {
        Value::Function(f) => f,
        _ => panic!("Invalid function call"),
      }).expect("Expected a function");
    if args.len() != func_obj.params.len() {
      panic!("Function arguments mismatch!")
    }
    let activation_env = Rc::new(RefCell::new(Environment::new(Some(func_obj.env))));
    // TODO: How to implement "debugger" Expr::Breakpoint and test execution stack? Also display stack trace on exceptions
    self.execution_stack.push(Rc::clone(&activation_env));
    for (index, param_name) in func_obj.params.iter().enumerate() {
      let evaluated_arg = self.eval_expr(args[index].clone(), Rc::clone(&env)).expect("Unable to evaluate argument");
      let _ = activation_env.borrow_mut().define(param_name, evaluated_arg).expect("Parameter definition failed");
    }
    let result = self.eval_expr(*func_obj.body, Rc::clone(&activation_env));
    self.execution_stack.pop();
    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use syntax::Parser;

  // #[test]
  // fn self_evaluating_expressions() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("null"), None),
  //     Some(Value::Null)
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse("true"), None),
  //     Some(Value::Boolean(true))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse("false"), None),
  //     Some(Value::Boolean(false))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse("1"), None),
  //     Some(Value::Int(1))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse("-10"), None),
  //     Some(Value::Int(-10))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse(r#""hello""#), None),
  //     Some(Value::Str("hello".to_string()))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse(r#""🎅 新年快乐!""#), None),
  //     Some(Value::Str("🎅 新年快乐!".to_string()))
  //   );
  // }

  // #[test]
  // fn built_in_functions() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   // Math functions:
  //   assert_eq!(eva.eval(parser.parse("(+ 1 5)"), None), Some(Value::Int(6)));
  //   assert_eq!(eva.eval(parser.parse("(+ (+ 2 3) 5)"), None), Some(Value::Int(10)));
  //   assert_eq!(eva.eval(parser.parse("(+ (* 2 3) 5)"), None), Some(Value::Int(11)));
  //   assert_eq!(eva.eval(parser.parse("(+ (/ 10 2) 5)"), None), Some(Value::Int(10)));

  //   // Comparison functions:
  //   assert_eq!(eva.eval(parser.parse("(> 1 5)"), None), Some(Value::Boolean(false)));
  //   assert_eq!(eva.eval(parser.parse("(< 1 5)"), None), Some(Value::Boolean(true)));
  //   assert_eq!(eva.eval(parser.parse("(>= 1 5)"), None), Some(Value::Boolean(false)));
  //   assert_eq!(eva.eval(parser.parse("(<= 1 5)"), None), Some(Value::Boolean(true)));
  //   assert_eq!(eva.eval(parser.parse("(== 5 5)"), None), Some(Value::Boolean(true)));
  // }

  // #[test]
  // #[should_panic]
  // fn division_by_zero() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("(/ 1 0)"), None), Some(Value::Int(0)));
  // }

  // #[test]
  // fn variable_declaration() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("(var x 10)"), None),
  //     Some(Value::Int(10))
  //   );
  //   assert_eq!(
  //     eva.eval(parser.parse("x"), None),
  //     Some(Value::Int(10))
  //   );
  //   eva.eval(parser.parse("(var z (+ 2 4))"), None);
  //   assert_eq!(
  //     eva.eval(parser.parse("z"), None),
  //     Some(Value::Int(6))
  //   );
  // }

  // #[test]
  // fn block_statement() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var x 10)
  //         (var y 20)
  //         (+ (* x y) 30)
  //       )
  //     "),
  //       None
  //     ),
  //     Some(Value::Int(230))
  //   );
  // }

  // #[test]
  // fn nested_blocks() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var x 10)
  //         (begin
  //           (var x 20))
  //         x
  //       )
  //     "), None),
  //     Some(Value::Int(10)),
  //   );
  // }

  // #[test]
  // fn outer_scope_reference() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var x 10)
  //         (var y (begin
  //           x))
  //         y
  //       )
  //     "), None),
  //     Some(Value::Int(10))
  //   )
  // }

  // #[test]
  // fn scope_chain_traversal() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var value 10)
  //         (var result (begin
  //           (var x (+ value 10))
  //           x)
  //         )
  //         result
  //       )
  //     "), None),
  //     Some(Value::Int(20)),
  //   )
  // }

  // #[test]
  // fn same_scope_assignment() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var data 10)
  //         (set data 100)
  //         data
  //       )
  //     "), None),
  //     Some(Value::Int(100))
  //   );
  // }

  // #[test]
  // fn outer_scope_assignment() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var data 10)
  //         (begin
  //           (set data 100)
  //         )
  //         data
  //       )
  //     "), None),
  //     Some(Value::Int(100))
  //   );
  // }

  // #[test]
  // fn if_expression() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var x 10)
  //         (var y 0)
  //         (if
  //           (> x 10)
  //           (set y 20)
  //           (set y 30)
  //         )
  //       )
  //     "), None),
  //     Some(Value::Int(30))
  //   )
  // }

  // #[test]
  // fn while_statement() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(
  //     eva.eval(parser.parse("
  //       (begin
  //         (var counter 0)
  //         (var result 0)
  //         (while
  //           (< counter 10)
  //           (begin
  //             (set counter (+ counter 1))
  //             (set result (+ result 1))
  //           )
  //         )
  //         result
  //       )
  //     "), None),
  //     Some(Value::Int(10))
  //   )
  // }

  // #[test]
  // fn user_defined_functions() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin 
  //       (def square (x) (* x x))
  //       (square 2)
  //     )
  //     "), None),
  //     Some(Value::Int(4))
  //   );
  //   assert_eq!(eva.eval(parser.parse("
  //     (begin 
  //       (def calc (x y) 
  //         (begin
  //           (var z 30)
  //           (+ (* x y) z)
  //         )
  //       )
  //       (calc 10 20)
  //     )
  //     "), None),
  //     Some(Value::Int(230))
  //   );
  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (var value 100)
  //       (def calc (x y) 
  //         (begin
  //           (var z (+ x y))
  //           (def inner (foo)
  //             (+ (+ foo z) value)
  //           )
  //           inner
  //         )
  //       )
  //       (var fn (calc 10 20))
  //       (fn 30)
  //     )
  //     "), None),
  //     Some(Value::Int(160))
  //   );
  // }

  // #[test]
  // fn lambda_functions() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (def onClick (callback) 
  //         (begin
  //           (var x 10)
  //           (var y 20)
  //           (callback (+ x y))
  //         )
  //       )
  //       (onClick (lambda (data) (* data 10)))
  //     )
  //   "), None), Some(Value::Int(300)))
  // }

  // #[test]
  // fn immediately_invoked_lambda_expression() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("((lambda (x) (* x x)) 2)"), None), Some(Value::Int(4)));
  // }

  // #[test]
  // fn lambda_assignment() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (var square (lambda (x) (* x x)))
  //       (square 2)
  //     )
  //   "), None), Some(Value::Int(4)));
  // }

  // #[test]
  // fn recursive_function() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (def factorial (x)
  //         (if (== x 1)
  //           1
  //           (* x (factorial (- x 1)))
  //         )
  //       )
  //       (factorial 5)
  //     )
  //   "), None), Some(Value::Int(120)))
  // }

  // #[test]
  // fn switch_statement() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (var x 10)
  //       (switch ((== x 10) 100)
  //               ((> x 10)  200)
  //               (else      300))
  //     )
  //   "), None), Some(Value::Int(100)))
  // }

  // #[test]
  // fn for_loop() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //   (begin
  //       (var result 0)
  //       (for (var counter 0) (< counter 10) (++ counter)
  //         (++ result 1)
  //       )
  //       result
  //     )
  //   "), None), Some(Value::Int(10)))
  // }

  // #[test]
  // fn increment_operator() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (var result 0)
  //       (++ result)
  //       result
  //     )
  //   "), None), Some(Value::Int(1)))
  // }

  // #[test]
  // fn decrement_operator() {
  //   let mut eva = Eva::new();
  //   let mut parser = Parser::new();

  //   assert_eq!(eva.eval(parser.parse("
  //     (begin
  //       (var result 0)
  //       (-- result)
  //       result
  //     )
  //   "), None), Some(Value::Int(-1)))
  // }

  #[test]
  fn classes() {
    let mut eva = Eva::new();
    let mut parser = Parser::new();

    assert_eq!(eva.eval(parser.parse("
      (begin
        (class Point null
          (begin
            (def constructor (this x y)
              (begin
                (set (prop this x) x)
                (set (prop this y) y)
              )
            )
          )
        )

        (var p (new Point 10 20))
      )
    "), None), Some(Value::Int(30)))
  }
}
