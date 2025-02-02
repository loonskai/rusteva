use std::{cell::RefCell, rc::Rc};

use common::{Expr, ParsedExpr};

#[derive(Debug)]
pub struct Transformer {}

impl Transformer {
  pub fn new() -> Self {
    Transformer {}
  }

  pub fn transform_parsed_params_to_strings(&self, params_parsed_expr: &Vec<Rc<RefCell<ParsedExpr>>>) -> Vec<String> {
    params_parsed_expr
      .iter()
      .map(|param_parsed_exp| {
        match &*param_parsed_exp.borrow() {
            ParsedExpr::Symbol(param) => param.clone(),
            _ => panic!("Function parameter must be a symbol")
        }
      })
      .collect()
  }

  pub fn transform_def_to_var_lambda(&self, def_expr: Expr) -> Expr {
    if let Expr::FunctionDeclaration(func_name, params, body) = def_expr {
      Expr::VariableDeclaration(func_name, Box::new(Expr::LambdaExpression(params, body)))
    } else {
      panic!("Invalid def expression")
    }
  }

  pub fn transform_switch_to_if(&self, cases: Vec<Rc<RefCell<ParsedExpr>>>) -> ParsedExpr {
    let if_vec = vec![
      Rc::new(RefCell::new(ParsedExpr::Symbol("if".to_string()))),
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // condition placeholder
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // consequent placeholder
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // alternate placeholder
    ];
    let root_if_expr = Rc::new(RefCell::new(ParsedExpr::List(if_vec)));
    let mut current_if_expr = Rc::clone(&root_if_expr);
    let cases_len = cases.len();
    for i in (0..cases_len - 1).step_by(2) {
      // (switch <current> <next> ...)
      let ParsedExpr::List(ref current) = *cases[i].borrow() else {
        panic!("Invalid case statement")
      };
      let ParsedExpr::List(ref next) = *cases[i+1].borrow() else {
        panic!("Invalid case statement")
      };
      if let ParsedExpr::List(ref mut current_if_vec) = *current_if_expr.borrow_mut() {
        current_if_vec[1] = Rc::clone(&current[0]); // condition -> (if (condition) (???) (???))
        current_if_vec[2] = Rc::clone(&current[1]); // consequent -> (if (condition) (consequent) (???))
      }
      let next_condition = next[0].borrow();
      if let ParsedExpr::Symbol(ref next_condition_symbol) = *next_condition {
        if next_condition_symbol == "else" {
          if let ParsedExpr::List(ref mut current_if_vec) = *current_if_expr.borrow_mut() {
            current_if_vec[3] = Rc::clone(&next[1]); // alternate -> (if (condition) (consequent) (alternate))
            break;
          }
        }
      }
      // Start building nested "if" as an alternate for current condition
      let new_if_vec = vec![
        Rc::new(RefCell::new(ParsedExpr::Symbol("if".to_string()))),
        Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // condition placeholder
        Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // consequent placeholder
        Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // alternate placeholder
      ];
      let new_if_expr = Rc::new(RefCell::new(ParsedExpr::List(new_if_vec)));
      if let ParsedExpr::List(ref mut current_if_vec) = *current_if_expr.borrow_mut() {
        current_if_vec[3] = Rc::clone(&new_if_expr); // alternate -> (if (condition) (consequent) (if (???) (???) (???)))
      }
      current_if_expr = Rc::clone(&new_if_expr);
    }
    ParsedExpr::List(Transformer::clone_list(&root_if_expr))
  }

  pub fn transform_for_to_while(&self, expressions: Vec<Rc<RefCell<ParsedExpr>>>) -> ParsedExpr {
    if expressions.len() != 4 {
      panic!("Invalid for expression. Requires init, condition, modifier and expression")
    }
    ParsedExpr::List(vec![
      Rc::new(RefCell::new(ParsedExpr::Symbol("begin".to_string()))),
      Rc::clone(&expressions[0]), // init
      Rc::new(RefCell::new(ParsedExpr::List(vec![
        Rc::new(RefCell::new(ParsedExpr::Symbol("while".to_string()))),
        Rc::clone(&expressions[1]), // condition
        Rc::new(RefCell::new(ParsedExpr::List(vec![
          Rc::new(RefCell::new(ParsedExpr::Symbol("begin".to_string()))),
          Rc::clone(&expressions[3]), // expression
          Rc::clone(&expressions[2]), // modifier
        ]))),
      ]))),
    ])
  }

  pub fn transform_math_to_assignment(&self, identifier_expr: Rc<RefCell<ParsedExpr>>, operator: String) -> ParsedExpr {
    ParsedExpr::List(vec![
      Rc::new(RefCell::new(ParsedExpr::Symbol("set".to_string()))),
      Rc::clone(&identifier_expr),
      Rc::new(RefCell::new(ParsedExpr::List(vec![
        Rc::new(RefCell::new(ParsedExpr::Symbol(operator))),
        Rc::clone(&identifier_expr),
        Rc::new(RefCell::new(ParsedExpr::Number(1)))
      ]))),
    ])
  }

  pub fn clone_list(expr: &Rc<RefCell<ParsedExpr>>) -> Vec<Rc<RefCell<ParsedExpr>>> {
    if let ParsedExpr::List(ref expr_list) = *expr.borrow() {
      expr_list.iter().map(|expr| Rc::clone(expr)).collect()
    } else {
      panic!("Expected ParsedExpr::List")
    }
  }
}
