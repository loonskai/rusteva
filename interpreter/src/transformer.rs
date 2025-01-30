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
    let mut if_vec = vec![
      Rc::new(RefCell::new(ParsedExpr::String("if".to_string()))),
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // condition placeholder
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // consequent placeholder
      Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // alternate placeholder
    ];
    let mut current_if_vec = &mut if_vec;
    let cases_len = cases.len();
    for i in (0..cases_len - 1).step_by(2) {
      let ParsedExpr::List(ref current) = *cases[i].borrow() else {
        panic!("Invalid case statement")
      };
      let ParsedExpr::List(ref next) = *cases[i+1].borrow() else {
        panic!("Invalid case statement")
      };
      
      current_if_vec[1] = Rc::clone(&current[0]); // condition
      current_if_vec[2] = Rc::clone(&current[1]); // consequent

      let next_condition = next[0].borrow();
      if let ParsedExpr::Symbol(ref next_condition_symbol) = *next_condition {
        if next_condition_symbol == &"else".to_string() {
          current_if_vec[3] = Rc::clone(&next[1]); // next block
        } 
      } else {
        let new_if_vec = vec![
          Rc::new(RefCell::new(ParsedExpr::String("if".to_string()))),
          Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // condition placeholder
          Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // consequent placeholder
          Rc::new(RefCell::new(ParsedExpr::String("".to_string()))), // alternate placeholder
        ];
        let new_if_cell = Rc::new(RefCell::new(ParsedExpr::List(new_if_vec)));
        current_if_vec[3] = Rc::clone(&new_if_cell);
        // FIXME
        if let ParsedExpr::List(ref mut next_if_vec) = *new_if_cell.borrow_mut() {
          current_if_vec = next_if_vec;
        };
      }
    }
    ParsedExpr::List(if_vec)
  }
}
