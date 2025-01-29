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
    let if_symbol = Rc::new(RefCell::new(ParsedExpr::Symbol("if".to_string())));
    let mut if_expr: Vec<Rc<RefCell<ParsedExpr>>> = vec![Rc::clone(&if_symbol)];
    let current_vec = &mut if_expr;
    for (index, case_expr) in cases[0..cases.len()-1].iter().enumerate() {
      match &*case_expr.borrow() {
        ParsedExpr::List(case_tuple) => {
          let current_condition = &case_tuple[0];
          let current_block = &case_tuple[1];
          current_vec.push(Rc::clone(&current_condition));
          current_vec.push(Rc::clone(&current_block));
          let next_case_tuple = Rc::clone(&cases[index+1]);
          match &*next_case_tuple.borrow() {
            ParsedExpr::List(next_case) => {
              let next_condition = &next_case[0];
              let next_block = &next_case[1];
              let alternate_expr = match &*next_condition.borrow() {
                  ParsedExpr::Symbol(symbol) => {
                    if *symbol == "else".to_string() {
                      Rc::clone(next_block)
                    } else {
                      Rc::new(RefCell::new(ParsedExpr::List(vec![Rc::clone(&if_symbol)])))
                    }
                  },
                  _ => Rc::new(RefCell::new(ParsedExpr::List(vec![Rc::clone(&if_symbol)])))
              };
              // FIXME: How to change the reference?
              let nested_if_rc = vec![alternate_expr];
              *current_vec = vec![Rc::clone(&current_vec[3])];
            },
            _ => panic!("Invalid case statement")
          };
        },
        _ => panic!("Invalid case statement")
      }
    }
    return ParsedExpr::List(if_expr);
  }
}
