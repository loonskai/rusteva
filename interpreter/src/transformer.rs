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

  // pub fn transform_switch_to_if(&self, switch_expr: Expr) -> Expr {
  //   if let Expr::SwitchStatement(expressions) = switch_expr {
  //     let mut current_condition_expr: Expr;
  //     let mut current_consequent_expr: Expr;
  //     for expr in expressions {
  //       if let Expr::SwitchCase(condition_expr, consequent_expr) = expr {
  //         current_condition_expr = condition_expr;
  //         current_consequent_expr = consequent_expr;
  //       } else {
  //         panic!("Invalid case expression")
  //       }
  //     }
  //   } else {
  //     panic!("Invalid switch expression")
  //   }
  // }
}
