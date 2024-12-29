use std::fmt::{Debug};
use unicode_segmentation::{self, UnicodeSegmentation};

enum Input {
  Number(isize),
  String(String),
  Add(char, isize, isize),
}

#[derive(PartialEq)]
#[derive(Debug)]
enum ExpressionType {
  Int(isize),
  Str(String)
}

pub struct Eva {}

impl Eva {
  fn new() -> Self {
    Eva {}
  }

  fn eval(&self, exp: Input) -> Option<ExpressionType> {
    match exp {
        Input::Add(operator, op1,op2 ) => {
          if operator == '+' {
            return Some(ExpressionType::Int(op1 + op2));
          } else {
            return None;
          }
        }
        Input::Number(n) => {
          return Some(ExpressionType::Int(n));
        },
        Input::String(s) => {
          if s.graphemes(true).nth(0).unwrap() == r#"""# && s.graphemes(true).last().unwrap() == r#"""# {
            let with_quotation_trimmed = s[1..s.len()-1].to_string();
            return Some(ExpressionType::Str(with_quotation_trimmed));
          }
          return None
        },
    }
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn self_evaluating_expressions() {
    let eva = Eva::new();

    assert_eq!(eva.eval(Input::Number(1)), Some(ExpressionType::Int(1)));
    assert_eq!(eva.eval(Input::Number(-10)), Some(ExpressionType::Int(-10)));

    assert_eq!(eva.eval(Input::String(r#""hello""#.to_string())), Some(ExpressionType::Str("hello".to_string())));
    assert_eq!(eva.eval(Input::String(r#""🎅 新年快乐!""#.to_string())), Some(ExpressionType::Str("🎅 新年快乐!".to_string())));
  }
}
