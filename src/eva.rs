use std::fmt::{Debug, Display};
use unicode_segmentation::{self, UnicodeSegmentation};

#[derive(PartialEq)]
#[derive(Debug)]
enum ExpressionType {
  Int(isize),
  Str(String)
}

impl Display for ExpressionType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
          ExpressionType::Int(num) => write!(f, "{num}"),
          ExpressionType::Str(s) => write!(f, "{s}"),
      }
  }
}

pub struct Eva {}

impl Eva {
  fn new() -> Self {
    Eva {}
  }

  fn eval(&self, exp: String) -> Option<ExpressionType> {
    if let Ok(num) = exp.parse::<isize>() {
      return Some(ExpressionType::Int(num));
    }

    if exp.graphemes(true).nth(0).unwrap() == r#"""# && 
      exp.graphemes(true).last().unwrap() == r#"""# {
        let with_quotation_trimmed = exp[1..exp.len()-1].to_string();
      return Some(ExpressionType::Str(with_quotation_trimmed));
    }

    return None;
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn self_evaluating_expressions() {
    let eva = Eva::new();

    assert_eq!(eva.eval("1".to_string()), Some(ExpressionType::Int(1)));
    assert_eq!(eva.eval("-10".to_string()), Some(ExpressionType::Int(-10)));

    assert_eq!(eva.eval(r#""hello""#.to_string().to_string()), Some(ExpressionType::Str("hello".to_string())));
    assert_eq!(eva.eval(r#""🎅 新年快乐!""#.to_string()), Some(ExpressionType::Str("🎅 新年快乐!".to_string())));
  }
}
