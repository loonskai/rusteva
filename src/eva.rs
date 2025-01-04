use std::fmt::{Debug};
use unicode_segmentation::{self, UnicodeSegmentation};

#[derive(PartialEq)]
#[derive(Debug)]
enum Exp {
  Int(isize),
  Str(String),
  Operation(char, Box<Exp>, Box<Exp>),
}

pub struct Eva {}

impl Eva {
  fn new() -> Self {
    Eva {}
  }

  fn eval(&self, exp: Exp) -> Option<Exp> {
    match exp {
        Exp::Operation(operator, op1,op2 ) => {
          match operator {
            '+' => {
              let n1 = self.eval(*op1).expect("Invalid operand.");
              let n2 = self.eval(*op2).expect("Invalid operand.");
              match (n1, n2) {
                (Exp::Int(num1), Exp::Int(num2)) => Some(Exp::Int(num1 + num2)),
                _ => None
              }
            },
            '*' => {
              let n1 = self.eval(*op1).expect("Invalid operand.");
              let n2 = self.eval(*op2).expect("Invalid operand.");
              match (n1, n2) {
                (Exp::Int(num1), Exp::Int(num2)) => Some(Exp::Int(num1 * num2)),
                _ => None
              }
            },
            _ => None
          }
        }
        Exp::Int(n) => {
          return Some(Exp::Int(n));
        },
        Exp::Str(s) => {
          if s.graphemes(true).nth(0).unwrap() == r#"""# && s.graphemes(true).last().unwrap() == r#"""# {
            let with_quotation_trimmed = s[1..s.len()-1].to_string();
            return Some(Exp::Str(with_quotation_trimmed));
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

    assert_eq!(eva.eval(Exp::Int(1)), Some(Exp::Int(1)));
    assert_eq!(eva.eval(Exp::Int(-10)), Some(Exp::Int(-10)));

    assert_eq!(eva.eval(Exp::Str(r#""hello""#.to_string())), Some(Exp::Str("hello".to_string())));
    assert_eq!(eva.eval(Exp::Str(r#""🎅 新年快乐!""#.to_string())), Some(Exp::Str("🎅 新年快乐!".to_string())));
  }

  #[test]
  fn addition() {
    let eva = Eva::new();

    assert_eq!(eva.eval(
      Exp::Operation('+', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
      Some(Exp::Int(5))
    );
    assert_eq!(eva.eval(
      Exp::Operation(
        '+', 
        Box::new(Exp::Operation('+', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
        Box::new(Exp::Int(3)),
      ),
    ), Some(Exp::Int(8)));
    assert_eq!(eva.eval(
      Exp::Operation(
        '+', 
        Box::new(Exp::Operation('+', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
        Box::new(Exp::Operation('+', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))),
      ),
    ), Some(Exp::Int(10)));
  }

  #[test]
  fn multiplication() {
    let eva = Eva::new();

    assert_eq!(eva.eval(
      Exp::Operation('*', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
      Some(Exp::Int(6))
    );
    assert_eq!(eva.eval(
      Exp::Operation(
        '*', 
        Box::new(Exp::Operation('*', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
        Box::new(Exp::Int(3)),
      ),
    ), Some(Exp::Int(18)));
    assert_eq!(eva.eval(
      Exp::Operation(
        '*', 
        Box::new(Exp::Operation('*', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))), 
        Box::new(Exp::Operation('*', Box::new(Exp::Int(3)), Box::new(Exp::Int(2)))),
      ),
    ), Some(Exp::Int(36)));
  }
}
