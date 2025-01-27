pub mod environment;
pub mod error;

use std::{cell::RefCell, rc::Rc};
use environment::Environment;

#[derive(Debug)]
pub enum ParsedExpr {
    Number(isize),
    String(String),
    Symbol(String),
    List(Vec<Rc<RefCell<ParsedExpr>>>)
}

#[derive(Debug,PartialEq,Clone)]
pub struct FuncObj {
    pub params: Vec<String>,
    pub body: Box<Expr>,
    pub env: Rc<RefCell<Environment>>
}

impl FuncObj {
    pub fn new(params: Vec<String>, body: Box<Expr>, env: Rc<RefCell<Environment>>) -> Self {
        FuncObj {
            params,
            body,
            env
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum Value {
    Int(isize),
    Str(String),
    Null,
    Boolean(bool),
    Function(FuncObj)
}

#[derive(Debug,PartialEq,Clone)]
pub enum Expr {
    Literal(Value),
    BinaryExpression(String, Box<Expr>, Box<Expr>),
    VariableDeclaration(String, Box<Expr>),
    Identifier(String),
    BlockStatement(Vec<Expr>),
    Assignment(String, Box<Expr>),
    IfExpression(Box<Expr>, Box<Expr>, Box<Expr>),
    WhileStatement(Box<Expr>, Box<Expr>),
    // CallExpression(String, Vec<Expr>),
    FunctionDeclaration(String, Vec<String>, Box<Expr>),
    LambdaExpression(Vec<String>, Box<Expr>),
    ApplyExpression(Box<Expr>, Vec<Expr>),
    SwitchStatement(Vec<Expr>),
}


