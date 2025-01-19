pub mod environment;
pub mod error;

use std::rc::Rc;
use environment::Environment;

#[derive(Debug,PartialEq,Clone)]
pub struct Func {
    params: Vec<String>,
    body: Box<Expr>,
    env: Rc<Environment>
}

#[derive(Debug,PartialEq,Clone)]
pub enum Value {
    Int(isize),
    Str(String),
    Null,
    Boolean(bool),
    Function(Func)
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
    CallExpression(String, Vec<Expr>),
    FunctionDeclaration(String, Vec<Expr>, Box<Expr>)
}


