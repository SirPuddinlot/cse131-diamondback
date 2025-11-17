// src/ast.rs
#[derive(Debug, Clone)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Print
}

#[derive(Debug, Clone)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal, 
    Greater, 
    GreaterEqual, 
    Less,
    LessEqual,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i32),
    Id(String),
    Input,
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Boolean(bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>),  
    Block(Vec<Expr>),  
    Set(String, Box<Expr>),  
    Loop(Box<Expr>),    
    Break(Box<Expr>),  
    Call(String, Vec<Expr>),  
}

#[derive(Debug)]
pub enum ReplEntry {
    Expr(Expr),
    Define(String, Box<Expr>),
    Fun(String, Vec<String>, Expr), // Add this variant for function definitions
    FunDefn(FunDefn),
}


// diamondback
#[derive(Debug)]
pub struct Program {
    pub defns: Vec<FunDefn>,
    pub main: Expr,
}

#[derive(Debug, Clone)]
pub struct FunDefn {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Expr>,
}