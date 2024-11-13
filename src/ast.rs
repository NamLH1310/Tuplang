use crate::ast::Operator::Unknown;
use crate::lexer::Token;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

enum Type {
    Int32,
    Int64,
    Float32,
    Float64,
    AnonymousTuple(Vec<Type>),
    NamedTuple(HashMap<String, Type>),
}

struct Param {
    name: String,
    typ: Type,
}

enum Decl {
    Var {
        name: String,
        expr: Expr,
    },
    Const {
        name: String,
        expr: Expr,
    },
    Func {
        name: String,
        params: Vec<Param>,
        rtype: Type,
        body: Stmts,
    },
    Type {
        name: String,
        d_type: Type,
    },
}

enum Stmt {
    Assign(String, Expr),
    Return(Expr),
    Expr(Expr),
    Block(Stmts),
    While {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    If {
        cond: Expr,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    For {
        init: Box<Stmt>,
        cond: Expr,
        every_loop: Box<Stmt>,
        body: Box<Stmt>,
    },
    Break,
    Continue,
}

type Stmts = Vec<Stmt>;

#[derive(Debug, PartialEq)]
pub(crate) enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    Not,
    Negate,
    And,
    Or,
    Modulo,
    Unknown,
}

impl From<&Token<'_>> for Operator {
    fn from(token: &Token) -> Self {
        use crate::lexer::Token::*;
        match token {
            Plus => Operator::Add,
            Minus => Operator::Subtract,
            Star => Operator::Multiply,
            Slash => Operator::Divide,
            Gt => Operator::Gt,
            Gte => Operator::Gte,
            Lt => Operator::Lt,
            Lte => Operator::Lte,
            Eq => Operator::Eq,
            Neq => Operator::Neq,
            And => Operator::And,
            Or => Operator::Or,
            Percent => Operator::Modulo,
            _ => Unknown,
        }
    }
}

impl From<&Token<'_>> for Expr {
    fn from(token: &Token) -> Self {
        use crate::lexer::Token::*;
        match *token {
            Identifier(s) => Expr::Id(s.to_owned()),
            IntLit(s) => Expr::Int(s.parse::<isize>().unwrap()),
            FloatLit(s) => {
                let v = s.parse::<f32>().unwrap();
                if v.is_finite() {
                    Expr::Float32(v)
                } else {
                    Expr::Float64(s.parse::<f64>().unwrap())
                }
            }
            StringLit(s) => Expr::String(
                s.replace("\\\"", "\"")
                    .replace("\\\\", "\\")
                    .replace("\\n", "\n")
                    .replace("\\r", "\r")
                    .replace("\\t", "\t")
                    .replace("\\0", "\0"),
            ),
            CharLit(s) => Expr::Char(s.chars().next().unwrap()),
            True => Expr::Bool(true),
            False => Expr::Bool(false),
            _ => Expr::Unknown,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Operator,
    },
    Unary {
        right: Box<Expr>,
        op: Operator,
    },
    SubScript {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    String(String),
    Char(char),
    Id(String),
    Bool(bool),
    Int(isize),
    Float64(f64),
    Float32(f32),
    Empty,
    Tuple(Vec<Expr>),
    Unknown,
}
