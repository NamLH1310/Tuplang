use std::collections::HashMap;

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
        typ: Type,
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
        otherwise: Option<Stmt>,
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

enum Operator {
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
}

enum Expr {
    Infix {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Operator,
    },
    Prefix {
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
}
