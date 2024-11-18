use crate::ast::Expr::FuncCall;
use crate::ast::{Decl, Expr, Operator, Stmt, Type};
use crate::lexer::{Token, Tokenizer};
use std::boxed::Box;
use std::collections::HashMap;

impl Token<'_> {
    #[inline]
    fn precedence(&self) -> i8 {
        match self {
            Token::Or | Token::And => 1,
            Token::Lt | Token::Gt | Token::Lte | Token::Gte => 2,
            Token::Eq | Token::Neq => 3,
            Token::Plus | Token::Minus => 4,
            Token::Slash | Token::Star | Token::Percent => 5,
            Token::Dot => 6,
            Token::OpenParen => 7,
            Token::OpenBracket => 8,
            _ => 0,
        }
    }

    #[inline]
    fn is_binary(&self) -> bool {
        use crate::lexer::Token::*;
        match self {
            Plus | Minus | Star | Slash | Gt | Gte | Lt | Lte | Eq | Neq => true,
            _ => false,
        }
    }

    fn is_atomic(&self) -> bool {
        use crate::lexer::Token::*;
        match self {
            Identifier(_) | IntLit(_) | FloatLit(_) | StringLit(_) | CharLit(_) | True | False => {
                true
            }
            _ => false,
        }
    }

    fn is_builtin_type(&self) -> bool {
        match self {
            Self::Int | Self::Float32 | Self::Float64 | Self::String | Self::Char => true,
            _ => false,
        }
    }
}

pub(crate) struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    next_token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        let mut tokenizer = Tokenizer::new(src);
        let next_token = tokenizer.next().unwrap_or(Token::Eof);
        Parser {
            tokenizer,
            next_token,
        }
    }

    fn advance(&mut self) {
        self.next_token = self.tokenizer.next().unwrap_or(Token::Eof);
    }

    fn parse_program(&mut self) -> Vec<Decl> {
        let mut decls = Vec::<Decl>::new();
        loop {
            match self.next_token {
                Token::Identifier(id) => {
                    self.advance();
                    let parse_result = match self.next_token {
                        Token::Walrus => {
                            self.advance();
                            self.parse_var_decl(id.to_owned())
                        }
                        Token::Colon => {
                            self.advance();
                            self.parse_func_decl()
                        }
                        _ => {
                            eprintln!("Expect :=, :, got {:?}", &self.next_token);
                            None
                        }
                    };
                    parse_result.map(|decl| decls.push(decl));
                }
                Token::Type => {
                    self.advance();
                    self.parse_type_decl().map(|decl| decls.push(decl));
                }
                Token::Eof => {
                    break;
                }
                ref token => {
                    eprintln!("Expect type, <identifier>, got: {:?}", token);
                    self.advance();
                }
            }
        }

        decls
    }

    fn match_token(&mut self, token: Token) {
        if self.next_token == token {
            self.advance();
        } else {
            eprintln!("Expect <{:?}>, got {:?}", token, &self.next_token);
        }
    }

    fn parse_type_decl(&mut self) -> Option<Decl> {
        match self.next_token {
            Token::Identifier(id) => {
                self.advance();

                self.parse_type().map(|typ| Decl::Type {
                    name: id.to_owned(),
                    typ,
                })
            }
            _ => None,
        }
    }

    fn parse_named_tuple_type(&mut self, mut map: HashMap<String, Type>) -> Type {
        loop {
            match self.next_token {
                Token::CloseParen => break,
                Token::Identifier(id) => {
                    self.advance();
                    match self.next_token {
                        Token::Colon => {
                            self.advance();
                            self.parse_type().map(|typ| map.insert(id.to_owned(), typ));
                        }
                        _ => {}
                    }
                }
                Token::Comma => self.advance(),
                _ => {}
            }
        }

        self.match_token(Token::CloseParen);
        Type::NamedTuple(map)
    }

    fn parse_anonymous_type(&mut self, mut vec: Vec<Type>) -> Type {
        loop {
            self.parse_type().map(|typ| vec.push(typ));
            match self.next_token {
                Token::Comma => self.advance(),
                Token::CloseParen => break,
                _ => {}
            }
        }
        self.match_token(Token::CloseParen);
        Type::AnonymousTuple(vec)
    }

    fn parse_type(&mut self) -> Option<Type> {
        match self.next_token {
            Token::OpenParen => {
                self.advance();
                let typ = match self.next_token {
                    Token::Identifier(id) => {
                        self.advance();
                        match self.next_token {
                            Token::Colon => {
                                self.advance();
                                self.parse_type().map(|typ| {
                                    let mut map = HashMap::<String, Type>::new();
                                    map.insert(id.to_owned(), typ);

                                    self.parse_named_tuple_type(map)
                                })
                            }
                            Token::Comma => {
                                self.advance();
                                let v = vec![Type::UserDefine(id.to_owned())];
                                Some(self.parse_anonymous_type(v))
                            }
                            _ => None,
                        }
                    }
                    Token::OpenParen => self.parse_type().map(|typ| {
                        let v = vec![typ];
                        self.parse_anonymous_type(v)
                    }),
                    Token::CloseParen => {
                        self.advance();
                        Some(Type::AnonymousTuple(vec![]))
                    }
                    ref token if token.is_builtin_type() => {
                        let mut v = vec![Type::from(token)];
                        self.advance();
                        if matches!(self.next_token, Token::Comma) {
                            self.advance();
                        }
                        Some(self.parse_anonymous_type(v))
                    }
                    _ => None,
                };

                if matches!(self.next_token, Token::Comma) {
                    self.advance();
                }

                typ
            }
            Token::Identifier(typename) => {
                self.advance();
                Some(Type::UserDefine((*typename).to_owned()))
            }
            ref token if token.is_builtin_type() => Some(Type::from(token)).map(|typ| {
                self.advance();
                typ
            }),
            _ => None,
        }
    }

    fn parse_func_decl(&mut self) -> Option<Decl> {
        todo!()
    }

    fn parse_var_decl(&mut self, name: String) -> Option<Decl> {
        self.parse_expression().map(|expr| Decl::Var { name, expr })
    }

    fn parse_leaf(&mut self) -> Option<Expr> {
        if self.next_token.is_atomic() {
            let expr = Expr::from(&self.next_token);
            self.advance();
            if matches!(&expr, Expr::Id(_)) && matches!(&self.next_token, Token::OpenParen) {
                return self.parse_leaf().map(|expr| match expr {
                    Expr::Empty => FuncCall(vec![]),
                    Expr::Tuple(exprs) => FuncCall(exprs),
                    Expr::String(_)
                    | Expr::Char(_)
                    | Expr::Id(_)
                    | Expr::Bool(_)
                    | Expr::Int(_)
                    | Expr::Float32(_)
                    | Expr::Float64(_) => FuncCall(vec![expr]),
                    _ => expr,
                });
            }
            return Some(expr);
        }

        match self.next_token {
            Token::Minus | Token::Tilda => {
                let op = Operator::from(&self.next_token);
                self.advance();
                self.parse_expression()
                    .map(|expr| Expr::Unary {
                        right: Box::new(expr),
                        op,
                    })
                    .or_else(|| {
                        eprintln!("expect <expr>");
                        None
                    })
            }
            Token::OpenParen => {
                self.advance();
                let expr = self.parse_expression();

                match self.next_token {
                    Token::CloseParen => {
                        self.advance();
                        if expr.is_none() {
                            Some(Expr::Empty)
                        } else {
                            expr
                        }
                    }
                    Token::Comma => {
                        self.advance();
                        if expr.is_none() {
                            eprintln!("Expect '(' or <expr>");
                            return None;
                        }

                        let mut exprs = vec![expr.unwrap()];

                        while let Some(expr) = self.parse_expression() {
                            exprs.push(expr);
                            match self.next_token {
                                Token::Comma => {
                                    self.advance();
                                }
                                Token::CloseParen => {
                                    self.advance();
                                    break;
                                }
                                _ => todo!(),
                            }
                        }

                        Some(Expr::Tuple(exprs))
                    }
                    _ => {
                        eprintln!("Unclosed parenthesis");
                        None
                    }
                }
            }
            _ => {
                eprintln!("Expect <expr>, -, ~, (");
                None
            }
        }
    }

    fn parse_increasing_precedence(&mut self, left: Expr, min_precedence: i8) -> (Expr, bool) {
        if !self.next_token.is_binary() {
            return (left, true);
        }

        let next_precedence = self.next_token.precedence();

        if next_precedence <= min_precedence {
            (left, true)
        } else {
            let op = Operator::from(&self.next_token);
            self.advance();
            let right = self.parse_expression_prime(next_precedence);
            if right.is_none() {
                return (left, true);
            }
            let right = right.unwrap();

            let node = Expr::Binary {
                left: Box::new(left),
                right: Box::new(right),
                op,
            };

            (node, false)
        }
    }

    fn parse_expression_prime(&mut self, min_precedence: i8) -> Option<Expr> {
        let left = self.parse_leaf();
        if left.is_none() {
            return None;
        }

        let mut left = left.unwrap();

        loop {
            let (node, should_return) = self.parse_increasing_precedence(left, min_precedence);
            if should_return {
                return Some(node);
            }
            left = node;
        }
    }

    #[inline(always)]
    fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_expression_prime(-1)
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_leaf() {
        let mut parser = Parser::new("1 - 2");
        assert_eq!(Expr::Int(1), parser.parse_leaf().unwrap());
        assert_eq!(
            Expr::Unary {
                right: Box::new(Expr::Int(2)),
                op: Operator::Subtract
            },
            parser.parse_leaf().unwrap()
        );
    }

    #[test]
    fn test_parse_expression() {
        use crate::ast::Expr::*;

        let mut parser = Parser::new("2 - -1");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(
            Binary {
                left: Box::new(Int(2)),
                right: Box::new(Unary {
                    right: Box::new(Int(1)),
                    op: Operator::Subtract,
                }),
                op: Operator::Subtract,
            },
            tree
        );

        let mut parser = Parser::new("a > b + c * d + e");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(
            Binary {
                left: Box::new(Id("a".to_owned())),
                right: Box::new(Binary {
                    left: Box::new(Binary {
                        left: Box::new(Id("b".to_owned())),
                        right: Box::new(Binary {
                            left: Box::new(Id("c".to_owned())),
                            right: Box::new(Id("d".to_owned())),
                            op: Operator::Multiply,
                        }),
                        op: Operator::Add,
                    }),
                    right: Box::new(Id("e".to_owned())),
                    op: Operator::Add,
                }),
                op: Operator::Gt
            },
            tree
        );

        let mut parser = Parser::new("a > (b + c) * (d + e)");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(
            Binary {
                left: Box::new(Id("a".to_owned())),
                right: Box::new(Binary {
                    left: Box::new(Binary {
                        left: Box::new(Id("b".to_owned())),
                        right: Box::new(Id("c".to_owned())),
                        op: Operator::Add,
                    }),
                    right: Box::new(Binary {
                        left: Box::new(Id("d".to_owned())),
                        right: Box::new(Id("e".to_owned())),
                        op: Operator::Add,
                    }),
                    op: Operator::Multiply,
                }),
                op: Operator::Gt,
            },
            tree
        );

        let mut parser = Parser::new("()");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(Empty, tree);

        let mut parser = Parser::new("(123)");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(Int(123), tree);

        let mut parser = Parser::new("(123, 456)");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(Tuple(vec![Int(123), Int(456)]), tree);

        let mut parser = Parser::new("(123, (a + b))");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(
            Tuple(vec![
                Int(123),
                Binary {
                    left: Box::new(Id("a".to_owned())),
                    right: Box::new(Id("b".to_owned())),
                    op: Operator::Add,
                }
            ]),
            tree
        );

        let mut parser = Parser::new("func()");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(FuncCall(vec![]), tree);

        let mut parser = Parser::new("func(123.123)");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(FuncCall(vec![Float32(123.123)]), tree);

        let mut parser = Parser::new("func(123.123, \"hello world\")");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(
            FuncCall(vec![Float32(123.123), String("hello world".to_owned())]),
            tree
        );
    }

    #[test]
    fn test_parse_var_decl() {
        use crate::ast::Decl::*;

        let mut parser = Parser::new(
            r#"
        a := 10
        b := a
        c := a + b / 30
        "#,
        );
        let decls = parser.parse_program();
        assert_eq!(
            vec![
                Var {
                    name: "a".to_owned(),
                    expr: Expr::Int(10),
                },
                Var {
                    name: "b".to_owned(),
                    expr: Expr::Id("a".to_owned()),
                },
                Var {
                    name: "c".to_owned(),
                    expr: Expr::Binary {
                        left: Box::new(Expr::Id("a".to_owned())),
                        right: Box::new(Expr::Binary {
                            left: Box::new(Expr::Id("b".to_owned())),
                            right: Box::new(Expr::Int(30)),
                            op: Operator::Divide,
                        }),
                        op: Operator::Add,
                    },
                },
            ],
            decls,
        );
    }

    #[test]
    fn test_parse_type_decl() {
        let mut parser = Parser::new(
            r#"
        type NewType Int
        type NewType Float32
        type NewType Float64
        type NewType String
        type NewType Char
        type NewType2 NewType
        type NewType ()
        type NewType (Int, Float32, Float64, String, Char, SomethingElse,)
        type NewType ((Int, Int), (Float32, Float32))
        type NewType (Field1: String, Field2: Int,)
        type NewType (Field1: String, Field2: (Field1: String, Field2: Char,),)
        type NewType (Field1: (Int, (Field2: Char)))
        "#,
        );

        let new_type_id = "NewType".to_owned();

        let decls = parser.parse_program();
        assert_eq!(
            vec![
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::Int32,
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::Float32,
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::Float64,
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::String,
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::Char,
                },
                Decl::Type {
                    name: "NewType2".to_owned(),
                    typ: Type::UserDefine(new_type_id.clone()),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::AnonymousTuple(vec![]),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::AnonymousTuple(vec![
                        Type::Int32,
                        Type::Float32,
                        Type::Float64,
                        Type::String,
                        Type::Char,
                        Type::UserDefine("SomethingElse".to_owned()),
                    ]),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::AnonymousTuple(vec![
                        Type::AnonymousTuple(vec![Type::Int32, Type::Int32]),
                        Type::AnonymousTuple(vec![Type::Float32, Type::Float32]),
                    ]),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::NamedTuple(HashMap::from([
                        ("Field1".to_owned(), Type::String),
                        ("Field2".to_owned(), Type::Int32),
                    ])),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::NamedTuple(HashMap::from([
                        ("Field1".to_owned(), Type::String),
                        (
                            "Field2".to_owned(),
                            Type::NamedTuple(HashMap::from([
                                ("Field1".to_owned(), Type::String),
                                ("Field2".to_owned(), Type::Char),
                            ]))
                        ),
                    ])),
                },
                Decl::Type {
                    name: new_type_id.clone(),
                    typ: Type::NamedTuple(HashMap::from([(
                        "Field1".to_owned(),
                        Type::AnonymousTuple(vec![
                            Type::Int32,
                            Type::NamedTuple(HashMap::from([("Field2".to_owned(), Type::Char)]))
                        ])
                    ),])),
                },
            ],
            decls,
        );
    }
}
