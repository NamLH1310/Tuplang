use crate::ast::{Expr, Operator};
use crate::lexer::Token::Identifier;
use crate::lexer::{Token, Tokenizer};
use std::boxed::Box;

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

    fn is_leaf(&self) -> bool {
        use crate::lexer::Token::*;
        match self {
            Identifier(_) | IntLit(_) | FloatLit(_) | StringLit(_) | CharLit(_) | True | False => {
                true
            }
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

    fn parse_leaf(&mut self) -> Option<Expr> {
        if self.next_token.is_leaf() {
            let expr = Expr::from(&self.next_token);
            self.advance();
            return Some(expr);
        }

        match self.next_token {
            Token::Minus | Token::Tilda => {
                let op = Operator::from(&self.next_token);
                self.advance();
                let expr = self.parse_expression();
                if expr.is_none() {
                    todo!("report error invalid next token: expect expression");
                }
                Some(Expr::Unary {
                    right: Box::new(expr.unwrap()),
                    op,
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
                    },
                    Token::Comma => {
                        self.advance();
                        if expr.is_none() {
                            todo!("report error: expect '(' or <expr>");
                        }

                        let mut exprs = vec![expr.unwrap()];

                        while let Some(expr) = self.parse_expression() {
                            match self.next_token {
                                Token::Comma => {
                                    exprs.push(expr);
                                    self.advance();
                                },
                                Token::CloseParen => {
                                    exprs.push(expr);
                                    self.advance();
                                    break;
                                },
                                _ => todo!()
                            }
                        }

                        Some(Expr::Tuple(exprs))
                    }
                    _ => todo!("report unclosed parenthesis error error")
                }
            }
            _ => None,
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
                return (left, true)
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
        assert_eq!(Binary {
            left: Box::new(Int(2)),
            right: Box::new(Unary {
                right: Box::new(Int(1)),
                op: Operator::Subtract,
            }),
            op: Operator::Subtract,
        }, tree);

        let mut parser = Parser::new("a > b + c * d + e");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(Binary {
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
        }, tree);

        let mut parser = Parser::new("a > (b + c) * (d + e)");
        let tree = parser.parse_expression().unwrap();
        assert_eq!(Binary {
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
        }, tree);

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
        assert_eq!(Tuple(vec![Int(123), Binary{
            left: Box::new(Id("a".to_owned())),
            right: Box::new(Id("b".to_owned())),
            op: Operator::Add,
        }]), tree);
    }
}
