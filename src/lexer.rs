use std::str::Chars;

const SINGLE_QUOTE: char = '\'';
const DOUBLE_QUOTE: char = '"';
const BACKSLASH: char = '\\';

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Identifier(&'a str),
    IntLit(&'a str),
    FloatLit(&'a str),
    StringLit(&'a str),
    CharLit(&'a str),
    Discard,
    True,
    False,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Percent,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Skip,
    Illegal,
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(value: &'a str) -> Self {
        use Token::*;
        match value {
            "_" => Discard,
            "if" => If,
            "else" => Else,
            "while" => While,
            "for" => For,
            "return" => Return,
            "true" => True,
            "false" => False,
            "continue" => Continue,
            "break" => Break,
            _ => Identifier(value),
        }
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    line: usize,
    col: usize,
    pos: usize,
    src: &'a str,
    chars: Chars<'a>,
    errors: Vec<LexerError<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Tokenizer {
            line: 1,
            col: 0,
            pos: 0,
            src,
            chars: src.chars(),
            errors: Vec::new(),
        }
    }

    #[inline(always)]
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        self.pos += c.len_utf8();
        self.col += 1;

        if c == '\n' {
            self.line += 1;
            self.col = 1;
        }

        Some(c)
    }

    fn bump_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        let c = self.peek()?;
        if !predicate(c) {
            return None;
        }

        self.bump()
    }

    fn bump_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.pos < self.src.len() && predicate(self.peek().unwrap()) {
            self.bump();
        }
    }

    fn match_number(&mut self, offset: usize, begin: char) -> Option<Token<'a>> {
        if !begin.is_ascii_digit() {
            return None;
        }

        self.bump_while(|c| c.is_ascii_digit());

        let scan_fraction = self
            .peek()
            .map_or(false, |c| c == '.');

        if scan_fraction {
            self.bump();
            self.bump_while(|c| c.is_ascii_digit());
        }

        let scan_exp = self
            .peek()
            .map_or(false, |c| c == 'e' || c == 'E');

        if scan_exp {
            self.bump();
            self.bump_if(|c| c == '+' || c == '-');
            self.bump_while(|c| c.is_ascii_digit());
        }

        let is_float = scan_fraction || scan_exp;

        let literal = &self.src[offset..self.pos];
        if is_float {
            Some(Token::FloatLit(literal))
        } else {
            Some(Token::IntLit(literal))
        }
    }

    fn match_identifier(&mut self, offset: usize, begin: char) -> Option<Token<'a>> {
        if !(begin.is_alphabetic() || begin == '_') {
            return None;
        }
        self.bump_while(|c| c.is_alphanumeric() || c == '_');

        let token: Token<'a> = (&self.src[offset..self.pos]).into();
        Some(token)
    }

    fn match_char(&mut self, begin: char) -> Option<Token<'a>> {
        if begin != SINGLE_QUOTE {
            return None;
        }

        let offset = self.pos;

        let next_char = match self.bump() {
            Some(c) => c,
            _ => {
                self.errors.push(LexerError::new(
                    self.line,
                    self.pos,
                    LexerErrorKind::UnclosedCharLiteral,
                ));
                return Some(Token::CharLit(""));
            }
        };

        if next_char == SINGLE_QUOTE {
            self.errors.push(LexerError::new(self.line, self.col, LexerErrorKind::EmptyCharLiteral));
            return Some(Token::CharLit(""));
        }

        let token = Token::CharLit(&self.src[offset..self.pos]);

        match self.peek() {
            Some(c) if c == SINGLE_QUOTE => {
                self.bump();
            }
            _ => {
                self.errors.push(LexerError::new(
                    self.line,
                    self.pos,
                    LexerErrorKind::UnclosedCharLiteral,
                ));
            }
        };

        Some(token)
    }

    fn match_string(&mut self, begin: char) -> Option<Token<'a>> {
        if begin != DOUBLE_QUOTE {
            return None;
        }

        let mut cur_char = begin;
        let offset = self.pos;

        while let Some(c) = self.peek() {
            if cur_char == BACKSLASH && ![DOUBLE_QUOTE, BACKSLASH, 'n', 'r', 't', '0'].contains(&c)
            {
                self.errors.push(LexerError::new(
                    self.line,
                    self.col,
                    LexerErrorKind::InvalidEscapedChar,
                ))
            }

            let is_escaped_double_quote = cur_char == BACKSLASH && c == DOUBLE_QUOTE;

            cur_char = c;

            if !(is_escaped_double_quote || (c != DOUBLE_QUOTE && c != '\n')) {
                break;
            }

            self.bump();
        }

        let next_char = cur_char;
        let literal = &self.src[offset..self.pos];

        if next_char == '\n' || self.peek().is_none() {
            self.errors.push(LexerError::new(
                self.line,
                self.col,
                LexerErrorKind::UnclosedStringLiteral,
            ))
        } else {
            debug_assert!(next_char == DOUBLE_QUOTE);
            self.bump();
        }

        Some(Token::StringLit(literal))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use LexerErrorKind::*;
        use Token::*;

        let offset = self.pos;

        let c = self.bump()?;

        if let Some(t) = self.match_identifier(offset, c) {
            return Some(t);
        }

        if let Some(t) = self.match_string(c) {
            return Some(t);
        }

        if let Some(t) = self.match_char(c) {
            return Some(t);
        }

        if let Some(t) = self.match_number(offset, c) {
            return Some(t);
        }

        let token = match c {
            '=' => Eq,
            '>' => match self.peek() {
                Some('=') => Gte,
                _ => Gt,
            },
            '<' => match self.peek() {
                Some('=') => Lte,
                _ => Lt,
            },
            '/' => match self.peek() {
                Some('/') => {
                    self.bump_while(|c| c != '\n');
                    Skip
                }
                Some('=') => Neq,
                _ => Slash,
            },
            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '^' => Caret,
            '%' => Percent,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            c if c.is_whitespace() => Skip,
            _ => Illegal,
        };

        match token {
            Illegal => {
                self.errors.push(LexerError::new(
                    self.line,
                    offset,
                    IllegalLiteral(&self.src[offset..self.pos]),
                ));
            }
            Gte | Lte | Neq => {
                self.bump();
                return Some(token);
            }
            Skip => {}
            _ => return Some(token),
        }

        self.next()
    }
}

#[derive(Debug, PartialEq)]
struct LexerError<'a> {
    line: usize,
    col: usize,
    kind: LexerErrorKind<'a>,
}

impl<'a> LexerError<'a> {
    fn new(line: usize, col: usize, kind: LexerErrorKind<'a>) -> Self {
        LexerError { line, col, kind }
    }
}

#[derive(Debug, PartialEq)]
enum LexerErrorKind<'a> {
    UnclosedCharLiteral,
    EmptyCharLiteral,
    InvalidEscapedChar,
    UnclosedStringLiteral,
    IllegalLiteral(&'a str),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifiers() {
        use crate::lexer::Token::{Discard, Identifier};

        let input = "foobar v_a_z1234 _";
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![Identifier("foobar"), Identifier("v_a_z1234"), Discard,],
            tokenizer.collect::<Vec<Token>>(),
        );
    }

    #[test]
    fn test_keywords() {
        use crate::lexer::Token::*;

        let input = "true\nfalse\nif else while for continue break return";
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![True, False, If, Else, While, For, Continue, Break, Return,],
            tokenizer.collect::<Vec<Token>>(),
        );
    }

    #[test]
    fn test_symbols() {
        use crate::lexer::Token::*;

        let input = "= /= > >= < <= +-*/^%,.(){}[]";
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![
                Eq,
                Neq,
                Gt,
                Gte,
                Lt,
                Lte,
                Plus,
                Minus,
                Star,
                Slash,
                Caret,
                Percent,
                Comma,
                Dot,
                OpenParen,
                CloseParen,
                OpenBrace,
                CloseBrace,
                OpenBracket,
                CloseBracket,
            ],
            tokenizer.collect::<Vec<Token>>(),
        );
    }

    #[test]
    fn test_string_literal() {
        use crate::lexer::Token::StringLit;

        let input = r#""this is a string"
        "string with escaped '\"\\\n\r\t\0'"
        "#;
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![
                StringLit("this is a string"),
                StringLit("string with escaped '\\\"\\\\\\n\\r\\t\\0'")
            ],
            tokenizer.collect::<Vec<Token>>(),
        );

        let input = r#"
"invalid escaped char: \v"
        "#;
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(
            StringLit("invalid escaped char: \\v"),
            tokenizer.next().unwrap(),
        );

        assert_eq!(
            LexerError::new(
                2,
                r#""invalid escaped char: \v"#.len(),
                LexerErrorKind::InvalidEscapedChar
            ),
            tokenizer.errors[0],
        );
    }

    #[test]
    fn test_char_literal() {
        use crate::lexer::Token::CharLit;

        let input = "'c''a''b'";
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![
                CharLit("c"),
                CharLit("a"),
                CharLit("b"),
            ],
            tokenizer.collect::<Vec<Token>>(),
        );

        let input = "'' '";
        let mut tokenizer = Tokenizer::new(input);
        while let Some(_) = tokenizer.next() {}

        assert_eq!(
            vec![
                LexerError::new(1, 2, LexerErrorKind::EmptyCharLiteral),
                LexerError::new(1, 4, LexerErrorKind::UnclosedCharLiteral),
            ],
            tokenizer.errors
        )
    }

    #[test]
    fn test_number_literal() {
        use super::Token::*;
        let input = "01234 123.0123 123e-1 245.123E+2";
        let tokenizer = Tokenizer::new(input);
        assert_eq!(
            vec![
                IntLit("01234"),
                FloatLit("123.0123"),
                FloatLit("123e-1"),
                FloatLit("245.123E+2"),
            ],
            tokenizer.collect::<Vec<Token>>(),
        )
    }
}
