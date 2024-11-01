use std::str::Chars;

const SINGLE_QUOTE: char = '\'';
const DOUBLE_QUOTE: char = '"';
const BACKSLASH: char = '\\';

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
            col: 1,
            pos: 0,
            src,
            chars: src.chars(),
            errors: Vec::new(),
        }
    }

    fn peek(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        while let c = chars.next()? {
            if !c.is_whitespace() {
                return Some(c);
            }
        }

        unreachable!()
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn bump(&mut self) -> Option<char> {
        while let c = self.chars.next()? {
            self.pos += c.len_utf8();
            self.col += 1;
            if !c.is_whitespace() {
                return Some(c);
            }
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            }
        }

        unreachable!()
    }

    fn bump_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        let c = self.peek()?;
        if !predicate(c) {
            return None;
        }

        self.bump()
    }

    fn bump_while(&mut self, predicate: impl Fn(char) -> bool) {
        while !self.is_eof() && predicate(self.peek().unwrap()) {
            self.bump();
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let old_pos = self.pos;
        while !self.is_eof() && predicate(self.peek().unwrap()) {
            self.bump();
        }

        &self.src[old_pos..self.pos]
    }

    fn match_number(&mut self, begin: char) -> Option<Token<'a>> {
        if !begin.is_ascii_digit() {
            return None;
        }

        let offset = self.pos;

        while let Some(c) = self.peek() {
            if !c.is_ascii_digit() {
                break;
            }

            self.bump();
        }

        let mut scan_fraction = false;
        let mut scan_exp = false;

        self.peek().map(|c| {
            if c == '.' {
                scan_fraction = true;
                return true;
            }

            if c == 'e' || c == 'E' {
                scan_exp = true;
                return true;
            }

            false
        });

        let is_float = scan_fraction || scan_exp;
        if is_float {
            self.bump();
        }

        if scan_fraction {
            let pos = self.pos;
            while let Some(c) = self.peek() {
                if !c.is_ascii_digit() {
                    break;
                }

                self.bump();
            }

            let bumped = pos == self.pos;
            if !bumped {
                return Some(Token::FloatLit(&self.src[offset..self.pos]));
            }

            scan_exp = self.peek().map_or(false, |c| c == 'e' || c == 'E');
            if scan_exp {
                self.bump();
                self.bump_if(|c| c == '+' || c == '-');
            }
        }

        if scan_exp {
            while let Some(c) = self.peek() {
                if !c.is_ascii_digit() {
                    break;
                }

                self.bump();
            }
        }

        let literal = &self.src[offset..self.pos];
        if is_float {
            Some(Token::FloatLit(literal))
        } else {
            Some(Token::IntLit(literal))
        }
    }

    fn match_identifier(&mut self, begin: char) -> Option<Token<'a>> {
        if !(begin.is_alphabetic() || begin == '_') {
            return None;
        }

        Some(self.take_while(|c| c.is_alphanumeric() || c == '_').into())
    }

    fn match_char(&mut self, begin: char) -> Option<Token<'a>> {
        if begin != SINGLE_QUOTE {
            return None;
        }

        let next_char = self.bump()?;
        if next_char == SINGLE_QUOTE {
            return Some(Token::CharLit(""));
        }

        let token = Token::CharLit(&self.src[self.pos..self.pos + 1]);

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
            if cur_char == BACKSLASH && ![SINGLE_QUOTE, DOUBLE_QUOTE, BACKSLASH, 'n', 'r', 't', 'b', 'f', 'v', '0'].contains(&c) {
                self.errors.push(LexerError::new(self.line, self.col + 1, LexerErrorKind::InvalidEscapedChar))
            }

            let is_escaped_double_quote = cur_char == BACKSLASH && c == DOUBLE_QUOTE;

            cur_char = c;

            self.bump();

            if !(is_escaped_double_quote || (c != DOUBLE_QUOTE && c != '\n')) {
                break;
            }
        }

        let literal = &self.src[offset..self.pos];

        if cur_char == '\n' || self.peek().is_none() {
            self.errors.push(LexerError::new(self.line, self.col, LexerErrorKind::UnclosedStringLiteral))
        }

        let token = Token::from(literal);

        Some(token)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;
        use LexerErrorKind::*;

        let offset = self.pos;

        let c = self.bump()?;

        if let Some(t) = self.match_identifier(c) {
            return Some(t);
        }

        if let Some(t) = self.match_string(c) {
            return Some(t);
        }

        if let Some(t) = self.match_char(c) {
            return Some(t);
        }

        if let Some(t) = self.match_number(c) {
            return Some(t);
        }

        let token = match c {
            '=' => Eq,
            '>' => match self.peek() {
                Some('=') => Gte,
                _ => Gt,
            }
            '<' => match self.peek() {
                Some('=') => Lte,
                _ => Lt,
            }
            '/' => match self.peek() {
                Some('/') => {
                    self.bump_while(|c| c != '\n');
                    Skip
                }
                Some('=') => Neq,
                _ => Slash,
            }
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
                self.errors.push(LexerError::new(self.line, offset, IllegalLiteral(&self.src[offset..self.pos])));
            }
            Gte | Lte | Neq => {
                self.bump();
                return Some(token);
            }
            _ => return Some(token),
        }

        self.next()
    }
}

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

enum LexerErrorKind<'a> {
    UnclosedCharLiteral,
    EmptyCharLiteral,
    InvalidEscapedChar,
    UnclosedStringLiteral,
    IllegalLiteral(&'a str),
}