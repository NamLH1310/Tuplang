use std::fs;
use std::str::Chars;
use std::str::FromStr;

struct Lexer<'a> {
    source:    &'a String,
    chars:     Chars<'a>,
    error_buf: &'a mut Vec<String>,
}

struct Token {
    kind: Kind,
}

#[derive(Debug, PartialEq)]
enum Kind {
    Eof,
    For,
    If,
    Discard,
    Return,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percentage,
    Ampersand,
    Pipe,
    Caret,
    IntLit(i64),
    FloatLit(f64),
    BoolLit(bool),
    StringLit(String),
    Ident(String),
    Unknown(String),
}

impl Kind {
    pub const Mul: Kind = Kind::Asterisk;
    pub const Mod: Kind = Kind::Percentage;
    pub const Div: Kind = Kind::Div;
}

enum LexerError {
    UnknownToken,
    UnclosedString,
    InvalidEscapedChar,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String, error_buf: &'a mut Vec<String>) -> Lexer<'a> {
        Lexer {
            source,
            error_buf,
            chars:  source.chars(),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.next_kind().map(|kind| Token{ kind })
    }

    fn next_kind(&mut self) -> Result<Kind, LexerError>{
        loop {
            // skip whitespace
            if let Some(_) = self.peek().filter(|&c| c.is_whitespace()) {
                self.next();
                continue;
            }

            // skip comment
            if self.match_str("```") {
                while let Some(_) = self.next() {
                    if self.match_str("```") {
                        break;
                    }
                }

                continue;
            }

            if let Some(kind) = self.match_ident_or_keyword_or_bool_literal() {
                break Ok(kind);
            } else if let Some(kind) = self.match_number_literal() {
                break Ok(kind);
            } else if let Some(kind) = self.match_symbol() {
                break Ok(kind);
            } else if let Some(res) = self.match_string_literal() {
                break Ok(Kind::Eof);
            } else if let None = self.peek() {
                break Ok(Kind::Eof);
            } else {
                break Err(LexerError::UnknownToken("".to_owned()));
            }
        }
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn match_symbol(&mut self) -> Option<Kind> {
        if let Some(c) = self.peek() {
            match c {
                '+' => Some(Kind::Plus),
                '-' => Some(Kind::Minus),
                '*' => Some(Kind::Asterisk),
                '/' => Some(Kind::Slash),
                '%' => Some(Kind::Percentage),
                '&' => Some(Kind::Ampersand),
                '|' => Some(Kind::Pipe),
                '^' => Some(Kind::Caret),
                _   => None,
            }
        } else {
            None
        }
    }

    fn match_number_literal(&mut self) -> Option<Kind> {
        enum Flag {
            Int,
            Float,
        }
        
        if let Some(c) = self.peek() {
            if !c.is_numeric() {
                return None;
            }

            let mut lexeme = self.match_base10_integer();

            let mut flag = Flag::Int;

            self.peek().filter(|&c| c == '.').map(|c| {
                flag = Flag::Float;
                self.next();
                lexeme.push(c);
                lexeme += self.match_decimal().as_str();
            });

            self.peek().filter(|&c| c == 'e' || c == 'E').map(|c| {
                flag = Flag::Float;
                self.next();
                lexeme.push(c);

                self.peek().filter(|&c| c == '+' || c == '-').map(|c| {
                    self.next();
                    lexeme.push(c);
                });

                lexeme += self.match_decimal().as_str();
            });

            return match flag {
                Flag::Int => match i64::from_str(lexeme.as_str()) {
                    Ok(val)  => Some(Kind::IntLit(val)),
                    Err(err) => {
                        eprintln!("[ERROR]: invalid integer: {err}");
                        None
                    },
                }
                Flag::Float => match f64::from_str(lexeme.as_str()) {
                    Ok(val)  => Some(Kind::FloatLit(val)),
                    Err(err) => {
                        eprintln!("[ERROR]: invalid floating point: {err}");
                        None
                    }
                }
            };
        }

        None
    }

    fn match_base10_integer(&mut self) -> String {
        if let Some(c) = self.peek() {
            if c == '0' {
                self.next();
                return "0".to_owned();
            }
        }

        self.match_decimal()
    }

    fn match_decimal(&mut self) -> String {
        let mut lexeme = "".to_string();

        while let Some(c) = self.peek() {
            match c {
                '0' ..= '9' => {
                    lexeme.push(c);
                }
                _ => { break; }
            };

            self.next();
        }

        lexeme
    }

    fn match_string_literal(&mut self) -> Result<Kind, LexerError> {
        if !self.match_str("\"") {
            return None;
        }

        let mut string_content = "".to_string();

        while let Some(c) = self.peek() {
            if c == '"' || c == '\n' {
                break;
            }

            if self.match_char('\\') {
                let valid_escaped_chars = [('\\', '\\'), ('r', '\r'), ('n', '\n'), ('t', '\t'), ('"', '"')];
                let mut is_valid_escaped_char = false;

                for s in valid_escaped_chars {
                    if self.match_char(s.0) {
                        string_content.push(s.1);
                        is_valid_escaped_char = true;
                        break;
                    }
                }

                if !is_valid_escaped_char {
                    return Err(LexerError::InvalidEscapedChar);
                }

                continue;
            }

            string_content.push(c);
            self.next();
        }

        if let None = self.peek() {
            return Err(LexerError::UnclosedString);
        }

        if let Some(_) = self.peek().filter(|&c| c == '\n') {
            return Err(LexerError::UnclosedString);
        }

        Ok(Kind::StringLit(string_content))
    }

    fn match_ident_or_keyword_or_bool_literal(&mut self) -> Option<Kind> {
        if let Some(c) = self.peek() {
            if !c.is_alphabetic() && c != '_' {
                return None;
            }

            let mut ident = "".to_string();

            while let Some(c) = self.peek() {
                if !c.is_alphanumeric() && c != '_' {
                    break;
                }

                ident.push(c);

                self.next();
            }

            return Some(match ident.as_str() {
                "for"    => Kind::For,
                "if"     => Kind::If,
                "return" => Kind::Return,
                "true"   => Kind::BoolLit(true),
                "false"  => Kind::BoolLit(false),
                "_"      => Kind::Discard,
                _        => Kind::Ident(ident),
            });
        }

        None
    }

    fn match_str(&mut self, s: &str) -> bool {
        let ok = self.chars.clone().zip(s.chars()).all(|(c1, c2)| c1 == c2);
        if ok {
            for _ in 0 .. s.chars().count() {
                self.next();
            }
        }
        ok
    }

    fn match_char(&mut self, expected_char: char) -> bool {
        match self.peek().filter(|&c| c == expected_char) {
            Some(_) => { self.next(); true },
            _       => false,
        }
    }

    fn report_error(&mut self, msg: &str) {
        self.error_buf.push(msg.to_owned());
    }
    
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "true ```this is a comment```false  identifier _ _123".to_owned();
        let mut l = Lexer::new(&source);

        let expected = [
            Kind::BoolLit(true),
            Kind::BoolLit(false),
            Kind::Ident("identifier".to_string()),
            Kind::Discard,
            Kind::Ident("_123".to_string()),
            Kind::Eof,
        ]

        
        assert_eq!(l.next_token().kind, Kind::BoolLit(true));
        assert_eq!(l.next_token().kind, Kind::BoolLit(false));
        assert_eq!(l.next_token().kind, Kind::Ident("identifier".to_string()));
        assert_eq!(l.next_token().kind, Kind::Discard);
        assert_eq!(l.next_token().kind, Kind::Ident("_123".to_string()));
        assert_eq!(l.next_token().kind, Kind::Eof);
    }

    #[test]
    fn test_lexer_num_literal() {
        let source = "1234 12.34 012 1e2 1e002 123.456e-0001".to_owned();
        let mut l = Lexer::new(&source);
        assert_eq!(l.next_token().kind, Kind::IntLit(1234));
        assert_eq!(l.next_token().kind, Kind::FloatLit(f64::from_str("12.34").unwrap()));
        assert_eq!(l.next_token().kind, Kind::IntLit(0));
        assert_eq!(l.next_token().kind, Kind::IntLit(12));
        assert_eq!(l.next_token().kind, Kind::FloatLit(f64::from_str("1e2").unwrap()));
        assert_eq!(l.next_token().kind, Kind::FloatLit(f64::from_str("1e002").unwrap()));
        assert_eq!(l.next_token().kind, Kind::FloatLit(f64::from_str("123.456e-0001").unwrap()));
        assert_eq!(l.next_token().kind, Kind::Eof);
    }

    #[test]
    fn test_lexer_string_literal() {
        let source = "".to_owned();
    }
}
*/

fn main() {
    let source_path = "test.tup";

    let source;

    match fs::read_to_string(&source_path) {
        Err(err) => {
            eprintln!("[ERROR]: Could not read {source_path}: {err}");
            return;
        }
        Ok(s) => {
            source = s;
        }
    };

    let mut error_buf = Vec::<String>::new();

    let l = Lexer::new(&source, &mut error_buf);
}
