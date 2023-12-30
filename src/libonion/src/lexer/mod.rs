use self::token::{TokenKind, Token, Span};

pub mod token;
mod tests;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input, 
            position: 0, 
            read_position: 0, 
            ch: 0 as char,
        };

        lexer.read_char();
        return lexer;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();
        
        let kind = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::NotEqual
                } else {
                    TokenKind::Bang
                }
            }
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::LessThanEqual
                } else {
                    TokenKind::LessThan
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::GreaterThanEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            '\u{0}' => TokenKind::EOF,
            _ => {
                if is_letter(self.ch) {
                    let (start, end, ident) = self.read_identifier();

                    return Token {
                        kind: TokenKind::Identifier(ident),
                        span: Span {
                            start,
                            end,
                        },
                    };
                } else if is_digit(self.ch) {
                    let (start, end, num) = self.read_number();

                    return Token {
                        kind: TokenKind::Integer(num),
                        span: Span {
                            start,
                            end,
                        },
                    };
                } else {
                    TokenKind::Illegal
                }
            }
        };

        self.read_char();

        return Token {
            kind,
            span: Span {
                start: self.position - 1,
                end: self.read_position - 1
            },
        };
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char
        } else {
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                self.ch = ch;
            } else {
                panic!("Read out of bounds");
            }
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return 0 as char
        } else {
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                return ch
            } else {
                panic!("Read out of bounds");
            }
        }
    }

    fn read_identifier(&mut self) -> (usize, usize, String) {
        let pos = self.position;

        while is_letter(self.ch) {
            self.read_char();
        }

        let ident = self.input[pos..self.position].to_string();
        return (pos, self.position, ident);
    }

    fn read_number(&mut self) -> (usize, usize, i64) {
        let pos = self.position;

        while is_digit(self.ch) {
            self.read_char();
        }

        let value = self.input[pos..self.position].parse().unwrap();
        return (pos, self.position, value);
    }

    fn read_string(&mut self) -> (usize, usize, String) {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == '"' || self.ch == '\u{0}' {
                break;
            }
        }

        let value = self.input[pos..self.position].to_string();

        if self.ch == '"' {
            self.read_char();
        }

        return (pos - 1, self.position, value);
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        if self.ch == '/' && self.peek_char() == '/' {
            self.read_char();
            self.read_char();
            
            loop {
                self.read_char();

                if self.ch == '\n' || self.ch == '\u{0}' {
                    // consume the comments end
                    if self.ch == '\n' {
                        self.read_char();
                    }

                    break;
                }
            }
        }
    }
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}