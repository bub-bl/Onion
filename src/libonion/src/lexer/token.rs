use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Debug, Eq, Hash, Ord, Serialize, Deserialize, PartialOrd, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, Ord, Serialize, Deserialize, PartialOrd, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "start: {}, end: {}, kind: {}",
            self.span.start, self.span.end, self.kind
        )
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, Serialize, Deserialize, PartialOrd, PartialEq)]
#[serde(tag = "type", content = "value")]
pub enum TokenKind {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    Identifier { name: String },
    Integer(i64),
    String(String),

    // Operators
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    LessThan,    // <
    GreaterThan, // >

    Equal,    // ==
    NotEqual, // !=

    // delimiters
    Comma,
    SemiColon,
    Colon,

    LParen,
    RParen,
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn lookup_identifier(identifier: &str) -> TokenKind {
    match identifier {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        _ => TokenKind::Identifier {
            name: identifier.to_string(),
        },
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier { name } => write!(f, "{}", name),
            TokenKind::Integer(i) => write!(f, "{}", i),
            TokenKind::String(s) => write!(f, "{}", s),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Comma => write!(f, ","),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::ILLEGAL => write!(f, "ILLEGAL"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Colon => write!(f, ":"),
        }
    }
}
