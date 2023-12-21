#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers and literals
    Ident(String),
    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),

    // Statements
    Assign,
    If,
    Else,

    // Operators
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Not,

    // Reserved keywords
    Function,
    Let,
    Return,

    // Punctuations
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}
