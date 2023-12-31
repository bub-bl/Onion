use crate::lexer::token::TokenKind;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or =
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}

pub fn get_token_precedence(token: &TokenKind) -> Precedence {
    match token {
        TokenKind::Equal => Precedence::Equals,
        TokenKind::NotEqual => Precedence::Equals,
        TokenKind::LessThan => Precedence::LessGreater,
        TokenKind::GreaterThan => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Asterisk => Precedence::Product,
        TokenKind::Slash => Precedence::Product,
        TokenKind::LParen => Precedence::Call,
        TokenKind::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
