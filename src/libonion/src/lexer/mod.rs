use nom::branch::*;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::combinator::{map, map_res, recognize};
use nom::multi::many0;
use nom::sequence::{delimited, pair};
use nom::*;

pub mod token;
use crate::lexer::token::*;

macro_rules! syntax {
    ($func_name: ident, $tag_string: literal, $output_token: expr) => {
        fn $func_name<'a>(s: &'a [u8]) -> IResult<&[u8], Token> {
            map(tag($tag_string), |_| $output_token)(s)
        }
    };
}

// Operators
syntax! {equal_operator, "==", Token::Equal}
syntax! {not_equal_operator, "!=", Token::NotEqual}
syntax! {assign_operator, "=", Token::Assign}
syntax! {plus_operator, "+", Token::Plus}
syntax! {minus_operator, "-", Token::Minus}
syntax! {multiply_operator, "*", Token::Multiply}
syntax! {divide_operator, "/", Token::Divide}
syntax! {not_operator, "!", Token::Not}
syntax! {greater_operator_equal, ">=", Token::GreaterThanEqual}
syntax! {lesser_operator_equal, "<=", Token::LessThanEqual}
syntax! {greater_operator, ">", Token::GreaterThan}
syntax! {lesser_operator, "<", Token::LessThan}

// Punctuations
syntax! {comma_punctuation, ",", Token::Comma}
syntax! {semicolon_punctuation, ";", Token::SemiColon}
syntax! {colon_punctuation, ":", Token::Colon}
syntax! {lparen_punctuation, "(", Token::LParen}
syntax! {rparen_punctuation, ")", Token::RParen}
syntax! {lbrace_punctuation, "{", Token::LBrace}
syntax! {rbrace_punctuation, "}", Token::RBrace}
syntax! {lbracket_punctuation, "[", Token::LBracket}
syntax! {rbracket_punctuation, "]", Token::RBracket}
