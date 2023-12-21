use nom::branch::*;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::combinator::{map, map_res, recognize};
use nom::multi::many0;
use nom::sequence::{delimited, pair};
use nom::*;

use std::str;
use std::str::{FromStr, Utf8Error};

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

use crate::lexer::byte_slice::*;
use crate::lexer::parsing::*;

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(bytes: &[u8]) -> IResult<&[u8], Vec<Token>> {
        lex_tokens(bytes)
            .map(|(slice, result)| (slice, [&result[..], &vec![Token::EOF][..]].concat()))
    }
}

mod parsing {
    use crate::lexer::token::Token;
    use crate::lexer::*;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take};
    use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
    use nom::combinator::{map, map_res, recognize};
    use nom::multi::many0;
    use nom::sequence::{delimited, pair};
    use nom::IResult;

    pub(crate) fn lex_tokens(input: &[u8]) -> IResult<&[u8], Vec<Token>> {
        many0(delimited(multispace0, lex_token, multispace0))(input)
    }

    fn lex_token(input: &[u8]) -> IResult<&[u8], Token> {
        alt((
            lex_operator,
            lex_punctuations,
            lex_string,
            lex_reserved_ident,
            lex_integer,
            lex_illegal,
        ))(input)
    }

    fn lex_illegal(input: &[u8]) -> IResult<&[u8], Token> {
        map(take(1usize), |_| Token::Illegal)(input)
    }

    fn lex_integer(input: &[u8]) -> IResult<&[u8], Token> {
        map(
            map_res(
                map_res(digit1, complete_byte_slice_str_from_utf8),
                complete_str_from_str,
            ),
            Token::NumberLiteral,
        )(input)
    }

    fn lex_reserved_ident(input: &[u8]) -> IResult<&[u8], Token> {
        map_res(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s| {
                let c = complete_byte_slice_str_from_utf8(s);
                c.map(|syntax| match syntax {
                    "let" => Token::Let,
                    "fn" => Token::Function,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    "true" => Token::BoolLiteral(true),
                    "false" => Token::BoolLiteral(false),
                    _ => Token::Ident(syntax.to_string()),
                })
            },
        )(input)
    }

    fn lex_string(input: &[u8]) -> IResult<&[u8], Token> {
        map(parse_string, Token::StringLiteral)(input)
    }

    pub fn lex_punctuations(input: &[u8]) -> IResult<&[u8], Token> {
        alt((
            comma_punctuation,
            semicolon_punctuation,
            colon_punctuation,
            lparen_punctuation,
            rparen_punctuation,
            lbrace_punctuation,
            rbrace_punctuation,
            lbracket_punctuation,
            rbracket_punctuation,
        ))(input)
    }

    pub fn lex_operator(input: &[u8]) -> IResult<&[u8], Token> {
        alt((
            equal_operator,
            not_equal_operator,
            assign_operator,
            plus_operator,
            minus_operator,
            multiply_operator,
            divide_operator,
            not_operator,
            greater_operator_equal,
            lesser_operator_equal,
            greater_operator,
            lesser_operator,
        ))(input)
    }
}

mod byte_slice {
    use nom::bytes::complete::{tag, take};
    use nom::combinator::map_res;
    use nom::sequence::delimited;
    use nom::{AsBytes, IResult};
    use std::str::{FromStr, Utf8Error};

    fn parse_quoted_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        use std::result::Result::*;

        let (i1, c1) = take(1usize)(input)?;
        match c1.as_bytes() {
            b"\"" => Ok((input, vec![])),
            b"\\" => {
                let (i2, c2) = take(1usize)(i1)?;
                parse_quoted_string(i2).map(|(slice, done)| (slice, concat_slice_vec(c2, done)))
            }
            c => parse_quoted_string(i1).map(|(slice, done)| (slice, concat_slice_vec(c, done))),
        }
    }

    pub(crate) fn concat_slice_vec(c: &[u8], done: Vec<u8>) -> Vec<u8> {
        let mut new_vec = c.to_vec();
        new_vec.extend(&done);
        new_vec
    }

    pub(crate) fn convert_vec_utf8(v: Vec<u8>) -> Result<String, Utf8Error> {
        let slice = v.as_slice();
        std::str::from_utf8(slice).map(|s| s.to_owned())
    }

    pub(crate) fn complete_str_from_str<F: FromStr>(c: &str) -> Result<F, F::Err> {
        FromStr::from_str(c)
    }

    pub(crate) fn complete_byte_slice_str_from_utf8(c: &[u8]) -> Result<&str, Utf8Error> {
        std::str::from_utf8(c)
    }

    pub(crate) fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
        delimited(
            tag("\""),
            map_res(parse_quoted_string, convert_vec_utf8),
            tag("\""),
        )(input)
    }
}
