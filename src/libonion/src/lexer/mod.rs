use nom::branch::*;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::combinator::{map, map_res, recognize};
use nom::multi::many0;
use nom::sequence::{delimited, pair};
use nom::*;

use std::str::{FromStr, Utf8Error};

use crate::lexer::byte_slice::*;
use crate::lexer::parsing::*;

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
syntax! {modulo_operator, "%", Token::Modulo}
syntax! {not_operator, "!", Token::Not}
syntax! {greater_operator_equal, ">=", Token::GreaterThanEqual}
syntax! {lesser_operator_equal, "<=", Token::LessThanEqual}
syntax! {greater_operator, ">", Token::GreaterThan}
syntax! {lesser_operator, "<", Token::LessThan}

// Punctuations
syntax! {dot_punctuation, ".", Token::Dot}
syntax! {comma_punctuation, ",", Token::Comma}
syntax! {semicolon_punctuation, ";", Token::SemiColon}
syntax! {colon_punctuation, ":", Token::Colon}
syntax! {lparen_punctuation, "(", Token::LParen}
syntax! {rparen_punctuation, ")", Token::RParen}
syntax! {lbrace_punctuation, "{", Token::LBrace}
syntax! {rbrace_punctuation, "}", Token::RBrace}
syntax! {lbracket_punctuation, "[", Token::LBracket}
syntax! {rbracket_punctuation, "]", Token::RBracket}

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
    use std::borrow::Cow;

    use crate::math::numbers::Number;
    use crate::parser::ast::Statement;
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take};
    use nom::character::complete::{alpha1, alphanumeric1, char, digit0, digit1, multispace0};
    use nom::character::{is_hex_digit, is_oct_digit};
    use nom::combinator::{complete, map, map_res, opt, recognize};
    use nom::multi::many0;
    use nom::number::complete::{float, recognize_float};
    use nom::sequence::{delimited, pair, preceded, tuple};
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
            // lex_number,
            // lex_integer_literal,
            lex_number,
            // lex_float_literal,
            lex_illegal,
        ))(input)
    }

    fn lex_illegal(input: &[u8]) -> IResult<&[u8], Token> {
        map(take(1usize), |_| Token::Illegal)(input)
    }

    // fn lex_number(input: &[u8]) -> IResult<&[u8], Token> {
    //     alt((lex_float_literal, lex_float_literal))(input)
    // }

    fn lex_integer_literal(input: &[u8]) -> IResult<&[u8], Token> {
        map(
            map_res(
                map_res(digit1, ConvertToUtf8Str::to_utf8_str),
                complete_str_from_str,
            ),
            Token::NumberLiteral,
        )(input)
    }

    fn lex_float_literal(input: &[u8]) -> IResult<&[u8], Token> {
        map(float, |f| {
            println!("CONVERT: {:?}", f);
            Token::NumberLiteral(Number::Float(f as f64))
        })(input)
    }

    fn lex_number(input: &[u8]) -> IResult<&[u8], Token> {
        alt((parse_float, parse_unsigned_integer, parse_integer))(input)
    }

    fn parse_integer(input: &[u8]) -> IResult<&[u8], Token> {
        map_res(digit1, |s: &[u8]| {
            String::from_utf8(s.to_vec())
                .ok()
                .and_then(|s| s.parse::<i64>().ok())
                .map(|n| Token::NumberLiteral(Number::SignedInteger(n)))
                .ok_or(nom::Err::Error((s, nom::error::ErrorKind::Digit)))
        })(input)
    }

    fn parse_unsigned_integer(input: &[u8]) -> IResult<&[u8], Token> {
        map_res(digit1, |s: &[u8]| {
            String::from_utf8(s.to_vec())
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .map(|n| Token::NumberLiteral(Number::UnsignedInteger(n)))
                .ok_or(nom::Err::Error((s, nom::error::ErrorKind::Digit)))
        })(input)
    }

    fn parse_float(input: &[u8]) -> IResult<&[u8], Token> {
        let parse_decimal = preceded(char('.'), digit1);
        let parser = tuple((digit1, opt(parse_decimal)));

        map_res(
            parser,
            |(int_part, decimal_part): (&[u8], Option<&[u8]>)| {
                let int = String::from_utf8_lossy(int_part);
                let decimal = decimal_part
                    .map(|d| String::from_utf8_lossy(d))
                    .unwrap_or(Cow::from("0"));

                let str = format!("{}.{}", int, decimal);

                // println!(
                //     "FLOAT: {:?}, {:?} {:?}",
                //     intp,
                //     decimalp,
                //     decimal_part.is_some()
                // );

                if decimal_part.is_some() {
                    str.parse::<f64>()
                        .map(|s| Token::NumberLiteral(Number::Float(s)))
                        .map_err(|_| nom::Err::Error((input, nom::error::ErrorKind::Digit)))
                } else {
                    str.parse::<u64>()
                        .map(|s| Token::NumberLiteral(Number::UnsignedInteger(s)))
                        .map_err(|_| nom::Err::Error((input, nom::error::ErrorKind::Digit)))
                }
            },
        )(input)
    }

    fn lex_reserved_ident(input: &[u8]) -> IResult<&[u8], Token> {
        map_res(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s| {
                let c = ConvertToUtf8Str::to_utf8_str(&s);
                c.map(|syntax| match syntax {
                    "let" => Token::Let,
                    "fn" => Token::Function,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    "true" => Token::BoolLiteral(true),
                    "false" => Token::BoolLiteral(false),
                    "component" => Token::Component,
                    "use" => Token::Use,
                    "loop" => Token::Loop,
                    "break" => Token::Break,
                    "next" => Token::Next,
                    "event" => Token::Event,
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
            dot_punctuation,
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
            modulo_operator,
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

    use super::ConvertToUtf8Str;

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

    // pub(crate) fn complete_byte_slice_str_from_utf8(c: &[u8]) -> Result<&str, Utf8Error> {
    //     std::str::from_utf8(c)
    // }

    pub(crate) fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
        delimited(
            tag("\""),
            map_res(parse_quoted_string, convert_vec_utf8),
            tag("\""),
        )(input)
    }
}

trait ConvertToUtf8Str {
    fn to_utf8_str(&self) -> Result<&str, Utf8Error>;
}

impl ConvertToUtf8Str for &[u8] {
    fn to_utf8_str(&self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(self)
    }
}

impl ConvertToUtf8Str for [u8] {
    fn to_utf8_str(&self) -> Result<&str, Utf8Error> {
        (&self[..]).to_utf8_str()
    }
}