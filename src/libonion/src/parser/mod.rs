use nom::*;

pub mod ast;
use crate::lexer::token::*;
use crate::parser::ast::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{map, opt, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

macro_rules! tag_token (
    ($func_name:ident, $tag: expr) => (
        fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| t.tok[0] == $tag)(tokens)
        }
    )
);

tag_token!(let_tag, Token::Let);
tag_token!(assign_tag, Token::Assign);
tag_token!(semicolon_tag, Token::SemiColon);
tag_token!(return_tag, Token::Return);
tag_token!(lbrace_tag, Token::LBrace);
tag_token!(rbrace_tag, Token::RBrace);
tag_token!(lparen_tag, Token::LParen);
tag_token!(rparen_tag, Token::RParen);
tag_token!(lbracket_tag, Token::LBracket);
tag_token!(rbracket_tag, Token::RBracket);
tag_token!(comma_tag, Token::Comma);
tag_token!(colon_tag, Token::Colon);
tag_token!(plus_tag, Token::Plus);
tag_token!(minus_tag, Token::Minus);
tag_token!(not_tag, Token::Not);
tag_token!(if_tag, Token::If);
tag_token!(else_tag, Token::Else);
tag_token!(function_tag, Token::Function);
tag_token!(eof_tag, Token::EOF);

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match t1.tok[0].clone() {
            Token::IntLiteral(name) => Ok((i1, Literal::IntLiteral(name))),
            Token::StringLiteral(s) => Ok((i1, Literal::StringLiteral(s))),
            Token::BoolLiteral(b) => Ok((i1, Literal::BoolLiteral(b))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_ident(input: Tokens) -> IResult<Tokens, Ident> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match t1.tok[0].clone() {
            Token::Ident(name) => Ok((i1, Ident(name))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}
