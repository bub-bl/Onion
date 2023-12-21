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

fn infix_op(t: &Token) -> (Precedence, Option<Infix>) {
    match *t {
        Token::Equal => (Precedence::Equals, Some(Infix::Equal)),
        Token::NotEqual => (Precedence::Equals, Some(Infix::NotEqual)),
        Token::LessThanEqual => (Precedence::LessGreater, Some(Infix::LessThanEqual)),
        Token::GreaterThanEqual => (Precedence::LessGreater, Some(Infix::GreaterThanEqual)),
        Token::LessThan => (Precedence::LessGreater, Some(Infix::LessThan)),
        Token::GreaterThan => (Precedence::LessGreater, Some(Infix::GreaterThan)),
        Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
        Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
        Token::Multiply => (Precedence::Product, Some(Infix::Multiply)),
        Token::Divide => (Precedence::Product, Some(Infix::Divide)),
        Token::LParen => (Precedence::Call, None),
        Token::LBracket => (Precedence::Index, None),
        _ => (Precedence::Lowest, None),
    }
}

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match t1.tok[0].clone() {
            Token::NumberLiteral(name) => Ok((i1, Literal::NumberLiteral(name))),
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

fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    terminated(many0(parse_stmt), eof_tag)(input)
}

fn parse_expr(input: Tokens) -> IResult<Tokens, Expression> {
    parse_pratt_expr(input, Precedence::Lowest)
}

fn parse_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    alt((parse_let_stmt, parse_return_stmt, parse_expr_stmt))(input)
}

fn parse_let_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            let_tag,
            parse_ident,
            assign_tag,
            parse_expr,
            opt(semicolon_tag),
        )),
        |(_, ident, _, expr, _)| Statement::Let(ident, expr),
    )(input)
}

fn parse_return_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        delimited(return_tag, parse_expr, opt(semicolon_tag)),
        Statement::Return,
    )(input)
}

fn parse_expr_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(terminated(parse_expr, opt(semicolon_tag)), |expr| {
        Statement::Expression(expr)
    })(input)
}

fn parse_paren_expr(input: Tokens) -> IResult<Tokens, Expression> {
    delimited(lparen_tag, parse_expr, rparen_tag)(input)
}

fn parse_lit_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_literal, Expression::Literal)(input)
}
fn parse_ident_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_ident, Expression::Identifier)(input)
}

fn parse_atom_expr(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        parse_lit_expr,
        parse_ident_expr,
        // parse_prefix_expr,
        parse_paren_expr,
        // parse_array_expr,
        // parse_hash_expr,
        // parse_if_expr,
        // parse_fn_expr,
    ))(input)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, Expression> {
    let (i1, left) = parse_atom_expr(input)?;
    go_parse_pratt_expr(i1, precedence, left)
}

fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    left: Expression,
) -> IResult<Tokens, Expression> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Ok((i1, left))
    } else {
        let preview = &t1.tok[0];
        let p = infix_op(preview);
        match p {
            // (Precedence::Call, _) if precedence < Precedence::Call => {
            //     let (i2, left2) = parse_call_expr(input, left)?;
            //     go_parse_pratt_expr(i2, precedence, left2)
            // }
            // (Precedence::Index, _) if precedence < Precedence::Index => {
            //     let (i2, left2) = parse_index_expr(input, left)?;
            //     go_parse_pratt_expr(i2, precedence, left2)
            // }
            // (ref peek_precedence, _) if precedence < *peek_precedence => {
            //     let (i2, left2) = parse_infix_expr(input, left)?;
            //     go_parse_pratt_expr(i2, precedence, left2)
            // }
            _ => Ok((input, left)),
        }
    }
}

pub struct Parser;

impl Parser {
    pub fn parse_tokens(tokens: Tokens) -> IResult<Tokens, Program> {
        parse_program(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;

    fn assert_input_with_program(input: &[u8], expected_results: Program) {
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse_tokens(tokens).unwrap();
        assert_eq!(result, expected_results);
    }

    fn compare_inputs(input: &[u8], input2: &[u8]) {
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse_tokens(tokens).unwrap();

        let (_, r) = Lexer::lex_tokens(input2).unwrap();
        let tokens = Tokens::new(&r);
        let (_, expected_results) = Parser::parse_tokens(tokens).unwrap();

        assert_eq!(result, expected_results);
    }

    #[test]
    fn empty() {
        assert_input_with_program(&b""[..], vec![]);
    }

    #[test]
    fn let_statements() {
        let input = "let x = 5;\
             let y = 10;\
             let foobar = 838383;\
             let boo = true;\
            "
        .as_bytes();

        let program: Program = vec![
            Statement::Let(
                Ident("x".to_owned()),
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(5))),
            ),
            Statement::Let(
                Ident("y".to_owned()),
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(10))),
            ),
            Statement::Let(
                Ident("foobar".to_owned()),
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(838383))),
            ),
            Statement::Let(
                Ident("boo".to_owned()),
                Expression::Literal(Literal::BoolLiteral(true)),
            ),
        ];

        assert_input_with_program(input, program);
    }
}
