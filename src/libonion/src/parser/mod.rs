use nom::*;
use std::convert::Into;

pub mod ast;
use crate::lexer::token::*;
use crate::math::numbers::Number;
use crate::parser::ast::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{map, map_res, opt, verify};
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
tag_token!(dot_tag, Token::Dot);
tag_token!(colon_tag, Token::Colon);
tag_token!(plus_tag, Token::Plus);
tag_token!(minus_tag, Token::Minus);
tag_token!(not_tag, Token::Not);
tag_token!(if_tag, Token::If);
tag_token!(else_tag, Token::Else);
tag_token!(function_tag, Token::Function);
tag_token!(eof_tag, Token::EOF);
tag_token!(bind_tag, Token::Bind);
tag_token!(event_tag, Token::Event);
tag_token!(use_tag, Token::Use);
tag_token!(init_tag, Token::Init);
tag_token!(get_tag, Token::Get);
tag_token!(set_tag, Token::Set);
tag_token!(component_tag, Token::Component);
tag_token!(loop_tag, Token::Loop);
tag_token!(break_tag, Token::Break);
tag_token!(next_tag, Token::Next);

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
        Token::Modulo => (Precedence::Product, Some(Infix::Modulo)),
        Token::LParen => (Precedence::Call, None),
        Token::LBracket => (Precedence::Index, None),
        _ => (Precedence::Lowest, None),
    }
}

// fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
//     let (i1, t1) = take(1usize)(input)?;
//
//     if t1.tok.is_empty() {
//         Err(Err::Error(Error::new(input, ErrorKind::Tag)))
//     } else {
//         match t1.tok[0].clone() {
//             Token::NumberLiteral(name) => Ok((i1, Literal::NumberLiteral(name))),
//             Token::StringLiteral(s) => Ok((i1, Literal::StringLiteral(s))),
//             Token::BoolLiteral(b) => Ok((i1, Literal::BoolLiteral(b))),
//             _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
//         }
//     }
// }

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match t1.tok[0].clone() {
            Token::NumberLiteral(Number::UnsignedInteger(name)) => {
                Ok((i1, Literal::NumberLiteral(Number::UnsignedInteger(name))))
            }
            Token::NumberLiteral(Number::Float(f)) => {
                Ok((i1, Literal::NumberLiteral(Number::Float(f.into()))))
            }
            Token::StringLiteral(s) => Ok((i1, Literal::StringLiteral(s))),
            Token::BoolLiteral(b) => Ok((i1, Literal::BoolLiteral(b))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_component_keyword(input: Tokens) -> IResult<Tokens, Ident> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        Ok((i1, Ident("component".to_owned())))
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
    alt((
        parse_let_stmt,
        parse_prop_stmt,
        parse_component_stmt,
        parse_named_block_stmt,
        parse_return_stmt,
        parse_expr_stmt,
    ))(input)
}

fn parse_let_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    // TODO - We don't want to deal with semicolon_tag at the end of the statement
    map(
        tuple((
            let_tag,
            parse_ident,
            opt(colon_tag),
            opt(parse_array_expr),
            assign_tag,
            parse_expr,
            opt(semicolon_tag),
        )),
        |(_, ident, _, modifiers, _, expr, _)| Statement::Let(ident, modifiers, expr),
    )(input)
}

fn parse_prop_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    // TODO - We don't want to deal with semicolon_tag at the end of the statement
    map(
        tuple((parse_ident, colon_tag, parse_expr, semicolon_tag)),
        |(ident, _, expr, _)| Statement::Prop(ident, expr),
    )(input)
}

fn parse_component_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((parse_component_keyword, parse_ident, parse_block_stmt)),
        |(_, ident, program)| Statement::Component {
            ident: Keyword::Some(ident),
            body: program,
        },
    )(input)
}

fn parse_named_block_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((parse_ident, parse_block_stmt)),
        |(ident, program)| Statement::NamedBlock {
            ident,
            body: program,
        },
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

fn parse_comma_exprs(input: Tokens) -> IResult<Tokens, Expression> {
    preceded(comma_tag, parse_expr)(input)
}

fn parse_exprs(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    map(
        pair(parse_expr, many0(parse_comma_exprs)),
        |(first, second)| [&vec![first][..], &second[..]].concat(),
    )(input)
}

fn empty_boxed_vec(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    Ok((input, vec![]))
}

fn parse_array_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(
            lbracket_tag,
            alt((parse_exprs, empty_boxed_vec)),
            rbracket_tag,
        ),
        Expression::Array,
    )(input)
}

fn parse_hash_pair(input: Tokens) -> IResult<Tokens, (Literal, Expression)> {
    separated_pair(parse_literal, colon_tag, parse_expr)(input)
}

fn parse_hash_comma_expr(input: Tokens) -> IResult<Tokens, (Literal, Expression)> {
    preceded(comma_tag, parse_hash_pair)(input)
}

fn parse_hash_pairs(input: Tokens) -> IResult<Tokens, Vec<(Literal, Expression)>> {
    map(
        pair(parse_hash_pair, many0(parse_hash_comma_expr)),
        |(first, second)| [&vec![first][..], &second[..]].concat(),
    )(input)
}

fn empty_pairs(input: Tokens) -> IResult<Tokens, Vec<(Literal, Expression)>> {
    Ok((input, vec![]))
}

fn parse_hash_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(lbrace_tag, alt((parse_hash_pairs, empty_pairs)), rbrace_tag),
        Expression::Hash,
    )(input)
}

fn parse_prefix_expr(input: Tokens) -> IResult<Tokens, Expression> {
    let (i1, t1) = alt((plus_tag, minus_tag, not_tag))(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let (i2, e) = parse_atom_expr(i1)?;
        match t1.tok[0].clone() {
            Token::Plus => Ok((i2, Expression::Prefix(Prefix::PrefixPlus, Box::new(e)))),
            Token::Minus => Ok((i2, Expression::Prefix(Prefix::PrefixMinus, Box::new(e)))),
            Token::Not => Ok((i2, Expression::Prefix(Prefix::Not, Box::new(e)))),
            _ => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
        }
    }
}

fn parse_block_stmt(input: Tokens) -> IResult<Tokens, Program> {
    delimited(lbrace_tag, many0(parse_stmt), rbrace_tag)(input)
}

fn parse_atom_expr(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        parse_lit_expr,
        parse_ident_expr,
        parse_prefix_expr,
        parse_paren_expr,
        parse_array_expr,
        parse_hash_expr,
        parse_if_expr,
        parse_fn_expr,
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
            (Precedence::Call, _) if precedence < Precedence::Call => {
                let (i2, left2) = parse_call_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            (Precedence::Index, _) if precedence < Precedence::Index => {
                let (i2, left2) = parse_index_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                let (i2, left2) = parse_infix_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            _ => Ok((input, left)),
        }
    }
}

fn parse_infix_expr(input: Tokens, left: Expression) -> IResult<Tokens, Expression> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let next = &t1.tok[0];
        let (precedence, maybe_op) = infix_op(next);
        match maybe_op {
            None => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
            Some(op) => {
                let (i2, right) = parse_pratt_expr(i1, precedence)?;
                Ok((i2, Expression::Infix(op, Box::new(left), Box::new(right))))
            }
        }
    }
}

fn parse_call_expr(input: Tokens, fn_handle: Expression) -> IResult<Tokens, Expression> {
    map(
        delimited(lparen_tag, alt((parse_exprs, empty_boxed_vec)), rparen_tag),
        |e| Expression::Call {
            function: Box::new(fn_handle.clone()),
            arguments: e,
        },
    )(input)
}

fn parse_index_expr(input: Tokens, arr: Expression) -> IResult<Tokens, Expression> {
    map(delimited(lbracket_tag, parse_expr, rbracket_tag), |idx| {
        Expression::Index {
            array: Box::new(arr.clone()),
            index: Box::new(idx),
        }
    })(input)
}

fn parse_if_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        tuple((
            if_tag,
            lparen_tag,
            parse_expr,
            rparen_tag,
            parse_block_stmt,
            parse_else_expr,
        )),
        |(_, _, expr, _, c, a)| Expression::If {
            cond: Box::new(expr),
            consequence: c,
            alternative: a,
        },
    )(input)
}

fn parse_else_expr(input: Tokens) -> IResult<Tokens, Option<Program>> {
    opt(preceded(else_tag, parse_block_stmt))(input)
}

fn empty_params(input: Tokens) -> IResult<Tokens, Vec<Ident>> {
    Ok((input, vec![]))
}

fn parse_fn_expr(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        tuple((
            function_tag,
            lparen_tag,
            alt((parse_params, empty_params)),
            rparen_tag,
            parse_block_stmt,
        )),
        |(_, _, p, _, b)| Expression::Fn { params: p, body: b },
    )(input)
}

fn parse_params(input: Tokens) -> IResult<Tokens, Vec<Ident>> {
    map(
        pair(parse_ident, many0(preceded(comma_tag, parse_ident))),
        |(p, ps)| [&vec![p][..], &ps[..]].concat(),
    )(input)
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
    use crate::math::numbers::Number;
    use crate::styles::style::Color;
    use crate::styles::style::Color::Rgba;
    use palette::{Hsla, Srgba};

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

    fn assert_infix(input: &[u8], infix: Infix) {
        let program: Program = vec![Statement::Expression(Expression::Infix(
            infix,
            Box::new(Expression::Literal(Literal::NumberLiteral(
                Number::UnsignedInteger(10),
            ))),
            Box::new(Expression::Literal(Literal::NumberLiteral(
                Number::UnsignedInteger(20),
            ))),
        ))];

        assert_input_with_program(input, program);
    }

    #[test]
    fn empty() {
        assert_input_with_program(&b""[..], vec![]);
    }

    #[test]
    fn let_statements() {
        let input = "\
        let x = 5;\
        let y = 10;\
        let foobar = 838383;\
        let boo = true;\
        "
        .as_bytes();

        let program: Program = vec![
            Statement::Let(
                Ident("x".to_owned()),
                None,
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(5))),
            ),
            Statement::Let(
                Ident("y".to_owned()),
                None,
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(10))),
            ),
            Statement::Let(
                Ident("foobar".to_owned()),
                None,
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(838383))),
            ),
            Statement::Let(
                Ident("boo".to_owned()),
                None,
                Expression::Literal(Literal::BoolLiteral(true)),
            ),
        ];

        assert_input_with_program(input, program);
    }

    #[test]
    fn prop_statements() {
        let input = "\
        age: 75;\
        is_active: true;\
        is_alive: false;\
        id: my_id;
        "
        .as_bytes();

        let program: Program = vec![
            Statement::Prop(
                Ident("age".to_owned()),
                Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(75))),
            ),
            Statement::Prop(
                Ident("is_active".to_owned()),
                Expression::Literal(Literal::BoolLiteral(true)),
            ),
            Statement::Prop(
                Ident("is_alive".to_owned()),
                Expression::Literal(Literal::BoolLiteral(false)),
            ),
            Statement::Prop(
                Ident("id".to_owned()),
                Expression::Identifier(Ident("my_id".to_owned())),
            )
        ];

        assert_input_with_program(input, program);
    }

    #[test]
    fn create_component() {
        let input = "\
        component Test {\
            let value = \"Hello World!\";
            let color = \"#ff0000\";
            let age: [enculer, pd] = 26;

            render {
                text {
                    content: \"Test\";
                    background: color;
                }
            }
        }\
        ".as_bytes();

        let program: Program = vec![
            Statement::Component { 
                ident: Keyword::Some(Ident("Test".to_owned())),
                body: vec![
                    Statement::Let(
                        Ident("value".to_owned()),
                        None,
                        Expression::Literal(Literal::StringLiteral("Hello World!".to_owned())),
                    ),
                    Statement::Let(
                        Ident("color".to_owned()),
                        None,
                        Expression::Literal(Literal::StringLiteral("#ff0000".to_owned())),
                    ),
                    Statement::Let(
                        Ident("age".to_owned()),
                        Some(Expression::Array(vec![
                            Expression::Identifier(Ident("enculer".to_owned())),
                            Expression::Identifier(Ident("pd".to_owned())),
                        ])),
                        Expression::Literal(Literal::NumberLiteral(Number::UnsignedInteger(26))),
                    ),
                    Statement::NamedBlock {
                        ident: Ident("render".to_owned()),
                        body: vec![
                            Statement::NamedBlock {
                                ident: Ident("text".to_owned()),
                                body: vec![
                                    Statement::Prop(
                                        Ident("content".to_owned()),
                                        Expression::Literal(Literal::StringLiteral("Test".to_owned())),
                                    ),
                                    Statement::Prop(
                                        Ident("background".to_owned()),
                                        Expression::Identifier(Ident("color".to_owned())),
                                    )
                                ]
                            }
                        ]
                    }
                ]
            }
        ];

        assert_input_with_program(input, program);
    }

    #[test]
    fn component_statement() {
        let input = "\
        button {\
            style {\
                width: px(100);\
                height: vh(50.0);\
                visible: true;\
            }\
        }\
        "
        .as_bytes();

        let program: Program = vec![Statement::NamedBlock {
            ident: Ident("button".to_owned()),
            body: vec![Statement::NamedBlock {
                ident: Ident("style".to_owned()),
                body: vec![
                    Statement::Prop(
                        Ident("width".to_owned()),
                        Expression::Call {
                            function: Box::new(Expression::Identifier(Ident("px".to_owned()))),
                            arguments: vec![Expression::Literal(Literal::NumberLiteral(
                                Number::UnsignedInteger(100),
                            ))],
                        },
                    ),
                    Statement::Prop(
                        Ident("height".to_owned()),
                        Expression::Call {
                            function: Box::new(Expression::Identifier(Ident("vh".to_owned()))),
                            arguments: vec![Expression::Literal(Literal::NumberLiteral(
                                Number::Float(50.0),
                            ))],
                        },
                    ),
                    Statement::Prop(
                        Ident("visible".to_owned()),
                        Expression::Literal(Literal::BoolLiteral(true)),
                    ),
                ],
            }],
        }];

        assert_input_with_program(input, program);
    }

    #[test]
    fn infix_expr() {
        let input = "10 + 20".as_bytes();
        assert_infix(input, Infix::Plus);

        let input = "10 * 20".as_bytes();
        assert_infix(input, Infix::Multiply);

        let input = "10 - 20".as_bytes();
        assert_infix(input, Infix::Minus);

        let input = "10 / 20".as_bytes();
        assert_infix(input, Infix::Divide);

        let input = "10 % 20".as_bytes();
        assert_infix(input, Infix::Modulo);

        let input = "10 + 5 / -20 - (x + x)".as_bytes();
        let input2 = "10 + (5 / (-20)) - (x + x)".as_bytes();
        compare_inputs(input, input2);

        let input = "10 + 5 / -20 - (x + x)".as_bytes();

        let program: Program = vec![Statement::Expression(Expression::Infix(
            Infix::Minus,
            Box::new(Expression::Infix(
                Infix::Plus,
                Box::new(Expression::Literal(Literal::NumberLiteral(
                    Number::UnsignedInteger(10),
                ))),
                Box::new(Expression::Infix(
                    Infix::Divide,
                    Box::new(Expression::Literal(Literal::NumberLiteral(
                        Number::UnsignedInteger(5),
                    ))),
                    Box::new(Expression::Prefix(
                        Prefix::PrefixMinus,
                        Box::new(Expression::Literal(Literal::NumberLiteral(
                            Number::UnsignedInteger(20),
                        ))),
                    )),
                )),
            )),
            Box::new(Expression::Infix(
                Infix::Plus,
                Box::new(Expression::Identifier(Ident("x".to_owned()))),
                Box::new(Expression::Identifier(Ident("x".to_owned()))),
            )),
        ))];

        assert_input_with_program(input, program);
    }

    #[test]
    fn op_precedence() {
        let input = "!-a".as_bytes();
        let input2 = "(!(-a))".as_bytes();
        compare_inputs(input, input2);

        let input = "a + b + c".as_bytes();
        let input2 = "((a + b) + c)".as_bytes();
        compare_inputs(input, input2);

        let input = "a + b - c".as_bytes();
        let input2 = "((a + b) - c)".as_bytes();
        compare_inputs(input, input2);

        let input = "a * b * c".as_bytes();
        let input2 = "((a * b) * c)".as_bytes();
        compare_inputs(input, input2);

        let input = "a * b / c".as_bytes();
        let input2 = "((a * b) / c)".as_bytes();
        compare_inputs(input, input2);

        let input = "a + b / c".as_bytes();
        let input2 = "(a + (b / c))".as_bytes();
        compare_inputs(input, input2);

        let input = "a + b * c + d / e - f".as_bytes();
        let input2 = "(((a + (b * c)) + (d / e)) - f)".as_bytes();
        compare_inputs(input, input2);

        let input = "3 + 4; -5 * 5".as_bytes();
        let input2 = "(3 + 4);((-5) * 5)".as_bytes();
        compare_inputs(input, input2);

        let input = "5 > 4 == 3 < 4".as_bytes();
        let input2 = "((5 > 4) == (3 < 4))".as_bytes();
        compare_inputs(input, input2);

        let input = "5 < 4 != 3 > 4".as_bytes();
        let input2 = "((5 < 4) != (3 > 4))".as_bytes();
        compare_inputs(input, input2);

        let input = "3 + 4 * 5 == 3 * 1 + 4 * 5".as_bytes();
        let input2 = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".as_bytes();
        compare_inputs(input, input2);
    }
}
