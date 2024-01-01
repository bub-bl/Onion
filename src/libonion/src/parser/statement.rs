use std::fmt;
use std::fmt::{Formatter, Result};
use serde::{Serialize, Deserialize};

use crate::lexer::token::{Span, Token, TokenKind};
use crate::parser::ast::format_statements;

use super::declaration::Declaration;
use super::expression::Expression;

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(untagged)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(Expression),
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct LetStatement {
    pub identifier: Token, // rust can't do precise type with enum
    pub expr: Expression,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct ReturnStatement {
    pub argument: Expression,
    pub span: Span,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(LetStatement {
                identifier: id,
                expr,
                ..
            }) => {
                if let TokenKind::Identifier { name } = &id.kind {
                    return write!(f, "let {} = {};", name, expr);
                }
                panic!("unreachable")
            }
            Statement::Return(ReturnStatement { argument, .. }) => {
                write!(f, "return {};", argument)
            }
            Statement::Expr(expr) => write!(f, "{}", expr),
        }
    }
}