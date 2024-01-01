use std::fmt;
use std::fmt::Formatter;

use serde::{Serialize, Deserialize};

use crate::{lexer::token::{Token, Span}, parser::ast::format_expressions};

use super::ast::{Identifier, Literal, FunctionDeclaration, FunctionCall, Index, If};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(untagged)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal), // need to flatten
    Prefix(UnaryExpression),
    Infix(BinaryExpression),
    If(If),
    Function(FunctionDeclaration),
    FunctionCall(FunctionCall),
    Index(Index),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct UnaryExpression {
    pub op: Token,
    pub operand: Box<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct BinaryExpression {
    pub op: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(Identifier { name: id, .. }) => write!(f, "{}", id),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Prefix(UnaryExpression {
                op, operand: expr, ..
            }) => {
                write!(f, "({}{})", op.kind, expr)
            }
            Expression::Infix(BinaryExpression {
                op, left, right, ..
            }) => {
                write!(f, "({} {} {})", left, op.kind, right)
            }
            Expression::If(If {
                condition,
                consequent,
                alternate,
                ..
            }) => {
                if let Some(else_block) = alternate {
                    write!(
                        f,
                        "if {} {{ {} }} else {{ {} }}",
                        condition, consequent, else_block,
                    )
                } else {
                    write!(f, "if {} {{ {} }}", condition, consequent,)
                }
            }
            Expression::Function(FunctionDeclaration {
                name, params, body, ..
            }) => {
                let func_params = params
                    .iter()
                    .map(|stmt| stmt.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fn {}({}) {{ {} }}", name, func_params, body)
            }
            Expression::FunctionCall(FunctionCall {
                callee, arguments, ..
            }) => {
                write!(f, "{}({})", callee, format_expressions(arguments))
            }
            Expression::Index(Index { object, index, .. }) => {
                write!(f, "({}[{}])", object, index)
            }
        }
    }
}