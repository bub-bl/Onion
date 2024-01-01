use crate::lexer::token::Span;
use core::fmt;
use core::fmt::Result;
use serde::{Deserialize, Serialize};
use std::fmt::Formatter;

use super::{expression::Expression, statement::{Statement, BlockStatement}};

// still wait for https://github.com/serde-rs/serde/issues/1402
#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(stmt) => write!(f, "{}", stmt),
            Node::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Program {
    pub body: Vec<Statement>,
    // pub body: Vec<Declaration>,
    pub span: Span,
}

impl Program {
    pub fn new() -> Self {
        Program {
            body: vec![],
            span: Span { start: 0, end: 0 },
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.name)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct FunctionDeclaration {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub span: Span,
    pub name: String,
}

// function can be Identifier or FunctionLiteral (think iife)
#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
pub struct Integer {
    pub raw: i64,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
pub struct Boolean {
    pub raw: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
pub struct StringType {
    pub raw: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
pub struct Array {
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
pub struct Hash {
    pub elements: Vec<(Expression, Expression)>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct If {
    pub condition: Box<Expression>,
    pub consequent: BlockStatement,
    pub alternate: Option<BlockStatement>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(tag = "type")]
pub enum Literal {
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
    Array(Array),
    Hash(Hash),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(Integer { raw: i, .. }) => write!(f, "{}", i),
            Literal::Boolean(Boolean { raw: b, .. }) => write!(f, "{}", b),
            Literal::String(StringType { raw: s, .. }) => write!(f, "\"{}\"", s),
            Literal::Array(Array { elements: e, .. }) => write!(f, "[{}]", format_expressions(e)),
            Literal::Hash(Hash { elements: map, .. }) => {
                let to_string = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{{{}}}", to_string)
            }
        }
    }
}

pub(crate) fn format_expressions(exprs: &Vec<Expression>) -> String {
    return exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(", ");
}

pub(crate) fn format_statements(statements: &Vec<Statement>) -> String {
    return statements
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("");
}