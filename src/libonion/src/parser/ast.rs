use serde::{Serialize, Deserialize};
use std::fmt::{Formatter, Display, Result};

use crate::lexer::token::{Span, Token, TokenKind};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Program {
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(untagged)]
pub enum Statement {
    Let(Let),
    Expression(Expression),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(untagged)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub enum Literal {
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
pub struct Integer {
    pub raw: i64,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
pub struct Boolean {
    pub raw: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
pub struct StringType {
    pub raw: String,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Let {
    pub identifier: Token,
    pub expression: Expression,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub span: Span,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Identifier(Identifier { name: id, .. }) => write!(f, "{}", id),
            Expression::Literal(l) => write!(f, "{}", l),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::Integer(Integer { raw: i, .. }) => write!(f, "{}", i),
            Literal::Boolean(Boolean { raw: b, .. }) => write!(f, "{}", b),
            Literal::String(StringType { raw: s, .. }) => write!(f, "\"{}\"", s),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(Let { identifier: id, expression, .. }) => {
                if let TokenKind::Identifier(name) = &id.kind {
                    return write!(f, "let {} = {};", name, expression);
                }
                panic!("unreachable")
            }
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(stmt) => write!(f, "{}", stmt),
            Node::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_statements(&self.body))
    }
}

impl Program {
    pub fn new() -> Self {
        Self { body: vec![], span: Span { start: 0, end: 0 } }
    }
}

fn format_statements(statements: &Vec<Statement>) -> String {
    return statements
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("");
}

fn format_expressions(exprs: &Vec<Expression>) -> String {
    return exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(", ");
}