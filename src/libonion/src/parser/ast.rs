use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result};

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
    Return(ReturnStatement),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(untagged)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(UnaryExpression),
    Infix(BinaryExpression),
    If(If),
    Function(FunctionDeclaration),
    FunctionCall(FunctionCall),
    Index(Index),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct ReturnStatement {
    pub argument: Expression,
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

#[derive(Clone, Debug, Serialize, Deserialize, Eq, Hash, PartialEq)]
#[serde(tag = "type")]
pub struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
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
pub enum Literal {
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
    Array(Array),
    Hash(Hash),
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

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or =
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.name)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(Let {
                identifier: id,
                expression,
                ..
            }) => {
                if let TokenKind::Identifier(name) = &id.kind {
                    return write!(f, "let {} = {};", name, expression);
                }
                panic!("unreachable")
            }
            Statement::Return(ReturnStatement { argument, .. }) => {
                write!(f, "return {};", argument)
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
        Self {
            body: vec![],
            span: Span { start: 0, end: 0 },
        }
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

pub fn get_token_precedence(token: &TokenKind) -> Precedence {
    match token {
        TokenKind::Equal => Precedence::Equals,
        TokenKind::NotEqual => Precedence::Equals,
        TokenKind::LessThan => Precedence::LessGreater,
        TokenKind::GreaterThan => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Asterisk => Precedence::Product,
        TokenKind::Slash => Precedence::Product,
        TokenKind::LParen => Precedence::Call,
        TokenKind::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
