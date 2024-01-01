use std::fmt;
use std::fmt::{Formatter, Result};
use serde::{Serialize, Deserialize};

use crate::lexer::token::{Token, Span, TokenKind};
use crate::parser::ast::format_statements;

use super::statement::Statement;

#[derive(Clone, Debug, Eq, Serialize, Deserialize, Hash, PartialEq)]
#[serde(untagged)]
pub enum Declaration {
    Component(ComponentDeclaration),
    // Page..
    // Layout..
}

#[derive(Clone, Debug, Eq, Hash, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub struct ComponentDeclaration {
    pub name: Token,
    pub body: Vec<Statement>,
    pub span: Span,
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Declaration::Component(ComponentDeclaration {
                name: id,
                ..
            }) => {
                if let TokenKind::Identifier { name } = &id.kind {
                    return write!(f, "component {} {{}}", name);
                }
                panic!("unreachable")
            },
        }
    }
}

impl fmt::Display for ComponentDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", format_statements(&self.body))
    }
}