use crate::lexer::token::{Span, Token, TokenKind};
use crate::lexer::Lexer;
use crate::parser::precedences::{get_token_precedence, Precedence};
use crate::parser::declaration::*;

use self::ast::{Program, StringType, Boolean, Integer, Identifier, Array, FunctionDeclaration, FunctionCall, Index, Node, Literal, If, Hash};
use self::expression::{Expression, UnaryExpression, BinaryExpression};
use self::statement::{Statement, BlockStatement, LetStatement, ReturnStatement};

pub mod ast;
pub mod expression;
pub mod precedences;
pub mod statement;
pub mod declaration;
mod tests;

type ParseError = String;
type ParseErrors = Vec<ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: ParseErrors,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let cur = lexer.next_token();
        let next = lexer.next_token();
        let errors = Vec::new();
        // in strict sense, rust can be as classic go pattern, but it requires more work
        // so let's just use pattern matching
        // ```rust
        // type PrefixParseFn = fn() -> Result<Expression, ParseError>;
        // type InfixParseFn = fn(Expression) -> Result<Expression, ParseError>;
        // let prefix_parse_fns = HashMap::new();
        // let infix_parse_fns = HashMap::new();
        // ```

        let p = Parser {
            lexer,
            current_token: cur,
            peek_token: next,
            errors,
        };

        return p;
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, token: &TokenKind) -> bool {
        self.current_token.kind == *token
    }

    fn peek_token_is(&mut self, token: &TokenKind) -> bool {
        self.peek_token.kind == *token
    }

    fn expect_peek(&mut self, token: &TokenKind) -> Result<(), ParseError> {
        self.next_token();
        if self.current_token.kind == *token {
            Ok(())
        } else {
            let e = format!("expected token: {} got: {}", token, self.current_token);
            Err(e)
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        let mut program = Program::new();

        while !self.current_token_is(&TokenKind::EOF) {
            match self.parse_statement() {
                Ok(stmt) => program.body.push(stmt),
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }

        program.span.end = self.current_token.span.end;

        if self.errors.is_empty() {
            return Ok(program);
        } else {
            return Err(self.errors.clone());
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Component => self.parse_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_declaration(&mut self) -> Result<Statement, ParseError> {
        match self.current_token.kind {
            TokenKind::Component => self.parse_component_declaration(),
            _ => Err("expected declaration".to_string()),
        }
    }

    fn parse_component_declaration(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();

        let mut block_statement = Vec::new();
        let name = self.current_token.clone();
        let mut identifier_name = "".to_string();
        
        match &self.current_token.kind {
            TokenKind::Identifier { name } => {
                identifier_name = name.to_string();
            }
            _ => return Err(format!("{} not an identifier", self.current_token)),
        };

        self.next_token();

        while !self.current_token_is(&TokenKind::RBrace) && !self.current_token_is(&TokenKind::EOF)
        {
            if let Ok(statement) = self.parse_statement() {
                block_statement.push(statement)
            }

            self.next_token();
        }

        // self.expect_peek(&TokenKind::LBrace)?;
        // self.next_token();

        // let consequent = self.parse_block_statement()?;

        // if self.peek_token_is(&TokenKind::RBrace) {
        //     self.next_token();
        // }

        let end = self.current_token.span.end;
        println!("body: {:?}", block_statement);

        return Ok(Statement::Declaration(Declaration::Component(ComponentDeclaration {
            name,
            body: block_statement,
            span: Span { start, end },
        })));
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();

        let name = self.current_token.clone();
        let mut identifier_name = "".to_string();
        
        match &self.current_token.kind {
            TokenKind::Identifier { name } => {
                identifier_name = name.to_string();
            }
            _ => return Err(format!("{} not an identifier", self.current_token)),
        };

        self.expect_peek(&TokenKind::Assign)?;
        self.next_token();

        let mut value = self.parse_expression(Precedence::Lowest)?.0;

        match value {
            Expression::Function(ref mut f) => {
                f.name = identifier_name;
            }
            _ => {}
        }

        if self.peek_token_is(&TokenKind::SemiColon) {
            self.next_token();
        }

        let end = self.current_token.span.end;

        return Ok(Statement::Let(LetStatement {
            identifier: name,
            expr: value,
            span: Span { start, end },
        }));
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?.0;

        if self.peek_token_is(&TokenKind::SemiColon) {
            self.next_token();
        }
        let end = self.current_token.span.end;

        return Ok(Statement::Return(ReturnStatement {
            argument: value,
            span: Span { start, end },
        }));
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest)?.0;
        if self.peek_token_is(&TokenKind::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Expr(expr))
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<(Expression, Span), ParseError> {
        let mut left_start = self.current_token.span.start;
        let mut left = self.parse_prefix_expression()?;
        while self.peek_token.kind != TokenKind::SemiColon
            && precedence < get_token_precedence(&self.peek_token.kind)
        {
            match self.parse_infix_expression(&left, left_start) {
                Some(infix) => {
                    left = infix?;
                    if let Expression::Infix(b) = left.clone() {
                        left_start = b.span.start;
                    }
                }
                None => {
                    return Ok((
                        left,
                        Span {
                            start: left_start,
                            end: self.current_token.span.end,
                        },
                    ))
                }
            }
        }

        let end = self.current_token.span.end;

        Ok((
            left,
            Span {
                start: left_start,
                end,
            },
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        // this is prefix fn map :)
        match &self.current_token.kind {
            TokenKind::Identifier { name } => {
                return Ok(Expression::Identifier(Identifier {
                    name: name.clone(),
                    span: self.current_token.clone().span,
                }))
            }
            TokenKind::Integer(i) => {
                return Ok(Expression::Literal(Literal::Integer(Integer {
                    raw: *i,
                    span: self.current_token.clone().span,
                })))
            }
            TokenKind::String(s) => {
                return Ok(Expression::Literal(Literal::String(StringType {
                    raw: s.to_string(),
                    span: self.current_token.clone().span,
                })))
            }
            b @ TokenKind::True | b @ TokenKind::False => {
                return Ok(Expression::Literal(Literal::Boolean(Boolean {
                    raw: *b == TokenKind::True,
                    span: self.current_token.clone().span,
                })))
            }
            TokenKind::Bang | TokenKind::Minus => {
                let start = self.current_token.span.start;
                let prefix_op = self.current_token.clone();
                self.next_token();
                let (expr, span) = self.parse_expression(Precedence::Prefix)?;
                return Ok(Expression::Prefix(UnaryExpression {
                    op: prefix_op,
                    operand: Box::new(expr),
                    span: Span {
                        start,
                        end: span.end,
                    },
                }));
            }
            TokenKind::LParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest)?.0;
                self.expect_peek(&TokenKind::RParen)?;
                return Ok(expr);
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::Function => self.parse_fn_expression(),
            TokenKind::LBracket => {
                let (elements, span) = self.parse_expression_list(&TokenKind::RBracket)?;
                return Ok(Expression::Literal(Literal::Array(Array {
                    elements,
                    span,
                })));
            }
            TokenKind::LBrace => self.parse_hash_expression(),
            _ => Err(format!(
                "no prefix function for token: {}",
                self.current_token
            )),
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: &Expression,
        left_start: usize,
    ) -> Option<Result<Expression, ParseError>> {
        match self.peek_token.kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::LessThan
            | TokenKind::GreaterThan => {
                self.next_token();
                let infix_op = self.current_token.clone();
                let precedence_value = get_token_precedence(&self.current_token.kind);
                self.next_token();
                let (right, span) = self.parse_expression(precedence_value).unwrap();
                return Some(Ok(Expression::Infix(BinaryExpression {
                    op: infix_op,
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    span: Span {
                        start: left_start,
                        end: span.end,
                    },
                })));
            }
            TokenKind::LParen => {
                self.next_token();
                return Some(self.parse_fn_call_expression(left.clone()));
            }
            TokenKind::LBracket => {
                self.next_token();
                return Some(self.parse_index_expression(left.clone()));
            }
            _ => None,
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;
        self.expect_peek(&TokenKind::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(&TokenKind::RParen)?;
        self.expect_peek(&TokenKind::LBrace)?;

        let consequent = self.parse_block_statement()?;

        let alternate = if self.peek_token_is(&TokenKind::Else) {
            self.next_token();
            self.expect_peek(&TokenKind::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        let end = self.current_token.span.end;

        return Ok(Expression::If(If {
            condition: Box::new(condition),
            consequent,
            alternate,
            span: Span { start, end },
        }));
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();
        let mut block_statement = Vec::new();

        while !self.current_token_is(&TokenKind::RBrace) && !self.current_token_is(&TokenKind::EOF)
        {
            if let Ok(statement) = self.parse_statement() {
                block_statement.push(statement)
            }

            self.next_token();
        }

        let end = self.current_token.span.end;

        Ok(BlockStatement {
            body: block_statement,
            span: Span { start, end },
        })
    }

    fn parse_fn_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;
        self.expect_peek(&TokenKind::LParen)?;

        let params = self.parse_fn_parameters()?;

        self.expect_peek(&TokenKind::LBrace)?;

        let function_body = self.parse_block_statement()?;

        let end = self.current_token.span.end;

        Ok(Expression::Function(FunctionDeclaration {
            params,
            body: function_body,
            span: Span { start, end },
            name: "".to_string(),
        }))
    }

    fn parse_fn_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut params = Vec::new();
        if self.peek_token_is(&TokenKind::RParen) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();

        match &self.current_token.kind {
            TokenKind::Identifier { name } => params.push(Identifier {
                name: name.clone(),
                span: self.current_token.span.clone(),
            }),
            token => {
                return Err(format!(
                    "expected function params  to be an identifier, got {}",
                    token
                ))
            }
        }

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();
            match &self.current_token.kind {
                TokenKind::Identifier { name } => params.push(Identifier {
                    name: name.clone(),
                    span: self.current_token.span.clone(),
                }),
                token => {
                    return Err(format!(
                        "expected function params  to be an identifier, got {}",
                        token
                    ))
                }
            }
        }

        self.expect_peek(&TokenKind::RParen)?;

        return Ok(params);
    }

    fn parse_fn_call_expression(&mut self, expr: Expression) -> Result<Expression, ParseError> {
        // fake positive
        #[allow(unused_assignments)]
        let mut start = self.current_token.span.start;
        let (arguments, ..) = self.parse_expression_list(&TokenKind::RParen)?;
        let end = self.current_token.span.end;
        match &expr {
            Expression::Identifier(i) => start = i.span.start,
            Expression::Function(f) => start = f.span.start,
            _ => return Err(format!("expected function")),
        }
        let callee = Box::new(expr);

        Ok(Expression::FunctionCall(FunctionCall {
            callee,
            arguments,
            span: Span { start, end },
        }))
    }

    fn parse_expression_list(
        &mut self,
        end: &TokenKind,
    ) -> Result<(Vec<Expression>, Span), ParseError> {
        let start = self.current_token.span.start;
        let mut expr_list = Vec::new();
        if self.peek_token_is(end) {
            self.next_token();
            let end = self.current_token.span.end;
            return Ok((expr_list, Span { start, end }));
        }

        self.next_token();

        expr_list.push(self.parse_expression(Precedence::Lowest)?.0);

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();
            expr_list.push(self.parse_expression(Precedence::Lowest)?.0);
        }

        self.expect_peek(end)?;
        let end = self.current_token.span.end;

        return Ok((expr_list, Span { start, end }));
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?.0;

        self.expect_peek(&TokenKind::RBracket)?;

        let end = self.current_token.span.end;

        return Ok(Expression::Index(Index {
            object: Box::new(left),
            index: Box::new(index),
            span: Span { start, end },
        }));
    }

    fn parse_hash_expression(&mut self) -> Result<Expression, ParseError> {
        let mut map = Vec::new();
        let start = self.current_token.span.start;
        while !self.peek_token_is(&TokenKind::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?.0;

            self.expect_peek(&TokenKind::Colon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?.0;

            map.push((key, value));

            if !self.peek_token_is(&TokenKind::RBrace) {
                self.expect_peek(&TokenKind::Comma)?;
            }
        }

        self.expect_peek(&TokenKind::RBrace)?;
        let end = self.current_token.span.end;

        Ok(Expression::Literal(Literal::Hash(Hash {
            elements: map,
            span: Span { start, end },
        })))
    }
}

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;

    Ok(Node::Program(program))
}

pub fn parse_ast_json_string(input: &str) -> Result<String, ParseErrors> {
    let ast = match parse(input) {
        Ok(node) => serde_json::to_string_pretty(&node).unwrap(),
        Err(e) => return Err(e),
    };

    return Ok(ast);
}
