use crate::math::numbers::Number;

pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(Ident, Expression),
    Prop(Ident, Expression),
    Component { ident: Ident, body: Program },
    NamedBlock { ident: Ident, body: Program },
    Return(Expression),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        cond: Box<Expression>,
        consequence: Program,
        alternative: Option<Program>,
    },
    Fn {
        params: Vec<Ident>,
        body: Program,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Array(Vec<Expression>),
    Hash(Vec<(Literal, Expression)>),
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    NumberLiteral(Number),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Call,
    Index,
}
