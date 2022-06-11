pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Identifier /* , Expression */),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Not,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equal,
    NotEqual,
}
