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
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);
