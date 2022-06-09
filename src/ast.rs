pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Identifier /* , Expression */),
}

pub enum Expression {}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);
