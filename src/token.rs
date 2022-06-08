#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers and literals
    Identifier(String),
    Int(i64),

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // delimiters
    Comma,
    SemiColon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // keywords
    Function,
    Let,
}
