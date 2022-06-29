#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers and literals
    Identifier(String),
    Int(i64),
    String(String),

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
    LeftBracket,
    RightBracket,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
