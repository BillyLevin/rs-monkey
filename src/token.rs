pub enum Token {
    Illegal,
    Eof,

    // identifiers and literals
    Ident(String),
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
