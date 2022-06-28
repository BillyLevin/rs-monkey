use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };

        // necessary to move `position` and `read_position` from their default positions
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'<' => Token::LessThan,
            b'>' => Token::GreaterThan,
            b'(' => Token::LeftParen,
            b')' => Token::RightParen,
            b'{' => Token::LeftBrace,
            b'}' => Token::RightBrace,
            b',' => Token::Comma,
            b';' => Token::SemiColon,
            b'a'..=b'z' | b'A'..=b'Z' => return self.read_identifier(),
            b'0'..=b'9' => return self.read_number(),
            b'"' => return self.read_string(),
            0 => Token::Eof,
            _ => Token::Illegal,
        };

        self.read_char();

        token
    }

    fn skip_whitespace(&mut self) {
        while let b' ' | b'\t' | b'\n' | b'\r' = self.ch {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;

        while let b'a'..=b'z' | b'A'..=b'Z' = self.ch {
            self.read_char();
        }

        let identifier = &self.input[position..self.position];

        match identifier {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(String::from(identifier)),
        }
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;

        while let b'0'..=b'9' = self.ch {
            self.read_char();
        }

        let number = &self.input[position..self.position];

        Token::Int(number.parse().unwrap())
    }

    fn read_string(&mut self) -> Token {
        self.read_char();

        let position = self.position;

        loop {
            match self.ch {
                b'"' | 0 => break,
                _ => self.read_char(),
            }
        }

        let string = &self.input[position..self.position];

        self.read_char();

        Token::String(string.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input = "
        let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        \"foobar\";
        \"foo bar\";
        ";

        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::SemiColon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RightParen,
            Token::LeftBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::SemiColon,
            Token::RightBrace,
            Token::SemiColon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LeftParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RightParen,
            Token::SemiColon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::SemiColon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::SemiColon,
            Token::If,
            Token::LeftParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::SemiColon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::SemiColon,
            Token::RightBrace,
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::SemiColon,
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::SemiColon,
            Token::String(String::from("foobar")),
            Token::SemiColon,
            Token::String(String::from("foo bar")),
            Token::SemiColon,
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
