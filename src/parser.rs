use crate::{
    ast::{Expression, Identifier, Literal, Program, Statement},
    lexer::Lexer,
    token::Token,
};

enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        // read two tokens so that `current_token` and `peek_token` both get set
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(self) -> Vec<String> {
        self.errors
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();

        loop {
            match self.current_token {
                Token::Eof => break,
                _ => (),
            }

            if let Some(statement) = self.parse_statement() {
                program.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match self.peek_token {
            Token::Identifier(_) => self.next_token(),
            _ => {
                self.no_identifier_error();
                return None;
            }
        }

        let identifier = match &self.current_token {
            Token::Identifier(name) => Identifier(name.to_string()),
            _ => return None,
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        // TODO: currently we are skipping the expression being assigned to the identifier
        while !self.current_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Let(identifier))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        // semi-colon is optional
        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Return(return_value))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        // semi-colon is optional
        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let left = self.parse_prefix()?;

        Some(left)
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Identifier(ref name) => {
                Some(Expression::Identifier(Identifier(name.to_string())))
            }
            _ => None,
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Int(num) => Some(Expression::Literal(Literal::Int(num))),
            _ => None,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&mut self, token: Token) -> bool {
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        ));
    }

    fn no_identifier_error(&mut self) {
        self.errors.push(format!(
            "expected some Identifier token, got {:?} instead",
            self.peek_token
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_parser_errors(parser: Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());

        for error in errors {
            println!("parser error: {}", error);
        }

        panic!("errors found");
    }

    #[test]
    fn parse_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        // TODO: make this parse the full statement - not just the identifiers
        assert_eq!(
            program,
            vec![
                (Statement::Let(Identifier(String::from("x")))),
                (Statement::Let(Identifier(String::from("y")))),
                (Statement::Let(Identifier(String::from("foobar")))),
            ]
        );
    }

    #[test]
    fn parse_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![
                Statement::Return(Expression::Literal(Literal::Int(5))),
                Statement::Return(Expression::Literal(Literal::Int(10))),
                Statement::Return(Expression::Literal(Literal::Int(993322)))
            ]
        )
    }

    #[test]
    fn parse_identifier_expressions() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::Identifier(Identifier(
                String::from("foobar")
            )))]
        )
    }

    #[test]
    fn parse_integer_literal_expressions() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::Literal(Literal::Int(5)))]
        )
    }
}