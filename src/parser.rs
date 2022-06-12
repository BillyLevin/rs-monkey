use crate::{
    ast::{BlockStatement, Expression, Identifier, Infix, Literal, Prefix, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd)]
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

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::Eof) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
        }

        statements
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
        let mut left = self.parse_prefix()?;

        while !self.peek_token_is(Token::SemiColon) && precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::GreaterThan
                | Token::LessThan
                | Token::Equal
                | Token::NotEqual => {
                    self.next_token();
                    left = self.parse_infix_expression(left)?;
                }
                Token::LeftParen => {
                    self.next_token();
                    left = self.parse_call_expression(left)?;
                }
                _ => return Some(left),
            }
        }

        Some(left)
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Int(_) => Some(self.parse_integer_literal()),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => Some(self.parse_boolean_literal_expression()),
            Token::LeftParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal_expression(),
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        let identifier = self.parse_identifier()?;

        Some(Expression::Identifier(identifier))
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.current_token {
            Token::Identifier(ref name) => Some(Identifier(name.to_string())),
            _ => None,
        }
    }

    fn parse_integer_literal(&mut self) -> Expression {
        match self.current_token {
            Token::Int(num) => Expression::Literal(Literal::Int(num)),
            _ => unreachable!(),
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = match self.current_token {
            Token::Bang => Prefix::Not,
            Token::Minus => Prefix::Minus,
            _ => unreachable!(),
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        return Some(Expression::Prefix(operator, Box::new(right)));
    }

    fn parse_boolean_literal_expression(&mut self) -> Expression {
        let value = self.current_token_is(Token::True);

        Expression::Literal(Literal::Boolean(value))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::RightParen) {
            return None;
        }

        Some(expression)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LeftParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::RightParen) {
            return None;
        }

        if !self.expect_peek(Token::LeftBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let mut alternative = None;

        if self.peek_token_is(Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::LeftBrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_function_literal_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::LeftParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Token::LeftBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(Token::RightParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(self.parse_identifier()?);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            identifiers.push(self.parse_identifier()?);
        }

        if !self.expect_peek(Token::RightParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Multiply,
            Token::Slash => Infix::Divide,
            Token::GreaterThan => Infix::GreaterThan,
            Token::LessThan => Infix::LessThan,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            _ => return None,
        };

        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;
        Some(Expression::Infix(Box::new(left), infix, Box::new(right)))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = self.parse_call_arguments()?;

        Some(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut arguments = Vec::new();

        if self.peek_token_is(Token::RightParen) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(Token::RightParen) {
            return None;
        }

        Some(arguments)
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

    fn peek_precedence(&mut self) -> Precedence {
        Self::get_token_precedence(&self.peek_token)
    }

    fn current_precedence(&mut self) -> Precedence {
        Self::get_token_precedence(&self.current_token)
    }

    fn get_token_precedence(token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LessThan | Token::GreaterThan => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LeftParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
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

    #[test]
    fn parse_boolean_literal_expressions() {
        let input = "
        true;
        false;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![
                Statement::Expression(Expression::Literal(Literal::Boolean(true))),
                Statement::Expression(Expression::Literal(Literal::Boolean(false))),
            ]
        );
    }

    #[test]
    fn parse_prefix_expressions() {
        let tests = vec![
            (
                "!5;",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "-15;",
                Statement::Expression(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(Expression::Literal(Literal::Int(15))),
                )),
            ),
            (
                "!true",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Literal(Literal::Boolean(true))),
                )),
            ),
            (
                "!false",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Literal(Literal::Boolean(false))),
                )),
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            check_parser_errors(parser);

            assert_eq!(program, vec![expected]);
        }
    }

    #[test]
    fn parse_infix_expressions() {
        let tests = vec![
            (
                "5 + 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 - 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::Minus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 * 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::Multiply,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 / 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::Divide,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 > 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::GreaterThan,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 < 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::LessThan,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 == 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 != 5;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Infix::NotEqual,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            ),
            (
                "true == true;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Boolean(true))),
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Boolean(true))),
                )),
            ),
            (
                "true != false;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Boolean(true))),
                    Infix::NotEqual,
                    Box::new(Expression::Literal(Literal::Boolean(false))),
                )),
            ),
            (
                "false == false;",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Boolean(false))),
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Boolean(false))),
                )),
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            check_parser_errors(parser);

            assert_eq!(program, vec![expected]);
        }
    }

    #[test]
    fn parse_operator_precedence() {
        let tests = vec![
            (
                "-a * b",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                    Infix::Multiply,
                    Box::new(Expression::Identifier(Identifier(String::from("b")))),
                )),
            ),
            (
                "!-a",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                )),
            ),
            (
                "a + b + c",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b - c",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Infix::Minus,
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b * c",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Infix::Multiply,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Infix::Multiply,
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b / c",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Infix::Multiply,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Infix::Divide,
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b / c",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                        Infix::Divide,
                        Box::new(Expression::Identifier(Identifier(String::from("c")))),
                    )),
                )),
            ),
            (
                "a + b * c + d / e - f",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Identifier(Identifier(String::from("a")))),
                            Infix::Plus,
                            Box::new(Expression::Infix(
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Infix::Multiply,
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )),
                        )),
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Identifier(Identifier(String::from("d")))),
                            Infix::Divide,
                            Box::new(Expression::Identifier(Identifier(String::from("e")))),
                        )),
                    )),
                    Infix::Minus,
                    Box::new(Expression::Identifier(Identifier(String::from("f")))),
                )),
            ),
            (
                "5 > 4 == 3 < 4",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "5 < 4 != 3 > 4",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                    Infix::NotEqual,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Int(5))),
                        )),
                    )),
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(3))),
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Int(1))),
                        )),
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Int(5))),
                        )),
                    )),
                )),
            ),
            (
                "true",
                Statement::Expression(Expression::Literal(Literal::Boolean(true))),
            ),
            (
                "false",
                Statement::Expression(Expression::Literal(Literal::Boolean(false))),
            ),
            (
                "3 > 5 == false",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Boolean(false))),
                )),
            ),
            (
                "3 < 5 == true",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Boolean(true))),
                )),
            ),
            (
                "1 + (2 + 3) + 4",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(1))),
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(2))),
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Int(3))),
                        )),
                    )),
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(4))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Infix::Multiply,
                    Box::new(Expression::Literal(Literal::Int(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Literal(Literal::Int(2))),
                    Infix::Divide,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "-(5 + 5)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "!(true == true)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Literal(Literal::Boolean(true))),
                        Infix::Equal,
                        Box::new(Expression::Literal(Literal::Boolean(true))),
                    )),
                )),
            ),
            (
                "a + add(b * c) + d",
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Infix::Plus,
                        Box::new(Expression::Call {
                            function: Box::new(Expression::Identifier(Identifier(String::from(
                                "add",
                            )))),
                            arguments: vec![Expression::Infix(
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Infix::Multiply,
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )],
                        }),
                    )),
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                )),
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                Statement::Expression(Expression::Call {
                    function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    arguments: vec![
                        Expression::Identifier(Identifier(String::from("a"))),
                        Expression::Identifier(Identifier(String::from("b"))),
                        Expression::Literal(Literal::Int(1)),
                        Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(2))),
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Int(3))),
                        ),
                        Expression::Infix(
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Int(5))),
                        ),
                        Expression::Call {
                            function: Box::new(Expression::Identifier(Identifier(String::from(
                                "add",
                            )))),
                            arguments: vec![
                                Expression::Literal(Literal::Int(6)),
                                Expression::Infix(
                                    Box::new(Expression::Literal(Literal::Int(7))),
                                    Infix::Multiply,
                                    Box::new(Expression::Literal(Literal::Int(8))),
                                ),
                            ],
                        },
                    ],
                }),
            ),
            (
                "add(a + b + c * d / f + g)",
                Statement::Expression(Expression::Call {
                    function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    arguments: vec![Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Infix(
                                Box::new(Expression::Identifier(Identifier(String::from("a")))),
                                Infix::Plus,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                            )),
                            Infix::Plus,
                            Box::new(Expression::Infix(
                                Box::new(Expression::Infix(
                                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                                    Infix::Multiply,
                                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                                )),
                                Infix::Divide,
                                Box::new(Expression::Identifier(Identifier(String::from("f")))),
                            )),
                        )),
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("g")))),
                    )],
                }),
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            check_parser_errors(parser);

            assert_eq!(program, vec![expected]);
        }
    }

    #[test]
    fn parse_if_expressions() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Infix::LessThan,
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                )),
                consequence: vec![Statement::Expression(Expression::Identifier(Identifier(
                    String::from("x")
                )))],
                alternative: None,
            }),]
        )
    }

    #[test]
    fn parse_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Infix::LessThan,
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                )),
                consequence: vec![Statement::Expression(Expression::Identifier(Identifier(
                    String::from("x")
                )))],
                alternative: Some(vec![Statement::Expression(Expression::Identifier(
                    Identifier(String::from("y"))
                ))]),
            }),]
        )
    }

    #[test]
    fn parse_function_literal_expressions() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::Function {
                parameters: vec![Identifier(String::from("x")), Identifier(String::from("y"))],
                body: vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                ))]
            })]
        )
    }

    #[test]
    fn parse_call_expressions() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program,
            vec![Statement::Expression(Expression::Call {
                function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                arguments: vec![
                    Expression::Literal(Literal::Int(1)),
                    Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(2))),
                        Infix::Multiply,
                        Box::new(Expression::Literal(Literal::Int(3))),
                    ),
                    Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(4))),
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )
                ]
            })]
        )
    }

    #[test]
    fn parse_call_expression_arguments() {
        let tests = vec![
            (
                "add();",
                Expression::Identifier(Identifier(String::from("add"))),
                vec![],
            ),
            (
                "add(1);",
                Expression::Identifier(Identifier(String::from("add"))),
                vec![Expression::Literal(Literal::Int(1))],
            ),
            (
                "add(1, 2 * 3, 4 + 5);",
                Expression::Identifier(Identifier(String::from("add"))),
                vec![
                    Expression::Literal(Literal::Int(1)),
                    Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(2))),
                        Infix::Multiply,
                        Box::new(Expression::Literal(Literal::Int(3))),
                    ),
                    Expression::Infix(
                        Box::new(Expression::Literal(Literal::Int(4))),
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                    ),
                ],
            ),
        ];

        for (input, expected_identifier, expected_args) in tests {
            let lexer = Lexer::new(input);

            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(parser);

            assert_eq!(
                program,
                vec![Statement::Expression(Expression::Call {
                    function: Box::new(expected_identifier),
                    arguments: expected_args,
                })]
            )
        }
    }
}
