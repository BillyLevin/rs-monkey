use crate::{
    ast::{Expression, Literal, Program, Statement},
    object::Object,
};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {}
    }

    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program {
            result = self.eval_statement(statement);
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            _ => None,
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            _ => None,
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Int(num) => Object::Int(num),
            Literal::Boolean(boolean) => Object::Boolean(boolean),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let mut evaluator = Evaluator::new();
        evaluator.eval(program)
    }

    #[test]
    fn eval_integer_expressions() {
        let tests = vec![("5", Some(Object::Int(5))), ("10", Some(Object::Int(10)))];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_boolean_expressions() {
        let tests = vec![
            ("true", Some(Object::Boolean(true))),
            ("false", Some(Object::Boolean(false))),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }
}
