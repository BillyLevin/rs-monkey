use crate::{
    ast::{Expression, Literal, Prefix, Program, Statement},
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
            Expression::Prefix(prefix, right) => self.eval_prefix_expression(prefix, *right),

            _ => None,
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Int(num) => Object::Int(num),
            Literal::Boolean(boolean) => Object::Boolean(boolean),
        }
    }

    fn eval_prefix_expression(&mut self, prefix: Prefix, right: Expression) -> Option<Object> {
        let right = self.eval_expression(right)?;

        match prefix {
            Prefix::Not => Some(self.eval_not_operator_expression(right)),
            Prefix::Minus => Some(self.eval_minus_operator_expression(right)),
        }
    }

    fn eval_not_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => Object::Boolean(!boolean),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }

    fn eval_minus_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Int(num) => Object::Int(-num),
            _ => Object::Null,
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
        let tests = vec![
            ("5", Some(Object::Int(5))),
            ("10", Some(Object::Int(10))),
            ("-5", Some(Object::Int(-5))),
            ("-10", Some(Object::Int(-10))),
        ];

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

    #[test]
    fn eval_not_operator() {
        let tests = vec![
            ("!true", Some(Object::Boolean(false))),
            ("!false", Some(Object::Boolean(true))),
            ("!5", Some(Object::Boolean(false))),
            ("!!true", Some(Object::Boolean(true))),
            ("!!false", Some(Object::Boolean(false))),
            ("!!5", Some(Object::Boolean(true))),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }
}
