use crate::{
    ast::{Expression, Infix, Literal, Prefix, Program, Statement},
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
            Expression::Infix(left, infix, right) => {
                self.eval_infix_expression(*left, infix, *right)
            }
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

    fn eval_infix_expression(
        &mut self,
        left: Expression,
        infix: Infix,
        right: Expression,
    ) -> Option<Object> {
        let left = self.eval_expression(left)?;
        let right = self.eval_expression(right)?;

        match left {
            Object::Int(left_val) => {
                if let Object::Int(right_val) = right {
                    Some(self.eval_integer_infix_expression(left_val, infix, right_val))
                } else {
                    None
                }
            }
            _ => todo!(),
        }
    }

    fn eval_integer_infix_expression(&mut self, left: i64, infix: Infix, right: i64) -> Object {
        match infix {
            Infix::Plus => Object::Int(left + right),
            Infix::Minus => Object::Int(left - right),
            Infix::Multiply => Object::Int(left * right),
            Infix::Divide => Object::Int(left / right),
            Infix::LessThan => Object::Boolean(left < right),
            Infix::GreaterThan => Object::Boolean(left > right),
            Infix::Equal => Object::Boolean(left == right),
            Infix::NotEqual => Object::Boolean(left != right),
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
            ("5 + 5 + 5 + 5 - 10", Some(Object::Int(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::Int(32))),
            ("-50 + 100 + -50", Some(Object::Int(0))),
            ("5 * 2 + 10", Some(Object::Int(20))),
            ("5 + 2 * 10", Some(Object::Int(25))),
            ("20 + 2 * -10", Some(Object::Int(0))),
            ("50 / 2 * 2 + 10", Some(Object::Int(60))),
            ("2 * (5 + 10)", Some(Object::Int(30))),
            ("3 * 3 * 3 + 10", Some(Object::Int(37))),
            ("3 * (3 * 3) + 10", Some(Object::Int(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::Int(50))),
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
            ("1 < 2", Some(Object::Boolean(true))),
            ("1 > 2", Some(Object::Boolean(false))),
            ("1 < 1", Some(Object::Boolean(false))),
            ("1 > 1", Some(Object::Boolean(false))),
            ("1 == 1", Some(Object::Boolean(true))),
            ("1 != 1", Some(Object::Boolean(false))),
            ("1 == 2", Some(Object::Boolean(false))),
            ("1 != 2", Some(Object::Boolean(true))),
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
