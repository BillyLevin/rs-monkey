use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Identifier, Infix, Literal, Prefix, Program, Statement},
    environment::Environment,
    object::Object,
};

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new_with_builtins())),
        }
    }

    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(*value),
                Some(Object::Error(message)) => return Some(Object::Error(message)),
                evaluated => result = evaluated,
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(expression) => self.eval_return(expression),
            Statement::Let(identifier, expression) => {
                let value = self.eval_expression(expression)?;

                if Self::is_error(&value) {
                    Some(value)
                } else {
                    let Identifier(name) = identifier;
                    self.environment.borrow_mut().set(name, value);
                    None
                }
            }
        }
    }

    fn eval_block_statement(&mut self, statements: BlockStatement) -> Option<Object> {
        let mut result = None;

        for statement in statements {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                Some(Object::Error(message)) => return Some(Object::Error(message)),
                evaluated => result = evaluated,
            }
        }

        result
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            Expression::Prefix(prefix, right) => self.eval_prefix_expression(prefix, *right),
            Expression::Infix(left, infix, right) => {
                self.eval_infix_expression(*left, infix, *right)
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_else_expression(*condition, consequence, alternative),
            Expression::Identifier(identifier) => self.eval_identifier_expression(identifier),
            Expression::Function { parameters, body } => Some(Object::Function {
                parameters,
                body,
                environment: Rc::clone(&self.environment),
            }),
            Expression::Call {
                function,
                arguments,
            } => Some(self.eval_call_expression(*function, arguments)),
            Expression::Index { left, index } => self.eval_index_expression(*left, *index),
        }
    }

    fn eval_return(&mut self, expression: Expression) -> Option<Object> {
        let evaluated = self.eval_expression(expression)?;

        if Self::is_error(&evaluated) {
            Some(evaluated)
        } else {
            Some(Object::ReturnValue(Box::new(evaluated)))
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Int(num) => Object::Int(num),
            Literal::Boolean(boolean) => Object::Boolean(boolean),
            Literal::String(string) => Object::String(string),
            Literal::Array(elements) => self.eval_array_literal_expression(elements),
            Literal::Hash(pairs) => self.eval_hash_literal_expression(pairs),
        }
    }

    fn eval_array_literal_expression(&mut self, elements: Vec<Expression>) -> Object {
        let elements = self.eval_expressions(elements);

        if elements.len() == 1 && Self::is_error(elements.get(0).unwrap()) {
            return elements.get(0).unwrap().clone();
        }

        Object::Array(elements)
    }

    fn eval_hash_literal_expression(&mut self, pairs: Vec<(Expression, Expression)>) -> Object {
        let mut hash = HashMap::new();

        for (key, value) in pairs {
            let key = self.eval_expression(key).unwrap_or(Object::Null);

            if Self::is_error(&key) {
                return key;
            }

            let value = self.eval_expression(value).unwrap_or(Object::Null);

            if Self::is_error(&value) {
                return value;
            }

            let is_valid_key =
                matches!(key, Object::String(_) | Object::Int(_) | Object::Boolean(_));

            if !is_valid_key {
                return Object::Error(format!("invalid hash key: {}", key));
            }

            hash.insert(key, value);
        }

        Object::Hash(hash)
    }

    fn eval_prefix_expression(&mut self, prefix: Prefix, right: Expression) -> Option<Object> {
        let right = self.eval_expression(right)?;

        if Self::is_error(&right) {
            return Some(right);
        }

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

        if Self::is_error(&left) {
            return Some(left);
        }

        let right = self.eval_expression(right)?;

        if Self::is_error(&right) {
            return Some(right);
        }

        match left {
            Object::Int(left_val) => {
                if let Object::Int(right_val) = right {
                    Some(self.eval_integer_infix_expression(left_val, infix, right_val))
                } else {
                    Some(Self::new_error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right,
                    )))
                }
            }
            Object::Boolean(left_val) => {
                if let Object::Boolean(right_val) = right {
                    Some(self.eval_boolean_infix_expression(left_val, infix, right_val))
                } else {
                    Some(Self::new_error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right,
                    )))
                }
            }
            Object::String(ref left_val) => {
                if let Object::String(right_val) = right {
                    Some(self.eval_string_infix_expression(left_val.clone(), infix, right_val))
                } else {
                    Some(Self::new_error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right,
                    )))
                }
            }
            _ => Some(Self::new_error(format!(
                "unknown operator: {} {} {}",
                left, infix, right
            ))),
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

    fn eval_boolean_infix_expression(&mut self, left: bool, infix: Infix, right: bool) -> Object {
        match infix {
            Infix::Equal => Object::Boolean(left == right),
            Infix::NotEqual => Object::Boolean(left != right),
            _ => Self::new_error(format!("unknown operator: {} {} {}", left, infix, right)),
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        left: String,
        infix: Infix,
        right: String,
    ) -> Object {
        match infix {
            Infix::Plus => Object::String(format!("{}{}", left, right)),
            _ => Self::new_error(format!("unknown operator: {} {} {}", left, infix, right)),
        }
    }

    fn eval_if_else_expression(
        &mut self,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Option<Object> {
        let condition = self.eval_expression(condition)?;

        if Self::is_error(&condition) {
            return Some(condition);
        }

        if Self::is_truthy(condition) {
            self.eval_block_statement(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_block_statement(alternative)
        } else {
            Some(Object::Null)
        }
    }

    fn eval_identifier_expression(&mut self, identifier: Identifier) -> Option<Object> {
        let Identifier(name) = identifier;

        let value = self.environment.borrow_mut().get(name.clone());

        if let Some(value) = value {
            Some(value)
        } else {
            Some(Self::new_error(format!("identifier not found: {}", name)))
        }
    }

    fn eval_call_expression(&mut self, function: Expression, arguments: Vec<Expression>) -> Object {
        let evaluated_arguments = self.eval_expressions(arguments);

        if evaluated_arguments.len() == 1 && Self::is_error(evaluated_arguments.get(0).unwrap()) {
            return evaluated_arguments.get(0).unwrap().clone();
        }

        let (parameters, body, function_environment) = match self.eval_expression(function) {
            Some(Object::Function {
                parameters,
                body,
                environment,
            }) => (parameters, body, environment),
            Some(Object::Builtin(function)) => return function(evaluated_arguments),
            Some(obj) => return Self::new_error(format!("not a function: {}", obj)),
            _ => return Object::Null,
        };

        let current_env = self.environment.clone();
        let mut enclosed_env = Environment::new_with_outer(function_environment);

        for (identifier, value) in parameters.iter().zip(evaluated_arguments.iter()) {
            let Identifier(name) = identifier.clone();

            enclosed_env.set(name, value.clone());
        }

        self.environment = Rc::new(RefCell::new(enclosed_env));

        let evaluated_body = self.eval_block_statement(body);

        self.environment = current_env;

        match evaluated_body {
            Some(obj) => obj,
            None => Object::Null,
        }
    }

    fn eval_index_expression(&mut self, left: Expression, index: Expression) -> Option<Object> {
        let left = self.eval_expression(left)?;

        if Self::is_error(&left) {
            return Some(left);
        }

        let index = self.eval_expression(index)?;

        if Self::is_error(&index) {
            return Some(index);
        }

        match left {
            Object::Array(elements) => {
                if let Object::Int(idx) = index {
                    let max = elements.len() as i64;

                    if idx < 0 || idx > max {
                        return Some(Object::Null);
                    }

                    if let Some(obj) = elements.get(idx as usize) {
                        Some(obj.clone())
                    } else {
                        Some(Object::Null)
                    }
                } else {
                    return Some(Object::Error(format!("invalid index: {}", index)));
                }
            }
            _ => Some(Object::Error(format!(
                "index operator not supported: {}",
                left
            ))),
        }
    }

    fn eval_expressions(&mut self, expressions: Vec<Expression>) -> Vec<Object> {
        let mut result = Vec::new();

        for expression in expressions {
            let evaluated = self.eval_expression(expression).unwrap_or(Object::Null);

            if Self::is_error(&evaluated) {
                return vec![evaluated];
            }

            result.push(evaluated);
        }

        result
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
            _ => Self::new_error(format!("unknown operator: -{}", right)),
        }
    }

    fn is_truthy(object: Object) -> bool {
        !matches!(object, Object::Boolean(false) | Object::Null)
    }

    fn new_error(message: String) -> Object {
        Object::Error(message)
    }

    fn is_error(object: &Object) -> bool {
        matches!(object, Object::Error(_))
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
            ("true == true", Some(Object::Boolean(true))),
            ("false == false", Some(Object::Boolean(true))),
            ("true == false", Some(Object::Boolean(false))),
            ("true != false", Some(Object::Boolean(true))),
            ("false != true", Some(Object::Boolean(true))),
            ("(1 < 2) == true", Some(Object::Boolean(true))),
            ("(1 < 2) == false", Some(Object::Boolean(false))),
            ("(1 > 2) == true", Some(Object::Boolean(false))),
            ("(1 > 2) == false", Some(Object::Boolean(true))),
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

    #[test]
    fn eval_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Some(Object::Int(10))),
            ("if (false) { 10 }", Some(Object::Null)),
            ("if (1) { 10 }", Some(Object::Int(10))),
            ("if (1 < 2) { 10 }", Some(Object::Int(10))),
            ("if (1 > 2) { 10 }", Some(Object::Null)),
            ("if (1 > 2) { 10 } else { 20 }", Some(Object::Int(20))),
            ("if (1 < 2) { 10 } else { 20 }", Some(Object::Int(10))),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_return_statements() {
        let tests = vec![
            ("return 10;", Some(Object::Int(10))),
            ("return 10; 9;", Some(Object::Int(10))),
            ("return 2 * 5; 9;", Some(Object::Int(10))),
            ("9; return 2 * 5; 9;", Some(Object::Int(10))),
            (
                "
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            ",
                Some(Object::Int(10)),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn error_handling() {
        let tests = vec![
            (
                "5 + true",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "5 + true; 5;",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "-true",
                Some(Object::Error(String::from("unknown operator: -true"))),
            ),
            (
                "5; true + false; 5;",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "if (10 > 1) { true + false; }",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "foobar;",
                Some(Object::Error(String::from("identifier not found: foobar"))),
            ),
            (
                "\"Hello\" - \"World\"",
                Some(Object::Error(String::from(
                    "unknown operator: Hello - World",
                ))),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Some(Object::Int(5))),
            ("let a = 5 * 5; a;", Some(Object::Int(25))),
            ("let a = 5; let b = a; b;", Some(Object::Int(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Some(Object::Int(15)),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_function_definitions() {
        assert_eq!(
            eval("fn(x) { x + 2; }"),
            Some(Object::Function {
                parameters: vec![Identifier(String::from("x"))],
                body: vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(2)))
                ))],
                environment: Rc::new(RefCell::new(Environment::new_with_builtins()))
            })
        )
    }

    #[test]
    fn eval_function_calls() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Some(Object::Int(20)),
            ),
            ("fn(x) { x; }(5)", Some(Object::Int(5))),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_closures() {
        let input = "
    let newAdder = fn(x) {
        fn(y) { x + y };
    };

    let addTwo = newAdder(2);
    addTwo(2);
";

        assert_eq!(eval(input), Some(Object::Int(4)));
    }

    #[test]
    fn eval_string_literal_expressions() {
        let input = "\"Hello World!\"";

        assert_eq!(
            eval(input),
            Some(Object::String(String::from("Hello World!")))
        );
    }

    #[test]
    fn eval_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        assert_eq!(
            eval(input),
            Some(Object::String(String::from("Hello World!")))
        );
    }

    #[test]
    fn eval_builtin_functions() {
        let tests = vec![
            ("len(\"\")", Some(Object::Int(0))),
            ("len(\"four\")", Some(Object::Int(4))),
            ("len(\"hello world\")", Some(Object::Int(11))),
            (
                "len(1)",
                Some(Object::Error(String::from(
                    "argument to `len` not supported, got 1",
                ))),
            ),
            (
                "len(\"one\", \"two\")",
                Some(Object::Error(String::from(
                    "wrong number of arguments. got=2, want=1",
                ))),
            ),
            ("len([1, 2, 3])", Some(Object::Int(3))),
            ("first([1, 2, 3])", Some(Object::Int(1))),
            ("first([])", Some(Object::Null)),
            (
                "first([], [])",
                Some(Object::Error(String::from(
                    "wrong number of arguments. got=2, want=1",
                ))),
            ),
            (
                "first(\"hello\")",
                Some(Object::Error(String::from(
                    "argument to `first` must be array, got hello",
                ))),
            ),
            (
                "first(1)",
                Some(Object::Error(String::from(
                    "argument to `first` must be array, got 1",
                ))),
            ),
            ("last([1, 2, 3])", Some(Object::Int(3))),
            ("last([])", Some(Object::Null)),
            (
                "last([], [])",
                Some(Object::Error(String::from(
                    "wrong number of arguments. got=2, want=1",
                ))),
            ),
            (
                "last(\"string\")",
                Some(Object::Error(String::from(
                    "argument to `last` must be array, got string",
                ))),
            ),
            (
                "last(1)",
                Some(Object::Error(String::from(
                    "argument to `last` must be array, got 1",
                ))),
            ),
            (
                "rest([1, 2, 3, 4])",
                Some(Object::Array(vec![
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            (
                "rest([2, 3, 4])",
                Some(Object::Array(vec![Object::Int(3), Object::Int(4)])),
            ),
            ("rest([4])", Some(Object::Array(vec![]))),
            ("rest([])", Some(Object::Null)),
            (
                "rest([], [])",
                Some(Object::Error(String::from(
                    "wrong number of arguments. got=2, want=1",
                ))),
            ),
            (
                "rest(\"string\")",
                Some(Object::Error(String::from(
                    "argument to `rest` must be array, got string",
                ))),
            ),
            (
                "rest(1)",
                Some(Object::Error(String::from(
                    "argument to `rest` must be array, got 1",
                ))),
            ),
            (
                "push([1, 2, 3], 4)",
                Some(Object::Array(vec![
                    Object::Int(1),
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            ("push([], 1)", Some(Object::Array(vec![Object::Int(1)]))),
            (
                "let a = [1]; push(a, 2); a",
                Some(Object::Array(vec![Object::Int(1)])),
            ),
            (
                "push([], [], [])",
                Some(Object::Error(String::from(
                    "wrong number of arguments. got=3, want=2",
                ))),
            ),
            (
                "push(\"string\", 1)",
                Some(Object::Error(String::from(
                    "argument to `push` must be array, got string",
                ))),
            ),
            (
                "push(1, 1)",
                Some(Object::Error(String::from(
                    "argument to `push` must be array, got 1",
                ))),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        assert_eq!(
            eval(input),
            Some(Object::Array(vec![
                Object::Int(1),
                Object::Int(4),
                Object::Int(6)
            ]))
        )
    }

    #[test]
    fn eval_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Some(Object::Int(1))),
            ("[1, 2, 3][1]", Some(Object::Int(2))),
            ("[1, 2, 3][2]", Some(Object::Int(3))),
            ("let i = 0; [1][i];", Some(Object::Int(1))),
            ("[1, 2, 3][1 + 1];", Some(Object::Int(3))),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(Object::Int(3))),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(Object::Int(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(Object::Int(2)),
            ),
            ("[1, 2, 3][3]", Some(Object::Null)),
            ("[1, 2, 3][-1]", Some(Object::Null)),
        ];

        for (input, expected) in tests {
            assert_eq!(eval(input), expected);
        }
    }

    #[test]
    fn eval_hash_literals() {
        let input = "let two = \"two\";
        {
            \"one\": 10 - 9,
            two: 1 + 1,
            \"thr\" + \"ee\": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }";

        assert_eq!(
            eval(input),
            Some(Object::Hash(
                vec![
                    (Object::String(String::from("one")), Object::Int(1),),
                    (Object::String(String::from("two")), Object::Int(2),),
                    (Object::String(String::from("three")), Object::Int(3),),
                    (Object::Int(4), Object::Int(4)),
                    (Object::Boolean(true), Object::Int(5)),
                    (Object::Boolean(false), Object::Int(6)),
                ]
                .into_iter()
                .collect()
            ))
        );
    }
}
