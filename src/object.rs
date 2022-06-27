use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        environment: Rc<RefCell<Environment>>,
    },
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(num) => write!(f, "{}", num),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(object) => write!(f, "{}", object),
            Object::Error(message) => write!(f, "{}", message),
            Object::Function { parameters, .. } => {
                let parameters = parameters
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({}) {{}}", parameters)
            }
        }
    }
}
