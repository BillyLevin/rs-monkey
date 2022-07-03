use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

#[derive(Debug, Clone)]
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
    String(String),
    Builtin(BuiltInFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
}

pub type BuiltInFunction = fn(Vec<Object>) -> Object;

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
            Object::String(string) => write!(f, "{}", string),
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "[{}]", elements)
            }
            Object::Hash(hash) => {
                let pairs = hash
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "{{ {} }}", pairs)
            }
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Int(num), Object::Int(other_num)) => num == other_num,
            (Object::Boolean(boolean), Object::Boolean(other_boolean)) => boolean == other_boolean,
            (Object::Null, Object::Null) => true,
            (Object::ReturnValue(object), Object::ReturnValue(other_object)) => {
                object == other_object
            }
            (Object::Error(error), Object::Error(other_error)) => error == other_error,
            (
                Object::Function {
                    parameters,
                    body,
                    environment,
                },
                Object::Function {
                    parameters: other_parameters,
                    body: other_body,
                    environment: other_environment,
                },
            ) => {
                parameters == other_parameters
                    && body == other_body
                    && environment == other_environment
            }
            (Object::String(string), Object::String(other_string)) => string == other_string,
            (Object::Builtin(function), Object::Builtin(other_function)) => {
                function == other_function
            }
            (Object::Array(array), Object::Array(other_array)) => array == other_array,
            (Object::Hash(hash), Object::Hash(other_hash)) => hash == other_hash,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // we only allow strings, integers, and booleans as hash keys
        match self {
            Object::Boolean(boolean) => boolean.hash(state),
            Object::Int(num) => num.hash(state),
            Object::String(string) => string.hash(state),
            _ => panic!("invalid hash key"),
        }
    }
}
