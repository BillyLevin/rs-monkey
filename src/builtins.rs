use std::collections::HashMap;

use crate::object::Object;

pub fn initialize_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();

    builtins.insert(String::from("len"), Object::Builtin(len));
    builtins.insert(String::from("first"), Object::Builtin(first));
    builtins.insert(String::from("last"), Object::Builtin(last));
    builtins.insert(String::from("rest"), Object::Builtin(rest));
    builtins.insert(String::from("push"), Object::Builtin(push));
    builtins.insert(String::from("puts"), Object::Builtin(puts));

    builtins
}

fn len(arguments: Vec<Object>) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len(),
        ));
    }

    match &arguments[0] {
        Object::String(string) => Object::Int(string.len() as i64),
        Object::Array(elements) => Object::Int(elements.len() as i64),
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            &arguments[0]
        )),
    }
}

fn first(arguments: Vec<Object>) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len(),
        ));
    }

    match &arguments[0] {
        Object::Array(elements) => {
            if let Some(obj) = elements.first() {
                obj.clone()
            } else {
                Object::Null
            }
        }
        arg => Object::Error(format!("argument to `first` must be array, got {}", arg)),
    }
}

fn last(arguments: Vec<Object>) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len(),
        ));
    }

    match &arguments[0] {
        Object::Array(elements) => {
            if let Some(obj) = elements.last() {
                obj.clone()
            } else {
                Object::Null
            }
        }
        arg => Object::Error(format!("argument to `last` must be array, got {}", arg)),
    }
}

fn rest(arguments: Vec<Object>) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len(),
        ));
    }

    match &arguments[0] {
        Object::Array(elements) => {
            if !elements.is_empty() {
                Object::Array(elements[1..].into())
            } else {
                Object::Null
            }
        }
        arg => Object::Error(format!("argument to `rest` must be array, got {}", arg)),
    }
}

fn push(arguments: Vec<Object>) -> Object {
    if arguments.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            arguments.len(),
        ));
    }

    match &arguments[0] {
        Object::Array(elements) => {
            let mut new_elements = elements.clone();
            new_elements.push(arguments[1].clone());
            Object::Array(new_elements)
        }
        arg => Object::Error(format!("argument to `push` must be array, got {}", arg)),
    }
}

fn puts(arguments: Vec<Object>) -> Object {
    for arg in arguments {
        println!("{}", arg);
    }

    Object::Null
}
