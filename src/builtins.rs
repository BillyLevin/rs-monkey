use std::collections::HashMap;

use crate::object::Object;

pub fn initialize_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();

    builtins.insert(String::from("len"), Object::Builtin(len));

    builtins
}

fn len(arguments: Vec<Object>) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len(),
        ));
    }

    if let Object::String(string) = &arguments[0] {
        Object::Int(string.len() as i64)
    } else {
        Object::Error(format!(
            "argument to `len` not supported, got {}",
            &arguments[0]
        ))
    }
}
