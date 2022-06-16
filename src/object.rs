#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(num) => write!(f, "{}", num),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(object) => write!(f, "{}", object),
        }
    }
}
