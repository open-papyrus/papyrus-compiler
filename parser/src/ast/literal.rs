use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(&'a str),
    None,
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Boolean(value) => write!(f, "{}", value),
            Literal::Integer(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
            Literal::None => write!(f, "none"),
        }
    }
}
