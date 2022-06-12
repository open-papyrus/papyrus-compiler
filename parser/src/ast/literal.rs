use std::fmt::{Display, Formatter};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(&'a str),
    None,
}

pub fn literal_parser<'a>() -> impl TokenParser<'a, Literal<'a>> {
    select! {
        Token::BooleanLiteral(value) => Literal::Boolean(value),
        Token::IntegerLiteral(value) => Literal::Integer(value),
        Token::FloatLiteral(value) => Literal::Float(value),
        Token::StringLiteral(value) => Literal::String(value),
        Token::NoneLiteral => Literal::None,
    }
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
