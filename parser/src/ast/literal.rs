use crate::parser::{Parse, Parser, ParserResult};
use crate::select_tokens;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'source> {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(&'source str),
    None,
}

impl<'source> Parse<'source> for Literal<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        select_tokens!(parser, "Literal",
            Token::BooleanLiteral(value) => Literal::Boolean(*value),
            Token::IntegerLiteral(value) => Literal::Integer(*value),
            Token::FloatLiteral(value) => Literal::Float(*value),
            Token::StringLiteral(value) => Literal::String(*value),
            Token::NoneLiteral => Literal::None
        )
    }
}

impl<'source> Display for Literal<'source> {
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
