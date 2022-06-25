use crate::parser::{Parse, Parser, ParserResult};
use crate::select_tokens;
use papyrus_compiler_lexer::syntax::token::Token;

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
