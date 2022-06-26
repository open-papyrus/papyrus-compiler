use crate::parser::{Parse, Parser, ParserError, ParserResult};
use papyrus_compiler_lexer::syntax::token::Token;

pub type Identifier<'source> = &'source str;

impl<'source> Parse<'source> for Identifier<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let (token, range) = parser.consume()?;
        match token {
            Token::Identifier(value) => Ok(*value),
            _ => Err(ParserError::ExpectedToken {
                expected: Token::Identifier(""),
                found: (*token, range.clone()),
            }),
        }
    }
}
