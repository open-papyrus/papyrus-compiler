use crate::parser::{Parse, Parser, ParserResult};
use crate::select_tokens;
use papyrus_compiler_lexer::syntax::token::Token;

pub type Identifier<'source> = &'source str;

impl<'source> Parse<'source> for Identifier<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        select_tokens!(parser, "Identifier",
            Token::Identifier(value) => *value
        )
    }
}
