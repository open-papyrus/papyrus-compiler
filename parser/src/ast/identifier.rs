use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::token::Token;

pub type Identifier<'a> = &'a str;

pub fn identifier_parser<'a>() -> impl TokenParser<'a, Identifier<'a>> {
    select! {
        Token::Identifier(value) => value
    }
}
