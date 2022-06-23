use crate::parse::TokenParser;
use crate::parser::{CustomParser, ParserError, ParserResult};
use crate::select_tokens;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;

pub type Identifier<'a> = &'a str;

pub fn identifier_parser<'a>() -> impl TokenParser<'a, Identifier<'a>> {
    select! {
        Token::Identifier(value) => value,

        // some keywords, mostly flags, are allowed to be parsed as an identifier
        Token::Keyword(KeywordKind::Default) => "default"
    }
}

pub(crate) fn custom_identifier_parser<'source>(
    parser: &mut CustomParser<'source>,
) -> ParserResult<'source, Identifier<'source>> {
    select_tokens!(parser,
        Token::Identifier(value) => value.clone(),
        
        // some keywords, mostly flags, are allowed to be parsed as an identifier
        Token::Keyword(KeywordKind::Default) => "default";
        
        vec![Token::Identifier(""), Token::Keyword(KeywordKind::Default)]
    )
}
