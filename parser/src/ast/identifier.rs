use crate::ast::script::{CustomParser, ParserError};
use crate::parse::TokenParser;
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

macro_rules! select_tokens {
    ($token:ident, $( $pattern:pat_param => $out:expr ),+ $(,)? ;  $expected:expr) => {
        match $token {
            $( $pattern => ::core::result::Result::Ok($out) ),+,
            _ => ::core::result::Result::Err($crate::ast::script::ParserError::ExpectedOneOf {
                found: $token,
                expected: $expected
            }),
        }
    };
}

macro_rules! select_token {
    ($token:ident, $pattern:pat_param => $out:expr ; $expected:expr) => {
        match $token {
            $pattern => ::core::result::Result::Ok($out),
            _ => ::core::result::Result::Err($crate::ast::script::ParserError::ExpectedOne {
                found: $token,
                expected: $expected,
            }),
        }
    };
}

pub(crate) fn custom_identifier_parser(parser: CustomParser) -> Result<Identifier, ParserError> {
    let token = parser.consume()?;

    select_token!(token,
        Token::Keyword(KeywordKind::Default) => "default";
        Token::Keyword(KeywordKind::Default)
    )

    // select_tokens!(token,
    //     Token::Identifier(value) => value,
    //     Token::Keyword(KeywordKind::Default) => "default";
    //     vec![Token::Identifier(""), Token::Keyword(KeywordKind::Default)]
    // )

    // matches!()
    // match token {
    //     Token::Identifier(value) => Ok(value),
    //     // some keywords, mostly flags, are allowed to be parsed as an identifier
    //     Token::Keyword(KeywordKind::Default) => Ok("default"),
    //     _ => Err(ParserError::ExpectedOneOf {
    //         found: token,
    //         expected: vec![Token::Identifier(""), Token::Keyword(KeywordKind::Default)]
    //     })
    // }
}
