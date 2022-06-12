use crate::{error::*, span::*};
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::token::Token;
use std::ops::Range;
use std::vec::IntoIter;

pub trait TokenParser<'a, O> = Parser<Token<'a>, O, Error = Error<'a>> + Clone;
pub type TokenStream<'a> = chumsky::Stream<'a, Token<'a>, Span, IntoIter<(Token<'a>, Span)>>;

pub(crate) type LexerSpan = Range<usize>;

pub fn create_token_stream(id: SourceId, tokens: Vec<(Token, LexerSpan)>) -> TokenStream {
    let tokens = tokens
        .into_iter()
        .filter(|(token, _)| {
            !matches!(
                token,
                Token::SingleLineComment(_)
                    | Token::MultiLineComment(_)
                    | Token::DocumentationComment(_)
            )
        })
        .map(|(token, lexer_span)| (token, Span::new(id.clone(), lexer_span)))
        .collect::<Vec<_>>();

    let eoi = match tokens.last() {
        Some((_, span)) => span.clone(),
        None => Span::new(id.clone(), 0..1),
    };

    chumsky::Stream::from_iter(eoi, tokens.into_iter())
}

pub fn run_lexer_and_get_stream(id: SourceId, input: &str) -> TokenStream {
    let tokens = papyrus_compiler_lexer::run_lexer(input);
    create_token_stream(id, tokens)
}

#[cfg(test)]
pub mod test_utils {
    use crate::parse::{run_lexer_and_get_stream, TokenParser};
    use chumsky::prelude::end;
    use std::fmt::Debug;

    pub fn run_test<'a, F, P, O>(src: &'a str, expected: O, parser_fn: F)
    where
        F: Fn() -> P,
        P: TokenParser<'a, O>,
        O: PartialEq + Debug,
    {
        let token_stream = run_lexer_and_get_stream("repl".to_string(), src);
        let res = parser_fn().then_ignore(end()).parse(token_stream).unwrap();
        assert_eq!(res, expected);
    }

    pub fn run_tests<'a, F, P, O>(data: Vec<(&'a str, O)>, parser_fn: F)
    where
        F: Fn() -> P,
        P: TokenParser<'a, O>,
        O: PartialEq + Debug,
    {
        for (src, expected) in data {
            run_test(src, expected, &parser_fn);
        }
    }
}
