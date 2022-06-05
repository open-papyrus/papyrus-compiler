use crate::syntax::token::Token;
use logos::Logos;

pub mod syntax;

pub fn run_lexer<'a>(input: &'a str) -> Vec<(Token<'a>, logos::Span)> {
    let lexer: logos::Lexer<'a, Token<'a>> = Token::lexer(input);
    let iter: logos::SpannedIter<'a, Token<'a>> = lexer.spanned();
    let tokens: Vec<_> = iter.collect();
    tokens
}
