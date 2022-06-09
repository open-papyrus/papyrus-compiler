#![feature(trait_alias)]

use crate::ast::script::Script;
use crate::span::Span;
use chumsky::Parser;
use papyrus_compiler_lexer::syntax::token::Token;

pub mod ast;
pub mod error;
pub mod parse;
pub mod span;

pub fn parse_script(tokens: Vec<(Token, Span)>) -> Result<Script, Vec<error::Error>> {
    let token_stream = parse::create_token_stream(tokens);
    parse::script_parser().parse(token_stream)
}
