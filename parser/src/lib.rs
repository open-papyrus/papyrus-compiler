#![feature(trait_alias)]

use crate::ast::script::Script;
use crate::parse::LexerSpan;
use crate::span::SourceId;
use chumsky::Parser;
use papyrus_compiler_lexer::syntax::token::Token;

pub mod ast;
pub mod error;
pub mod parse;
pub mod span;

pub fn parse_script(
    id: SourceId,
    tokens: Vec<(Token, LexerSpan)>,
) -> Result<Script, Vec<error::Error>> {
    let token_stream = parse::create_token_stream(id, tokens);
    ast::script::script_parser().parse(token_stream)
}
