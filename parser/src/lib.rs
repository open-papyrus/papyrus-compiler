#![feature(trait_alias)]
#![feature(assert_matches)]

extern crate core;

use crate::ast::script::Script;
use crate::parser_diagnostics::ParserDiagnostics;
use chumsky::Parser;
use papyrus_compiler_diagnostics::{SourceId, SourceRange};
use papyrus_compiler_lexer::syntax::token::Token;

pub mod ast;
pub mod error;
pub mod parse;
pub mod parser;
pub mod parser_diagnostics;

pub fn parse_script(
    id: SourceId,
    tokens: Vec<(Token, SourceRange)>,
) -> Result<Script, Vec<ParserDiagnostics>> {
    let token_stream = parse::create_token_stream(tokens);
    ast::script::script_parser()
        .parse(token_stream)
        .map_err(|errors| {
            errors
                .into_iter()
                .map(|error| error.to_diagnostics(id))
                .collect()
        })
}
