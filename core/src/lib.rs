use crate::{error::*, source::*};
use papyrus_compiler_lexer::syntax::token::Token;
use papyrus_compiler_parser::ast::script::Script;

pub mod cache;
pub mod error;
pub mod source;

pub fn compile_string(id: SourceId, src: &str) -> Result<Script, Vec<CustomReport>> {
    let lexer_res = run_lexer(id.clone(), src)?;
    let parser_res = run_parser(id, lexer_res)?;
    Ok(parser_res)
}

pub fn run_lexer(id: SourceId, src: &str) -> Result<Vec<(Token, LexerSpan)>, Vec<CustomReport>> {
    let tokens = papyrus_compiler_lexer::run_lexer(src);
    let errors = tokens
        .iter()
        .filter_map(|(token, span)| {
            if !matches!(token, Token::Error) {
                None
            } else {
                Some(lexer_error_to_report(id.clone(), span.clone()))
            }
        })
        .collect::<Vec<_>>();

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

pub fn run_parser(
    id: SourceId,
    tokens: Vec<(Token, LexerSpan)>,
) -> Result<Script, Vec<CustomReport>> {
    let parser_result = papyrus_compiler_parser::parse_script(id, tokens);
    parser_result.map_err(|errors| {
        errors
            .into_iter()
            .map(parser_error_to_report)
            .collect::<Vec<_>>()
    })
}
