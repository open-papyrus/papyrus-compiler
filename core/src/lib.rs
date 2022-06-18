use papyrus_compiler_diagnostics::{convert_diagnostics, Diagnostic, SourceId};
use papyrus_compiler_parser::ast::script::Script;

pub fn compile_string<'a>(
    source_id: SourceId,
    src: &'a str,
) -> Result<Script, Vec<Box<dyn Diagnostic + 'a>>> {
    let tokens = papyrus_compiler_lexer::run_lexer_with_result(source_id, src)
        .map_err(convert_diagnostics)?;
    let script =
        papyrus_compiler_parser::parse_script(source_id, tokens).map_err(convert_diagnostics)?;
    Ok(script)
}
