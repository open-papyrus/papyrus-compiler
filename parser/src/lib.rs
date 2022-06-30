#![feature(macro_metavar_expr)]

#[cfg(test)]
#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

use crate::ast::script::Script;
use crate::parser::{Parse, Parser};
use crate::parser_diagnostics::ParserDiagnostic;
use crate::parser_error::*;
use papyrus_compiler_diagnostics::{SourceId, SourceRange};
use papyrus_compiler_lexer::syntax::token::Token;

pub mod ast;
pub(crate) mod parser;
mod parser_diagnostics;
pub mod parser_error;

pub(crate) fn filter_tokens(tokens: Vec<(Token, SourceRange)>) -> Vec<(Token, SourceRange)> {
    tokens
        .into_iter()
        .filter(|(token, _)| {
            !matches!(
                token,
                Token::SingleLineComment(_)
                    | Token::MultiLineComment(_)
                    | Token::DocumentationComment(_)
            )
        })
        .collect::<Vec<_>>()
}

pub fn parse_script(
    id: SourceId,
    tokens: Vec<(Token, SourceRange)>,
) -> Result<Script, Vec<ParserDiagnostic>> {
    let tokens = filter_tokens(tokens);
    let mut parser = Parser::new(tokens);
    let res = flatten_result(Script::parse(&mut parser));

    match res {
        Ok(res) => Ok(res),
        Err(parser_error) => match parser_error {
            ParserError::AggregatedErrors(errors) => Err(errors
                .into_iter()
                .map(|err| ParserDiagnostic::new(id, err))
                .collect::<Vec<_>>()),
            _ => Err(vec![ParserDiagnostic::new(id, parser_error)]),
        },
    }
}

#[cfg(test)]
#[cfg(feature = "test-external-scripts")]
mod tests {
    use crate::ast::script::Script;
    use crate::filter_tokens;
    use crate::parser::{Parse, Parser};
    use crate::parser_error::*;

    #[test]
    fn test_external_scripts() {
        let files = vec![
            "MrOctopus/nl_mcm/main/source/nl_mcm.psc",
            "MrOctopus/nl_mcm/main/source/nl_mcm_globalinfo.psc",
            "MrOctopus/nl_mcm/main/source/nl_mcm_module.psc",
            "MrOctopus/nl_mcm/main/source/nl_mcm_playerloadalias.psc",
            "MrOctopus/nl_mcm/main/scripts/source/nl_mcm.psc",
            "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_globalinfo.psc",
            "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_module.psc",
            "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_playerloadalias.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_ActiveEffectsWidget.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigBase.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigManager.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigMenu.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_Main.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_PlayerLoadGameAlias.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_QF_ConfigManagerInstance.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_QuestBase.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_SettingsManager.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetBase.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetManager.psc",
            "SSE/BYOHRelationshipAdoptionScript.psc",
        ];

        for script_path in files {
            let script_path = format!("../extern/{}", script_path);
            let path = std::path::Path::new(script_path.as_str());
            assert!(path.exists(), "{}", script_path);

            let script = std::fs::read_to_string(path).unwrap();
            let tokens = filter_tokens(papyrus_compiler_lexer::run_lexer(script.as_str()));
            let mut parser = Parser::new(tokens);

            let res = flatten_result(Script::parse(&mut parser));
            assert!(res.is_ok(), "{} {:#?}", script_path, res);
        }
    }

    #[cfg(feature = "dhat-heap")]
    #[test]
    fn heap_profiling() {
        let _profiler = dhat::Profiler::builder().testing().build();

        let script = include_str!("../../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc");
        let tokens = filter_tokens(papyrus_compiler_lexer::run_lexer(script));
        let mut parser = Parser::new(tokens);

        let _res = flatten_result(Script::parse(&mut parser));

        let stats = dhat::HeapStats::get();

        dhat::assert_eq!(6_536_373, stats.total_bytes);
        dhat::assert_eq!(26_537, stats.total_blocks);
    }
}
