#![feature(trait_alias)]
#![feature(macro_metavar_expr)]

use crate::ast::script::Script;
use crate::parser::{Parse, Parser};
use papyrus_compiler_diagnostics::{SourceId, SourceRange};
use papyrus_compiler_lexer::syntax::token::Token;

pub mod ast;
pub(crate) mod parser;

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

pub fn parse_script(_id: SourceId, tokens: Vec<(Token, SourceRange)>) -> Option<Script> {
    let mut parser = Parser::new(tokens);
    let res = Script::parse(&mut parser);
    res.ok()
}

#[cfg(test)]
#[cfg(feature = "test-external-scripts")]
mod tests {
    use crate::ast::script::Script;
    use crate::filter_tokens;
    use crate::parser::{flatten_result, Parse, Parser};

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
}
