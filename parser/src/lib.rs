#![feature(trait_alias)]

extern crate core;

pub mod ast;
pub mod error;
pub mod parser;
pub mod parser_diagnostics;

// pub fn parse_script(
//     id: SourceId,
//     tokens: Vec<(Token, SourceRange)>,
// ) -> Result<Script, Vec<ParserDiagnostics>> {
//     let token_stream = parse::create_token_stream(tokens);
//     ast::script::script_parser()
//         .parse(token_stream)
//         .map_err(|errors| {
//             errors
//                 .into_iter()
//                 .map(|error| error.to_diagnostics(id))
//                 .collect()
//         })
// }

#[cfg(test)]
#[cfg(feature = "test-external-scripts")]
mod tests {
    use crate::ast::script::Script;
    use crate::parser::{Parse, Parser};

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
            let tokens = papyrus_compiler_lexer::run_lexer(script.as_str());
            let mut parser = Parser::new(tokens);

            let res = Script::parse(&mut parser);
            assert!(res.is_ok(), "{} {:?}", script_path, res);
            let res = res.unwrap();

            assert!(parser.expect_eoi().is_ok(), "{} {:?}", script_path, res);
        }
    }
}
