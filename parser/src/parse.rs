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
        .map(|(token, lexer_span)| (token, Span::new(id, lexer_span)))
        .collect::<Vec<_>>();

    let eoi = match tokens.last() {
        Some((_, span)) => span.clone(),
        None => Span::new(id, 0..1),
    };

    chumsky::Stream::from_iter(eoi, tokens.into_iter())
}

pub fn run_lexer_and_get_stream(id: SourceId, input: &str) -> TokenStream {
    let tokens = papyrus_compiler_lexer::run_lexer(input);
    create_token_stream(id, tokens)
}

#[cfg(test)]
pub mod test_utils {
    use crate::ast::script::script_parser;
    use crate::parse::{run_lexer_and_get_stream, TokenParser};
    use chumsky::prelude::*;
    use std::assert_matches::assert_matches;
    use std::fmt::Debug;

    pub fn run_test<'a, F, P, O>(src: &'a str, expected: O, parser_fn: F)
    where
        F: Fn() -> P,
        P: TokenParser<'a, O>,
        O: PartialEq + Debug,
    {
        let token_stream = run_lexer_and_get_stream(u32::MAX, src);
        let res = parser_fn().then_ignore(end()).parse(token_stream).unwrap();
        assert_eq!(res, expected, "{}", src);
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

    #[test]
    #[cfg(feature = "test-external-scripts")]
    fn test_scripts() {
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
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_PLayerLoadGameAlias.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_QF_ConfigManagerInstance.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_QuestBase.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_SettingsManager.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetBase.psc",
            "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetManager.psc",
        ];

        for script_path in files {
            let script_path = format!("../extern/{}", script_path);
            let path = std::path::Path::new(script_path.as_str());
            assert!(path.exists());

            let script = std::fs::read_to_string(path).unwrap();

            let token_stream = run_lexer_and_get_stream(u32::MAX, script.as_str());
            let res = script_parser().then_ignore(end()).parse(token_stream);
            assert_matches!(res, Ok(_), "Script with errors: \"{}\"", script_path);
        }
    }
}
