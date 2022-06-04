use crate::ast::{Expr, ScriptFlag};
use chumsky::error::Cheap;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use papyrus_compiler_lexer::SpannedToken;
pub mod ast;

pub type SpannedExpr<'a> = (Expr<'a>, core::ops::Range<usize>);

pub type ParserError<'a> = Cheap<SpannedToken<'a>>;

pub fn parser<'a>(
) -> impl Parser<SpannedToken<'a>, SpannedExpr<'a>, Error = ParserError<'a>> + Clone {
    let identifier = filter(|input: &SpannedToken<'a>| {
        let (token, _) = input;
        matches!(token, Token::Identifier(_))
    })
    .map(|input| {
        let (token, span) = input;
        let identifier = match token {
            Token::Identifier(identifier) => identifier,
            _ => panic!(),
        };

        (identifier, span)
    });

    let header = filter(|input: &SpannedToken<'a>| {
        let (token, _) = input;
        matches!(token, Token::Keyword(KeywordKind::ScriptName))
    })
    .map(|input| {
        // don't care about the keyword, only want the span
        let (_, span) = input;
        span
    })
    .then(identifier)
    .then(
        // 'extends <identifier>' is optional
        filter(|(token, _)| matches!(token, Token::Keyword(KeywordKind::Extends)))
            .ignore_then(identifier)
            .or_not(),
    )
    .then(
        filter(|input: &SpannedToken| {
            let (token, _) = input;
            match token {
                Token::Keyword(keyword) => matches!(
                    keyword,
                    KeywordKind::Conditional
                        | KeywordKind::Const
                        | KeywordKind::DebugOnly
                        | KeywordKind::BetaOnly
                        | KeywordKind::Hidden
                        | KeywordKind::Native
                        | KeywordKind::Default
                ),
                _ => false,
            }
        })
        .repeated()
        .at_least(1)
        .map(|tokens| {
            let last_token = tokens.last().unwrap();
            let (_, last_span) = last_token;

            let script_flags = tokens
                .iter()
                .map(|input| {
                    let (token, _) = input;
                    match token {
                        Token::Keyword(keyword) => match keyword {
                            KeywordKind::Conditional => ScriptFlag::Conditional,
                            KeywordKind::Const => ScriptFlag::Const,
                            KeywordKind::DebugOnly => ScriptFlag::DebugOnly,
                            KeywordKind::BetaOnly => ScriptFlag::BetaOnly,
                            KeywordKind::Hidden => ScriptFlag::Hidden,
                            KeywordKind::Native => ScriptFlag::Native,
                            KeywordKind::Default => ScriptFlag::Default,
                            _ => panic!("Unknown keyword: {:?}", keyword),
                        },
                        _ => panic!("Token is not a keyword!"),
                    }
                })
                .collect::<Vec<_>>();

            (script_flags, last_span.clone())
        })
        .or_not(),
    )
    .map(
        |(
            (
                (keyword_script_name_span, (identifier_script_name, identifier_script_name_span)),
                identifier_extends,
            ),
            script_flags,
        )| {
            let end_index = match script_flags.as_ref() {
                Some((_, script_flags_span)) => script_flags_span.end,
                None => match identifier_extends.as_ref() {
                    Some((_, identifier_extends_span)) => identifier_extends_span.end,
                    None => identifier_script_name_span.end,
                },
            };

            (
                Expr::HeaderLine(
                    identifier_script_name,
                    identifier_extends.map(|(identifier_extends, _)| identifier_extends),
                    script_flags.map(|(script_flags, _)| script_flags),
                ),
                keyword_script_name_span.start..end_index,
            )
        },
    );

    return header;
}

#[cfg(test)]
mod test {
    use crate::ast::ScriptFlag;
    use crate::{parser, Expr, SpannedExpr};
    use chumsky::Parser;
    use papyrus_compiler_lexer::{SpannedLexer, SpannedToken};
    use std::path::Path;

    #[test]
    fn test_header_line() {
        let data: Vec<(&str, SpannedExpr)> = vec![
            (
                "ScriptName MyScript",
                (Expr::HeaderLine("MyScript", None, None), 0..19),
            ),
            (
                "ScriptName MyScript extends OtherScript",
                (
                    Expr::HeaderLine("MyScript", Some("OtherScript"), None),
                    0..39,
                ),
            ),
            (
                "ScriptName MyScript extends OtherScript Conditional Const DebugOnly BetaOnly Hidden Native Default",
                (
                    Expr::HeaderLine("MyScript", Some("OtherScript"), Some(vec![
                        ScriptFlag::Conditional,
                        ScriptFlag::Const,
                        ScriptFlag::DebugOnly,
                        ScriptFlag::BetaOnly,
                        ScriptFlag::Hidden,
                        ScriptFlag::Native,
                        ScriptFlag::Default
                    ])),
                    0..98
                ),
            ),
        ];

        for (src, expected) in data {
            let tokens = SpannedLexer::lex_all(src);
            let res = parser().parse(tokens).unwrap();
            assert_eq!(res, expected);
        }
    }

    #[test]
    #[cfg(feature = "test-external-scripts")]
    fn test_external_scripts() {
        let path = Path::new("../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc");
        assert!(path.exists());

        let src = std::fs::read_to_string(path).unwrap();
        let lexer = SpannedLexer::new(src.as_str());
        let tokens: Vec<SpannedToken> = lexer.collect();

        let res = parser().parse(tokens);
        assert!(res.is_ok());

        let res = res.unwrap();
        println!("{:?}", res);
    }
}
