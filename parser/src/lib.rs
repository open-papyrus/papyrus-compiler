use crate::ast::{Expr, ScriptFlag};
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use papyrus_compiler_lexer::SpannedToken;
pub mod ast;

type Span = core::ops::Range<usize>;

pub type SpannedExpr<'a> = (Expr<'a>, Span);

// pub type ParserError<'a> = Cheap<SpannedToken<'a>>;

#[derive(Debug, PartialEq)]
pub enum ParserError<'a> {
    ExpectedFound(
        Span,
        Vec<Option<SpannedToken<'a>>>,
        Option<SpannedToken<'a>>,
    ),
    CustomError(Span, String),
}

impl<'a> ParserError<'a> {
    fn custom(span: Span, msg: String) -> Self {
        Self::CustomError(span, msg)
    }
}

impl<'a> chumsky::Error<SpannedToken<'a>> for ParserError<'a> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<SpannedToken<'a>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<SpannedToken<'a>>,
    ) -> Self {
        Self::ExpectedFound(span, expected.into_iter().collect(), found)
    }

    fn with_label(self, _: Self::Label) -> Self {
        todo!()
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (Self::ExpectedFound(_, expected, _), Self::ExpectedFound(_, expected_other, _)) =
            (&mut self, &mut other)
        {
            expected.append(expected_other);
        }

        self
    }
}

pub fn parser<'a>(
) -> impl Parser<SpannedToken<'a>, SpannedExpr<'a>, Error = ParserError<'a>> + Clone {
    let identifier = filter_map(|_, input: SpannedToken| {
        let (token, span) = input;
        let res = match token {
            Token::Identifier(identifier) => Ok((identifier, span)),
            _ => Err(ParserError::custom(
                span,
                format!("Expected identifier, found {:?}", token),
            )),
        };

        res
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
        filter_map(|_, input: SpannedToken| {
            let (token, span) = input;
            let script_flag = match token {
                Token::Keyword(keyword) => match keyword {
                    KeywordKind::Conditional => Some(ScriptFlag::Conditional),
                    KeywordKind::Const => Some(ScriptFlag::Const),
                    KeywordKind::DebugOnly => Some(ScriptFlag::DebugOnly),
                    KeywordKind::BetaOnly => Some(ScriptFlag::BetaOnly),
                    KeywordKind::Hidden => Some(ScriptFlag::Hidden),
                    KeywordKind::Native => Some(ScriptFlag::Native),
                    KeywordKind::Default => Some(ScriptFlag::Default),
                    _ => None,
                },
                _ => None,
            };

            match script_flag {
                Some(script_flag) => Ok((script_flag, span)),
                None => Err(ParserError::custom(
                    span,
                    format!("Expected a Script Flag, found {:?}", token),
                )),
            }
        })
        .repeated()
        .at_least(1)
        .map(|script_flags| {
            let (_, last_span) = script_flags.last().unwrap();
            let script_flags = script_flags
                .iter()
                .map(|(script_flag, _)| *script_flag)
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
