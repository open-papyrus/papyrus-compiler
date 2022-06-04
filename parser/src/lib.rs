use crate::ast::*;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use papyrus_compiler_lexer::SpannedToken;
pub mod ast;

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

pub fn parser<'a>() -> impl Parser<SpannedToken<'a>, Script<'a>, Error = ParserError<'a>> + Clone {
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

    let script_flag = select! {
        (Token::Keyword(KeywordKind::Conditional), span) => (ScriptFlag::Conditional, span),
        (Token::Keyword(KeywordKind::Const), span) => (ScriptFlag::Const, span),
        (Token::Keyword(KeywordKind::DebugOnly), span) => (ScriptFlag::DebugOnly, span),
        (Token::Keyword(KeywordKind::BetaOnly), span) => (ScriptFlag::BetaOnly, span),
        (Token::Keyword(KeywordKind::Hidden), span) => (ScriptFlag::Hidden, span),
        (Token::Keyword(KeywordKind::Native), span) => (ScriptFlag::Native, span),
        (Token::Keyword(KeywordKind::Default), span) => (ScriptFlag::Default, span),
    };

    let header = filter(|input: &SpannedToken| {
        let (token, _) = input;
        matches!(token, Token::Keyword(KeywordKind::ScriptName))
    })
    .ignore_then(identifier)
    .then(
        filter(|(token, _)| matches!(token, Token::Keyword(KeywordKind::Extends)))
            .ignore_then(identifier)
            .or_not(),
    )
    .then(script_flag.repeated().at_least(1).or_not());

    header.map(|header_output| {
        let ((name, extends), flags) = header_output;

        Script {
            name,
            extends,
            flags,
            expressions: vec![],
        }
    })
}

#[cfg(test)]
mod test {
    use crate::ast::*;
    use crate::parser;
    use chumsky::Parser;
    use papyrus_compiler_lexer::{SpannedLexer, SpannedToken};
    use std::path::Path;

    #[test]
    fn test_header_line() {
        let data: Vec<(&str, Script)> = vec![
            (
                "ScriptName MyScript",
                Script{
                    name: ("MyScript", 11..19),
                    extends: None,
                    flags: None,
                    expressions: vec![]
                }
            ),
            (
                "ScriptName MyScript extends OtherScript",
                Script{
                    name: ("MyScript", 11..19),
                    extends: Some(("OtherScript", 28..39)),
                    flags: None,
                    expressions: vec![]
                }
            ),
            (
                "ScriptName MyScript extends OtherScript Conditional Const DebugOnly BetaOnly Hidden Native Default",
                Script{
                    name: ("MyScript", 11..19),
                    extends: Some(("OtherScript", 28..39)),
                    flags: Some(vec![(ScriptFlag::Conditional, 40..51), (ScriptFlag::Const, 52..57), (ScriptFlag::DebugOnly, 58..67), (ScriptFlag::BetaOnly, 68..76), (ScriptFlag::Hidden, 77..83), (ScriptFlag::Native, 84..90), (ScriptFlag::Default, 91..98)]),
                    expressions: vec![]
                }
            ),
            (
                "ScriptName MyScript Native",
                Script{
                    name: ("MyScript", 11..19),
                    extends: None,
                    flags: Some(vec![(ScriptFlag::Native, 20..26)]),
                    expressions: vec![]
                }
            )
        ];

        for (src, expected) in data {
            let tokens = SpannedLexer::lex_all(src);
            let res = parser().parse(tokens).unwrap();
            assert_eq!(res.name, expected.name);
            assert_eq!(res.extends, expected.extends);
            assert_eq!(res.flags, expected.flags);
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
