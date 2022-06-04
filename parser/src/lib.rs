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

    // let literal = select! {
    //     (Token::BooleanLiteral(value), span) => (Expr::Literal(LiteralKind::BooleanLiteral(value)), span),
    //     (Token::IntegerLiteral(value), span) => (Expr::Literal(LiteralKind::IntegerLiteral(value)), span),
    //     (Token::FloatLiteral(value), span) => (Expr::Literal(LiteralKind::FloatLiteral(value)), span),
    //     (Token::StringLiteral(value), span) => (Expr::Literal(LiteralKind::StringLiteral(value)), span),
    // };

    let known_type_name = select! {
        (Token::Keyword(KeywordKind::Var), span) => (TypeName::KnownType(KnownTypeKind::Var), span),
        (Token::Keyword(KeywordKind::Bool), span) => (TypeName::KnownType(KnownTypeKind::Bool), span),
        (Token::Keyword(KeywordKind::String), span) => (TypeName::KnownType(KnownTypeKind::String), span),
        (Token::Keyword(KeywordKind::Int), span) => (TypeName::KnownType(KnownTypeKind::Int), span),
        (Token::Keyword(KeywordKind::Float), span) => (TypeName::KnownType(KnownTypeKind::Float), span),
    };

    let custom_type_name = select! {
        (Token::Identifier(value), span) => (TypeName::CustomType(value), span)
    };

    let variable_declaration =
        known_type_name
            .or(custom_type_name)
            .then(identifier)
            .map(|output| {
                let ((type_name, type_name_span), (identifier, identifier_span)) = output;
                (
                    Expr::VariableDeclaration(type_name, identifier),
                    type_name_span.start..identifier_span.end,
                )
            });

    // let expression = literal.or_not();

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

    header
        .then(variable_declaration.repeated().or_not())
        .then_ignore(end())
        .map(|header_output| {
            let (((name, extends), flags), expressions) = header_output;

            Script {
                name,
                extends,
                flags,
                expressions: match expressions {
                    Some(expressions) => expressions,
                    None => vec![],
                },
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
    fn test_header() {
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

    fn add_dummy_header(src: &str) -> (String, usize) {
        (format!("ScriptName MyScript\n{}", src), 20)
    }

    fn test_expressions(data: Vec<(&str, (Expr, Span))>) {
        for (src, expected) in data {
            let (src, offset) = add_dummy_header(src);
            let tokens = SpannedLexer::lex_all(src.as_str());

            let res = parser().parse(tokens).unwrap();
            let expressions = res.expressions;
            assert_eq!(expressions.len(), 1);

            let (expected_expression, expected_span) = expected;
            let (expression, span) = expressions.first().unwrap().clone();

            assert_eq!(expression, expected_expression);
            assert_eq!(span.start - offset..span.end - offset, expected_span);
        }
    }

    #[test]
    fn test_variables() {
        let data = vec![
            (
                "int foo",
                (
                    Expr::VariableDeclaration(TypeName::KnownType(KnownTypeKind::Int), "foo"),
                    0..7,
                ),
            ),
            (
                "string foo",
                (
                    Expr::VariableDeclaration(TypeName::KnownType(KnownTypeKind::String), "foo"),
                    0..10,
                ),
            ),
            (
                "bool foo",
                (
                    Expr::VariableDeclaration(TypeName::KnownType(KnownTypeKind::Bool), "foo"),
                    0..8,
                ),
            ),
            (
                "float foo",
                (
                    Expr::VariableDeclaration(TypeName::KnownType(KnownTypeKind::Float), "foo"),
                    0..9,
                ),
            ),
            (
                "var foo",
                (
                    Expr::VariableDeclaration(TypeName::KnownType(KnownTypeKind::Var), "foo"),
                    0..7,
                ),
            ),
        ];

        test_expressions(data);
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
        println!("{:?}", res);
    }
}
