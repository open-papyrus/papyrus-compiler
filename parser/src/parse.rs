use crate::ast::property::{AutoProperty, PropertyGroup};
use crate::ast::structure::{Structure, StructureField};
use crate::ast::types::{BaseType, Type, TypeName};
use crate::ast::variable::ScriptVariable;
use crate::ast::{flags::*, identifier::*, literal::*, node::*, script::*};
use crate::error::Error;
use crate::span::Span;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::ops::Range;
use std::vec::IntoIter;

pub trait TokenParser<'a, O> = Parser<Token<'a>, O, Error = Error<'a>> + Clone;
pub type TokenStream<'a> =
    chumsky::Stream<'a, Token<'a>, Range<usize>, IntoIter<(Token<'a>, Span)>>;

pub fn create_token_stream(tokens: Vec<(Token, Span)>) -> TokenStream {
    let eoi = match tokens.last() {
        Some((_, span)) => span.clone(),
        None => Span { start: 0, end: 0 },
    };

    chumsky::Stream::from_iter(eoi, tokens.into_iter())
}

pub fn run_lexer_and_get_stream(input: &str) -> TokenStream {
    let tokens = papyrus_compiler_lexer::run_lexer(input);
    create_token_stream(tokens)
}

pub fn literal_parser<'a>() -> impl TokenParser<'a, Literal<'a>> {
    select! {
        Token::BooleanLiteral(value) => Literal::Boolean(value),
        Token::IntegerLiteral(value) => Literal::Integer(value),
        Token::FloatLiteral(value) => Literal::Float(value),
        Token::StringLiteral(value) => Literal::String(value),
        Token::NoneLiteral => Literal::None,
    }
}

pub fn identifier_parser<'a>() -> impl TokenParser<'a, Identifier<'a>> {
    select! {
        Token::Identifier(value) => value
    }
}

pub fn script_flag_parser<'a>() -> impl TokenParser<'a, ScriptFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => ScriptFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => ScriptFlag::Const,
        Token::Keyword(KeywordKind::DebugOnly) => ScriptFlag::DebugOnly,
        Token::Keyword(KeywordKind::BetaOnly) => ScriptFlag::BetaOnly,
        Token::Keyword(KeywordKind::Hidden) => ScriptFlag::Hidden,
        Token::Keyword(KeywordKind::Native) => ScriptFlag::Native,
        Token::Keyword(KeywordKind::Default) => ScriptFlag::Default,
    }
}

pub fn property_flag_parser<'a>() -> impl TokenParser<'a, PropertyFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => PropertyFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => PropertyFlag::Const,
        Token::Keyword(KeywordKind::Hidden) => PropertyFlag::Hidden,
        Token::Keyword(KeywordKind::Mandatory) => PropertyFlag::Mandatory,
    }
}

pub fn variable_flag_parser<'a>() -> impl TokenParser<'a, VariableFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => VariableFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => VariableFlag::Const,
        Token::Keyword(KeywordKind::Hidden) => VariableFlag::Hidden,
    }
}

pub fn group_flag_parser<'a>() -> impl TokenParser<'a, GroupFlag> {
    select! {
        Token::Keyword(KeywordKind::CollapsedOnRef) => GroupFlag::CollapsedOnRef,
        Token::Keyword(KeywordKind::CollapsedOnBase) => GroupFlag::CollapsedOnBase,
        Token::Keyword(KeywordKind::Collapsed) => GroupFlag::Collapsed,
    }
}

pub fn function_flag_parser<'a>() -> impl TokenParser<'a, FunctionFlag> {
    select! {
        Token::Keyword(KeywordKind::DebugOnly) => FunctionFlag::DebugOnly,
        Token::Keyword(KeywordKind::BetaOnly) => FunctionFlag::BetaOnly,
    }
}

pub fn type_parser<'a>() -> impl TokenParser<'a, Type<'a>> {
    select! {
        Token::Keyword(KeywordKind::Bool) => BaseType::Bool,
        Token::Keyword(KeywordKind::Int) => BaseType::Int,
        Token::Keyword(KeywordKind::Float) => BaseType::Float,
        Token::Keyword(KeywordKind::String) => BaseType::String,
        Token::Keyword(KeywordKind::Var) => BaseType::Var,
    }
    .map(TypeName::BaseType)
    .or(identifier_parser().map(TypeName::Identifier))
    .map_with_span(Node::new)
    .then(
        just(Token::Operator(OperatorKind::SquareBracketsOpen))
            .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
            .or_not()
            .map(|option| !matches!(option, None)),
    )
    .map(|(type_name, is_array)| Type::new(type_name, is_array))
}

pub fn script_variable_parser<'a>() -> impl TokenParser<'a, ScriptVariable<'a>> {
    type_parser()
        .map_with_span(Node::new)
        .then(identifier_parser().map_with_span(Node::new))
        .then(
            just(Token::Operator(OperatorKind::Assignment))
                .ignore_then(literal_parser().map_with_span(Node::new))
                .or_not(),
        )
        .then(
            variable_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let (((type_node, identifier), initial_value), flags) = output;
            ScriptVariable::new(type_node, identifier, initial_value, flags)
        })
}

pub fn auto_property_parser<'a>() -> impl TokenParser<'a, AutoProperty<'a>> {
    type_parser()
        .map_with_span(Node::new)
        .then_ignore(just(Token::Keyword(KeywordKind::Property)))
        .then(identifier_parser().map_with_span(Node::new))
        .then(
            just(Token::Operator(OperatorKind::Assignment))
                .ignore_then(literal_parser().map_with_span(Node::new))
                .or_not(),
        )
        .then(
            just(Token::Keyword(KeywordKind::Auto))
                .map(|_| false)
                .or(just(Token::Keyword(KeywordKind::AutoReadOnly)).map(|_| true)),
        )
        .then(
            property_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let ((((type_node, identifier), literal), is_read_only), flags) = output;
            AutoProperty::new(type_node, identifier, literal, flags, is_read_only)
        })
}

pub fn property_group_parser<'a>() -> impl TokenParser<'a, PropertyGroup<'a>> {
    just(Token::Keyword(KeywordKind::Group))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then(
            group_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .or_not(),
        )
        .then(
            auto_property_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndGroup)))
        .map(|output| {
            let ((identifier, flags), auto_properties) = output;
            PropertyGroup::new(identifier, flags, auto_properties)
        })
}

pub fn struct_field_parser<'a>() -> impl TokenParser<'a, StructureField<'a>> {
    script_variable_parser()
}

pub fn struct_parser<'a>() -> impl TokenParser<'a, Structure<'a>> {
    just(Token::Keyword(KeywordKind::Struct))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then(
            struct_field_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndStruct)))
        .map(|output| {
            let (identifier, fields) = output;
            Structure::new(identifier, fields)
        })
}

pub fn import_parser<'a>() -> impl TokenParser<'a, Identifier<'a>> {
    just(Token::Keyword(KeywordKind::Import)).ignore_then(identifier_parser())
}

pub fn script_parser<'a>() -> impl TokenParser<'a, Script<'a>> {
    just(Token::Keyword(KeywordKind::ScriptName))
        .ignore_then(
            identifier_parser()
                .map_with_span(Node::new)
                .then(
                    just(Token::Keyword(KeywordKind::Extends))
                        .ignore_then(identifier_parser().map_with_span(Node::new))
                        .or_not(),
                )
                .then(
                    script_flag_parser()
                        .map_with_span(Node::new)
                        .repeated()
                        .at_least(1)
                        .or_not(),
                ),
        )
        .map(|output| {
            let ((name_identifier, extends_identifier), script_flags) = output;
            Script::new(name_identifier, extends_identifier, script_flags)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::flags::{GroupFlag, PropertyFlag, ScriptFlag, VariableFlag};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::property::{AutoProperty, PropertyGroup};
    use crate::ast::script::Script;
    use crate::ast::structure::{Structure, StructureField};
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::ast::variable::ScriptVariable;
    use crate::parse::{
        auto_property_parser, import_parser, literal_parser, property_group_parser,
        run_lexer_and_get_stream, script_parser, script_variable_parser, struct_parser,
        type_parser, TokenParser,
    };
    use chumsky::prelude::*;

    #[test]
    fn test_literal_parser() {
        let data = vec![
            ("1", Literal::Integer(1)),
            ("1.0", Literal::Float(1.0)),
            ("false", Literal::Boolean(false)),
            (r#""Hello World""#, Literal::String("Hello World")),
            ("none", Literal::None),
        ];

        for (src, expected) in data {
            let token_stream = run_lexer_and_get_stream(src);
            let res = literal_parser()
                .then_ignore(end())
                .parse(token_stream)
                .unwrap();
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_script_parser() {
        let src = "ScriptName MyScript extends OtherScript Conditional Const DebugOnly BetaOnly Hidden Native Default";
        let expected = Script::new(
            Node::new("MyScript", 11..19),
            Some(Node::new("OtherScript", 28..39)),
            Some(vec![
                (Node::new(ScriptFlag::Conditional, 40..51)),
                (Node::new(ScriptFlag::Const, 52..57)),
                (Node::new(ScriptFlag::DebugOnly, 58..67)),
                (Node::new(ScriptFlag::BetaOnly, 68..76)),
                (Node::new(ScriptFlag::Hidden, 77..83)),
                (Node::new(ScriptFlag::Native, 84..90)),
                (Node::new(ScriptFlag::Default, 91..98)),
            ]),
        );

        let token_stream = run_lexer_and_get_stream(src);
        let res = script_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_type_parser() {
        let data = vec![
            (
                "bool",
                Type::new(Node::new(TypeName::BaseType(BaseType::Bool), 0..4), false),
            ),
            (
                "int",
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
            ),
            (
                "float",
                Type::new(Node::new(TypeName::BaseType(BaseType::Float), 0..5), false),
            ),
            (
                "string",
                Type::new(Node::new(TypeName::BaseType(BaseType::String), 0..6), false),
            ),
            (
                "var",
                Type::new(Node::new(TypeName::BaseType(BaseType::Var), 0..3), false),
            ),
            (
                "custom",
                Type::new(Node::new(TypeName::Identifier("custom"), 0..6), false),
            ),
            (
                "bool[]",
                Type::new(Node::new(TypeName::BaseType(BaseType::Bool), 0..4), true),
            ),
            (
                "int[]",
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), true),
            ),
            (
                "float[]",
                Type::new(Node::new(TypeName::BaseType(BaseType::Float), 0..5), true),
            ),
            (
                "string[]",
                Type::new(Node::new(TypeName::BaseType(BaseType::String), 0..6), true),
            ),
            (
                "custom[]",
                Type::new(Node::new(TypeName::Identifier("custom"), 0..6), true),
            ),
        ];

        for (src, expected) in data {
            let token_stream = run_lexer_and_get_stream(src);
            let res = type_parser()
                .then_ignore(end())
                .parse(token_stream)
                .unwrap();
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_script_variable_parser() {
        let src = "int x = 0 Conditional Const Hidden";
        let expected = ScriptVariable::new(
            Node::new(
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
                0..3,
            ),
            Node::new("x", 4..5),
            Some(Node::new(Literal::Integer(0), 8..9)),
            Some(vec![
                Node::new(VariableFlag::Conditional, 10..21),
                Node::new(VariableFlag::Const, 22..27),
                Node::new(VariableFlag::Hidden, 28..34),
            ]),
        );

        let token_stream = run_lexer_and_get_stream(src);
        let res = script_variable_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_auto_property_parser() {
        let src = "int Property MyProperty = 1 Auto Conditional Const Hidden Mandatory";
        let expected = AutoProperty::new(
            Node::new(
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
                0..3,
            ),
            Node::new("MyProperty", 13..23),
            Some(Node::new(Literal::Integer(1), 26..27)),
            Some(vec![
                Node::new(PropertyFlag::Conditional, 33..44),
                Node::new(PropertyFlag::Const, 45..50),
                Node::new(PropertyFlag::Hidden, 51..57),
                Node::new(PropertyFlag::Mandatory, 58..67),
            ]),
            false,
        );

        let token_stream = run_lexer_and_get_stream(src);
        let res = auto_property_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_property_group_parser() {
        let src = "Group MyGroup CollapsedOnRef CollapsedOnBase Collapsed\n int Property FirstProperty Auto\n float Property SecondProperty Auto\nEndGroup";
        let expected = PropertyGroup::new(
            Node::new("MyGroup", 6..13),
            Some(vec![
                Node::new(GroupFlag::CollapsedOnRef, 14..28),
                Node::new(GroupFlag::CollapsedOnBase, 29..44),
                Node::new(GroupFlag::Collapsed, 45..54),
            ]),
            vec![
                Node::new(
                    AutoProperty::new(
                        Node::new(
                            Type::new(Node::new(TypeName::BaseType(BaseType::Int), 56..59), false),
                            56..59,
                        ),
                        Node::new("FirstProperty", 69..82),
                        None,
                        None,
                        false,
                    ),
                    56..87,
                ),
                Node::new(
                    AutoProperty::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 89..94),
                                false,
                            ),
                            89..94,
                        ),
                        Node::new("SecondProperty", 104..118),
                        None,
                        None,
                        false,
                    ),
                    89..123,
                ),
            ],
        );

        let token_stream = run_lexer_and_get_stream(src);
        let res = property_group_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_struct_parser() {
        let src = "Struct Point\nfloat X\nfloat Y\nEndStruct";
        let expected = Structure::new(
            Node::new("Point", 7..12),
            vec![
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 13..18),
                                false,
                            ),
                            13..18,
                        ),
                        Node::new("X", 19..20),
                        None,
                        None,
                    ),
                    13..20,
                ),
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 21..26),
                                false,
                            ),
                            21..26,
                        ),
                        Node::new("Y", 27..28),
                        None,
                        None,
                    ),
                    21..28,
                ),
            ],
        );

        let token_stream = run_lexer_and_get_stream(src);
        let res = struct_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_import_parser() {
        let src = "Import Other";
        let expected = "Other";
        let token_stream = run_lexer_and_get_stream(src);
        let res = import_parser()
            .then_ignore(end())
            .parse(token_stream)
            .unwrap();
        assert_eq!(res, expected);
    }
}
