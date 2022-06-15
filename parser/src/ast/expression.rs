use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::literal::{literal_parser, Literal};
use crate::ast::node::Node;
use crate::ast::types::{type_name_parser, TypeName};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    /// 'a && b'
    LogicalOperation {
        lhs: Node<Expression<'a>>,
        kind: Node<LogicalKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a != b'
    Comparison {
        lhs: Node<Expression<'a>>,
        kind: Node<ComparisonKind>,
        rhs: Node<Expression<'a>>,
    },
    /// '!a'
    Unary {
        kind: Node<UnaryKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a + b'
    Binary {
        lhs: Node<Expression<'a>>,
        kind: Node<BinaryKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a[i]'
    ArrayAccess {
        array: Node<Expression<'a>>,
        index: Node<Expression<'a>>,
    },
    /// 'MyObject.MyProperty'
    MemberAccess {
        lhs: Node<Expression<'a>>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a as string'
    Cast {
        lhs: Node<Expression<'a>>,
        rhs: Node<TypeName<'a>>,
    },
    /// 'a is string'
    TypeCheck {
        lhs: Node<Expression<'a>>,
        rhs: Node<TypeName<'a>>,
    },
    /// 'new int[10 * count]'
    NewArray {
        element_type: Node<TypeName<'a>>,
        size: Node<Expression<'a>>,
    },
    /// 'new Point'
    NewStructure(Node<TypeName<'a>>),
    /// 'DoSomething(a, b, 1, 3, "Hello World")'
    FunctionCall {
        name: Node<Expression<'a>>,
        arguments: Option<Vec<Node<Expression<'a>>>>,
    },
    /// '1', '"Hello World"', '1.0', 'false', 'none'
    Literal(Node<Literal<'a>>),
    Identifier(Node<Identifier<'a>>),
    Self_,
    Parent,
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::LogicalOperation { lhs, kind, rhs } => {
                write!(f, "{} {} {}", lhs, kind, rhs)
            }
            Expression::Comparison { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
            Expression::Unary { kind, rhs } => write!(f, "{}{}", kind, rhs),
            Expression::Binary { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
            Expression::ArrayAccess { array, index } => write!(f, "{}[{}]", array, index),
            Expression::MemberAccess { lhs, rhs } => write!(f, "{}.{}", lhs, rhs),
            Expression::Cast { lhs, rhs } => write!(f, "{} as {}", lhs, rhs),
            Expression::TypeCheck { lhs, rhs } => write!(f, "{} is {}", lhs, rhs),
            Expression::NewArray { element_type, size } => {
                write!(f, "new {}[{}]", element_type, size)
            }
            Expression::NewStructure(type_name) => write!(f, "new {}", type_name),
            Expression::FunctionCall { name, arguments } => {
                write!(f, "{} (", name)?;

                match arguments.as_ref() {
                    Some(arguments) => {
                        for i in 0..arguments.len() {
                            let argument = arguments.get(i).unwrap();
                            if i == arguments.len() - 1 {
                                write!(f, "{}", argument)?;
                            } else {
                                write!(f, "{}, ", argument)?;
                            }
                        }
                    }
                    None => {}
                }

                write!(f, ")")?;

                Ok(())
            }
            Expression::Literal(value) => write!(f, "{}", value),
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::Parent => write!(f, "Parent"),
            Expression::Self_ => write!(f, "Self"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LogicalKind {
    /// '&&'
    And,
    /// '||'
    Or,
}

impl Display for LogicalKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalKind::And => write!(f, "&&"),
            LogicalKind::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ComparisonKind {
    /// '=='
    EqualTo,
    /// '!='
    NotEqualTo,
    /// '>'
    GreaterThan,
    /// '<'
    LessThan,
    /// '>='
    GreaterThanOrEqualTo,
    /// '<='
    LessThanOrEqualTo,
}

impl Display for ComparisonKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonKind::EqualTo => write!(f, "=="),
            ComparisonKind::NotEqualTo => write!(f, "!="),
            ComparisonKind::GreaterThan => write!(f, ">"),
            ComparisonKind::LessThan => write!(f, "<"),
            ComparisonKind::GreaterThanOrEqualTo => write!(f, ">="),
            ComparisonKind::LessThanOrEqualTo => write!(f, "<="),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnaryKind {
    /// '!'
    LogicalNot,
    /// '-'
    Negative,
}

impl Display for UnaryKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryKind::LogicalNot => write!(f, "!"),
            UnaryKind::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryKind {
    /// '+'
    Addition,
    /// '-'
    Subtraction,
    /// '*'
    Multiplication,
    /// '/'
    Division,
    /// '%'
    Modulus,
}

impl Display for BinaryKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryKind::Addition => write!(f, "+"),
            BinaryKind::Subtraction => write!(f, "-"),
            BinaryKind::Multiplication => write!(f, "*"),
            BinaryKind::Division => write!(f, "/"),
            BinaryKind::Modulus => write!(f, "%"),
        }
    }
}

/// ```ebnf
/// <expression>       ::= <and expression> ('||' <and expression>)*
/// <and expression>   ::= <bool expression> ('&&' <bool expression>)*
/// <bool expression>  ::= <add expression> (<comparison operator> <add expression>)*
/// <add expression>   ::= <mult expression> (('+' | '-') <mult expression>)*
/// <mult expression>  ::= <unary expression> (('*' | '/' | '%') <unary expression>)*
/// <unary expression> ::= ['-' | '!'] <cast atom>
/// <cast atom>        ::= <dot atom> ['as' <type>]
/// <dot atom>         ::= (<array atom> ('.' <array func or id>)*) | <constant>
/// <array atom>       ::= <atom> ['[' <expression> ']']
/// <atom>             ::= ('(' <expression> ')') | ('new' <type> '[' <expression> ']') | <func or id>
/// <array func or id> ::= <func or id> ['[' <expression> ']']
/// <func or id>       ::= <function call> | 'self' | 'parent'
/// ```
pub fn expression_parser<'a>() -> impl TokenParser<'a, Expression<'a>> {
    recursive(|expr| {
        let literal = literal_parser()
            .map_with_span(Node::new)
            .map(Expression::Literal);

        let identifier = identifier_parser()
            .map_with_span(Node::new)
            .map(Expression::Identifier);

        let func_or_id = select! {
            Token::Keyword(KeywordKind::Parent) => Expression::Parent,
            Token::Keyword(KeywordKind::Self_) => Expression::Self_
        }
        .or(identifier
            .clone()
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::ParenthesisOpen))
                    .ignore_then(
                        expr.clone()
                            .map_with_span(Node::new)
                            .separated_by(just(Token::Operator(OperatorKind::Comma)))
                            .map(|parameters| {
                                if parameters.is_empty() {
                                    None
                                } else {
                                    Some(parameters)
                                }
                            }),
                    )
                    .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
                    .or_not()
                    .map(|option| match option {
                        Some(option) => option,
                        None => None,
                    }),
            )
            .map(|output| {
                let (identifier, function_call_arguments) = output;
                match function_call_arguments {
                    Some(function_call_arguments) => Expression::FunctionCall {
                        name: identifier,
                        arguments: Some(function_call_arguments),
                    },
                    None => identifier.into_inner(),
                }
            }));

        let array_func_or_id = func_or_id
            .clone()
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::SquareBracketsOpen))
                    .ignore_then(expr.clone().map_with_span(Node::new))
                    .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
                    .or_not(),
            )
            .map(|output| {
                let (lhs, array_index) = output;
                match array_index {
                    Some(array_index) => Expression::ArrayAccess {
                        array: lhs,
                        index: array_index,
                    },
                    None => lhs.into_inner(),
                }
            });

        let atom = just(Token::Operator(OperatorKind::ParenthesisOpen))
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
            .or(just(Token::Keyword(KeywordKind::New))
                .ignore_then(
                    type_name_parser().map_with_span(Node::new).then(
                        just(Token::Operator(OperatorKind::SquareBracketsOpen))
                            .ignore_then(expr.clone().map_with_span(Node::new))
                            .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
                            .or_not(),
                    ),
                )
                .map(|output| {
                    let (type_name, array_length) = output;
                    match array_length {
                        Some(array_length) => Expression::NewArray {
                            element_type: type_name,
                            size: array_length,
                        },
                        None => Expression::NewStructure(type_name),
                    }
                }))
            .or(func_or_id.clone());

        let array_atom = atom
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::SquareBracketsOpen))
                    .ignore_then(expr.clone().map_with_span(Node::new))
                    .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
                    .or_not(),
            )
            .map(|output| {
                let (lhs, array_index) = output;
                match array_index {
                    Some(array_index) => Expression::ArrayAccess {
                        array: lhs,
                        index: array_index,
                    },
                    None => lhs.into_inner(),
                }
            })
            .boxed();

        let dot_atom = literal.or(array_atom
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::Access))
                    .ignore_then(array_func_or_id.map_with_span(Node::new))
                    .repeated(),
            )
            .foldl(|lhs, rhs| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::MemberAccess { lhs, rhs }, span)
            })
            .map(|x| x.into_inner()));

        let cast_atom = dot_atom
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::CastAs))
                    .to(true)
                    .or(just(Token::Operator(OperatorKind::CastIs)).to(false))
                    .then(type_name_parser().map_with_span(Node::new))
                    .or_not(),
            )
            .map(|(lhs, rhs)| match rhs {
                Some((is_cast_as, type_name)) => {
                    if is_cast_as {
                        Expression::Cast {
                            lhs,
                            rhs: type_name,
                        }
                    } else {
                        Expression::TypeCheck {
                            lhs,
                            rhs: type_name,
                        }
                    }
                }
                None => lhs.into_inner(),
            })
            .boxed();

        let unary_expression = just(Token::Operator(OperatorKind::Subtraction))
            .to(UnaryKind::Negative)
            .or(just(Token::Operator(OperatorKind::LogicalNot)).to(UnaryKind::LogicalNot))
            .map_with_span(Node::new)
            .or_not()
            .then(cast_atom.map_with_span(Node::new))
            .map(|(kind, rhs)| match kind {
                Some(kind) => Expression::Unary { kind, rhs },
                None => rhs.into_inner(),
            });

        let mult_expression = unary_expression
            .clone()
            .map_with_span(Node::new)
            .then(
                select! {
                    Token::Operator(OperatorKind::Multiplication) => BinaryKind::Multiplication,
                    Token::Operator(OperatorKind::Division) => BinaryKind::Division,
                    Token::Operator(OperatorKind::Modulus) => BinaryKind::Modulus,
                }
                .map_with_span(Node::new)
                .then(unary_expression.clone().map_with_span(Node::new))
                .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::Binary { lhs, kind, rhs }, span)
            })
            .map(|x| x.into_inner());

        let add_expression = mult_expression
            .clone()
            .map_with_span(Node::new)
            .then(
                select! {
                    Token::Operator(OperatorKind::Addition) => BinaryKind::Addition,
                    Token::Operator(OperatorKind::Subtraction) => BinaryKind::Subtraction,
                }
                .map_with_span(Node::new)
                .then(mult_expression.clone().map_with_span(Node::new))
                .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::Binary { lhs, kind, rhs }, span)
            })
            .map(|x| x.into_inner());

        let comparison_expression = add_expression
            .clone()
            .map_with_span(Node::new)
            .then(
                select! {
                    Token::Operator(OperatorKind::EqualTo) => ComparisonKind::EqualTo,
                    Token::Operator(OperatorKind::NotEqualTo) => ComparisonKind::NotEqualTo,
                    Token::Operator(OperatorKind::GreaterThan) => ComparisonKind::GreaterThan,
                    Token::Operator(OperatorKind::LessThan) => ComparisonKind::LessThan,
                    Token::Operator(OperatorKind::GreaterThanOrEqualTo) => ComparisonKind::GreaterThanOrEqualTo,
                    Token::Operator(OperatorKind::LessThanOrEqualTo) => ComparisonKind::LessThanOrEqualTo,
                }
                    .map_with_span(Node::new)
                    .then(add_expression.clone().map_with_span(Node::new))
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::Comparison { lhs, kind, rhs }, span)
            })
            .map(|x| x.into_inner()).boxed();

        let and_expression = comparison_expression
            .clone()
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::LogicalAnd))
                    .to(LogicalKind::And)
                    .map_with_span(Node::new)
                    .then(comparison_expression.clone().map_with_span(Node::new))
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::LogicalOperation { lhs, kind, rhs }, span)
            })
            .map(|x| x.into_inner());

        let or_expression = and_expression
            .clone()
            .map_with_span(Node::new)
            .then(
                just(Token::Operator(OperatorKind::LogicalOr))
                    .to(LogicalKind::Or)
                    .map_with_span(Node::new)
                    .then(and_expression.clone().map_with_span(Node::new))
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = lhs.span_union(&rhs);
                Node::new(Expression::LogicalOperation { lhs, kind, rhs }, span)
            })
            .map(|x| x.into_inner());

        or_expression
    })
}

#[cfg(test)]
mod test {
    use crate::ast::expression::{
        expression_parser, BinaryKind, ComparisonKind, Expression, LogicalKind, UnaryKind,
    };
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::types::{BaseType, TypeName};
    use crate::parse::test_utils::{run_test, run_tests};

    #[test]
    fn test_literal_expression() {
        let data = vec![
            (
                "none",
                Expression::Literal(Node::new(Literal::None, (0..4).into())),
            ),
            (
                "1",
                Expression::Literal(Node::new(Literal::Integer(1), (0..1).into())),
            ),
            (
                "1.0",
                Expression::Literal(Node::new(Literal::Float(1.0), (0..3).into())),
            ),
            (
                "false",
                Expression::Literal(Node::new(Literal::Boolean(false), (0..5).into())),
            ),
            (
                r#""Hello World!""#,
                Expression::Literal(Node::new(Literal::String("Hello World!"), (0..14).into())),
            ),
        ];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_self_parent_expression() {
        let data = vec![("self", Expression::Self_), ("parent", Expression::Parent)];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_function_call_expression() {
        let src = r#"MyFunc(someArgument, anotherArgument, 1, 1.0, false, "Hi!", none)"#;
        let expected = Expression::FunctionCall {
            name: Node::new(
                Expression::Identifier(Node::new("MyFunc", (0..6).into())),
                (0..6).into(),
            ),
            arguments: Some(vec![
                Node::new(
                    Expression::Identifier(Node::new("someArgument", (7..19).into())),
                    (7..19).into(),
                ),
                Node::new(
                    Expression::Identifier(Node::new("anotherArgument", (21..36).into())),
                    (21..36).into(),
                ),
                Node::new(
                    Expression::Literal(Node::new(Literal::Integer(1), (38..39).into())),
                    (38..39).into(),
                ),
                Node::new(
                    Expression::Literal(Node::new(Literal::Float(1.0), (41..44).into())),
                    (41..44).into(),
                ),
                Node::new(
                    Expression::Literal(Node::new(Literal::Boolean(false), (46..51).into())),
                    (46..51).into(),
                ),
                Node::new(
                    Expression::Literal(Node::new(Literal::String("Hi!"), (53..58).into())),
                    (53..58).into(),
                ),
                Node::new(
                    Expression::Literal(Node::new(Literal::None, (60..64).into())),
                    (60..64).into(),
                ),
            ]),
        };

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_nested_parentheses() {
        let src = r#"(((((((((("Help I'm Stuck!"))))))))))"#;
        let expected = Expression::Literal(Node::new(
            Literal::String("Help I'm Stuck!"),
            (10..27).into(),
        ));

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_new_array_expression() {
        let src = "new int[100]";
        let expected = Expression::NewArray {
            element_type: Node::new(TypeName::BaseType(BaseType::Int), (4..7).into()),
            size: Node::new(
                Expression::Literal(Node::new(Literal::Integer(100), (8..11).into())),
                (8..11).into(),
            ),
        };

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_new_structure_expression() {
        let src = "new CoolStruct";
        let expected = Expression::NewStructure(Node::new(
            TypeName::Identifier("CoolStruct"),
            (4..14).into(),
        ));

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_array_access_expression() {
        let src = "myCoolArray[10]";
        let expected = Expression::ArrayAccess {
            array: Node::new(
                Expression::Identifier(Node::new("myCoolArray", (0..11).into())),
                (0..11).into(),
            ),
            index: Node::new(
                Expression::Literal(Node::new(Literal::Integer(10), (12..14).into())),
                (12..14).into(),
            ),
        };

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_member_access_expression() {
        let data = vec![
            (
                "MyObject.AnotherObject.MyProperty",
                Expression::MemberAccess {
                    lhs: Node::new(
                        Expression::MemberAccess {
                            lhs: Node::new(
                                Expression::Identifier(Node::new("MyObject", (0..8).into())),
                                (0..8).into(),
                            ),
                            rhs: Node::new(
                                Expression::Identifier(Node::new("AnotherObject", (9..22).into())),
                                (9..22).into(),
                            ),
                        },
                        (0..22).into(),
                    ),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("MyProperty", (23..33).into())),
                        (23..33).into(),
                    ),
                },
            ),
            (
                "MyObject.MyProperty",
                Expression::MemberAccess {
                    lhs: Node::new(
                        Expression::Identifier(Node::new("MyObject", (0..8).into())),
                        (0..8).into(),
                    ),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("MyProperty", (9..19).into())),
                        (9..19).into(),
                    ),
                },
            ),
        ];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_cast_expression() {
        let src = "x as int";
        let expected = Expression::Cast {
            lhs: Node::new(
                Expression::Identifier(Node::new("x", (0..1).into())),
                (0..1).into(),
            ),
            rhs: Node::new(TypeName::BaseType(BaseType::Int), (5..8).into()),
        };

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_type_check_expression() {
        let src = "x is int";
        let expected = Expression::TypeCheck {
            lhs: Node::new(
                Expression::Identifier(Node::new("x", (0..1).into())),
                (0..1).into(),
            ),
            rhs: Node::new(TypeName::BaseType(BaseType::Int), (5..8).into()),
        };

        run_test(src, expected, expression_parser);
    }

    #[test]
    fn test_unary_expression() {
        let data = vec![
            (
                "!x",
                Expression::Unary {
                    kind: Node::new(UnaryKind::LogicalNot, (0..1).into()),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("x", (1..2).into())),
                        (1..2).into(),
                    ),
                },
            ),
            (
                "-x",
                Expression::Unary {
                    kind: Node::new(UnaryKind::Negative, (0..1).into()),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("x", (1..2).into())),
                        (1..2).into(),
                    ),
                },
            ),
        ];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_binary_expression() {
        let lhs = Node::new(
            Expression::Identifier(Node::new("x", (0..1).into())),
            (0..1).into(),
        );
        let rhs = Node::new(
            Expression::Identifier(Node::new("y", (4..5).into())),
            (4..5).into(),
        );

        let data = vec![
            (
                "x * y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: Node::new(BinaryKind::Multiplication, (2..3).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x / y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: Node::new(BinaryKind::Division, (2..3).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x % y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: Node::new(BinaryKind::Modulus, (2..3).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x + y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: Node::new(BinaryKind::Addition, (2..3).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x - y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: Node::new(BinaryKind::Subtraction, (2..3).into()),
                    rhs: rhs.clone(),
                },
            ),
        ];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_comparison_expression() {
        let lhs = Node::new(
            Expression::Identifier(Node::new("x", (0..1).into())),
            (0..1).into(),
        );

        let single_rhs = Node::new(
            Expression::Identifier(Node::new("y", (4..5).into())),
            (4..5).into(),
        );

        let double_rhs = Node::new(
            Expression::Identifier(Node::new("y", (5..6).into())),
            (5..6).into(),
        );

        let data = vec![
            (
                "x == y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::EqualTo, (2..4).into()),
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x != y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::NotEqualTo, (2..4).into()),
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x > y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::GreaterThan, (2..3).into()),
                    rhs: single_rhs.clone(),
                },
            ),
            (
                "x < y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::LessThan, (2..3).into()),
                    rhs: single_rhs.clone(),
                },
            ),
            (
                "x >= y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::GreaterThanOrEqualTo, (2..4).into()),
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x <= y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: Node::new(ComparisonKind::LessThanOrEqualTo, (2..4).into()),
                    rhs: double_rhs.clone(),
                },
            ),
        ];

        run_tests(data, expression_parser);
    }

    #[test]
    fn test_logical_operation_expression() {
        let lhs = Node::new(
            Expression::Identifier(Node::new("x", (0..1).into())),
            (0..1).into(),
        );
        let rhs = Node::new(
            Expression::Identifier(Node::new("y", (5..6).into())),
            (5..6).into(),
        );

        let data = vec![
            (
                "x && y",
                Expression::LogicalOperation {
                    lhs: lhs.clone(),
                    kind: Node::new(LogicalKind::And, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x || y",
                Expression::LogicalOperation {
                    lhs: lhs.clone(),
                    kind: Node::new(LogicalKind::Or, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
        ];

        run_tests(data, expression_parser);
    }
}
