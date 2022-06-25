use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::TypeName;
use crate::parser::{Parse, Parser, ParserError, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'source> {
    /// 'a && b'
    LogicalOperation {
        lhs: Node<Expression<'source>>,
        kind: LogicalKind,
        rhs: Node<Expression<'source>>,
    },
    /// 'a != b'
    Comparison {
        lhs: Node<Expression<'source>>,
        kind: ComparisonKind,
        rhs: Node<Expression<'source>>,
    },
    /// '!a'
    Unary {
        kind: UnaryKind,
        rhs: Node<Expression<'source>>,
    },
    /// 'a + b'
    Binary {
        lhs: Node<Expression<'source>>,
        kind: BinaryKind,
        rhs: Node<Expression<'source>>,
    },
    /// 'a[i]'
    ArrayAccess {
        array: Node<Expression<'source>>,
        index: Node<Expression<'source>>,
    },
    /// 'MyObject.MyProperty'
    MemberAccess {
        lhs: Node<Expression<'source>>,
        rhs: Node<Expression<'source>>,
    },
    /// 'a as string'
    Cast {
        lhs: Node<Expression<'source>>,
        rhs: Node<TypeName<'source>>,
    },
    /// 'a is string'
    TypeCheck {
        lhs: Node<Expression<'source>>,
        rhs: Node<TypeName<'source>>,
    },
    /// 'new int[10 * count]'
    NewArray {
        element_type: Node<TypeName<'source>>,
        size: Node<Expression<'source>>,
    },
    /// 'new Point'
    NewStructure(Node<TypeName<'source>>),
    /// 'DoSomething(a, b, 1, 3, "Hello World")'
    FunctionCall {
        name: Node<Expression<'source>>,
        arguments: Option<Vec<Node<FunctionArgument<'source>>>>,
    },
    /// '1', '"Hello World"', '1.0', 'false', 'none'
    Literal(Node<Literal<'source>>),
    Identifier(Node<Identifier<'source>>),
    Self_,
    Parent,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionArgument<'source> {
    Positional(Node<Expression<'source>>),
    Named {
        name: Node<Identifier<'source>>,
        value: Node<Expression<'source>>,
    },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LogicalKind {
    /// '&&'
    And,
    /// '||'
    Or,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnaryKind {
    /// '!'
    LogicalNot,
    /// '-'
    Negative,
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

impl<'source> Parse<'source> for FunctionArgument<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let argument_name = parser.optional(|parser| {
            let identifier = parser.parse_node::<Identifier>()?;
            parser.expect_operator(OperatorKind::Assignment)?;

            Ok(identifier)
        });

        let value = parser.parse_node::<Expression>()?;

        match argument_name {
            Some(name) => Ok(FunctionArgument::Named { name, value }),
            None => Ok(FunctionArgument::Positional(value)),
        }
    }
}

/// ```ebnf
/// <func or id> ::= <function call> | 'self' | 'parent'
/// ```
fn parse_func_or_id_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let parent_or_self_expression = parser.optional(|parser| {
        let token = parser.consume()?;
        match token {
            Token::Keyword(KeywordKind::Parent) => Ok(Expression::Parent),
            Token::Keyword(KeywordKind::Self_) => Ok(Expression::Self_),
            _ => Err(ParserError::ExpectedOneOf {
                found: *token,
                expected: vec![
                    Token::Keyword(KeywordKind::Parent),
                    Token::Keyword(KeywordKind::Self_),
                ],
            }),
        }
    });

    match parent_or_self_expression {
        Some(expression) => return Ok(expression),
        None => {}
    }

    let identifier = parser
        .with_node(|parser| Ok(Expression::Identifier(parser.parse_node::<Identifier>()?)))?;

    let function_call_arguments = parser.optional(|parser| {
        parser.expect_operator(OperatorKind::ParenthesisOpen)?;
        let arguments = parser.optional_separated(
            |parser| parser.parse_node::<FunctionArgument>(),
            OperatorKind::Comma,
        );
        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        Ok(arguments)
    });

    match function_call_arguments {
        Some(arguments) => Ok(Expression::FunctionCall {
            name: identifier,
            arguments,
        }),
        None => Ok(identifier.into_inner()),
    }
}

fn parse_array_index<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Node<Expression<'source>>> {
    parser.expect_operator(OperatorKind::SquareBracketsOpen)?;
    let array_index = parser.parse_node::<Expression>()?;
    parser.expect_operator(OperatorKind::SquareBracketsClose)?;

    Ok(array_index)
}

/// ```ebnf
/// <array func or id> ::= <func or id> ['[' <expression> ']']
/// ```
fn parse_array_func_or_id_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let func_or_id = parser.with_node(parse_func_or_id_expression)?;

    let array_index = parser.optional(parse_array_index);

    match array_index {
        Some(array_index) => Ok(Expression::ArrayAccess {
            array: func_or_id,
            index: array_index,
        }),
        None => Ok(func_or_id.into_inner()),
    }
}

/// ```ebnf
/// <atom> ::= ('(' <expression> ')') | ('new' <type> '[' <expression> ']') | <func or id>
/// ```
fn parse_atom_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let token = parser.peek();
    match token {
        Some(Token::Operator(OperatorKind::ParenthesisOpen)) => {
            parser.consume()?;
            let nested_expression = Expression::parse(parser)?;
            parser.expect_operator(OperatorKind::ParenthesisClose)?;

            Ok(nested_expression)
        }
        Some(Token::Keyword(KeywordKind::New)) => {
            parser.consume()?;
            let type_name = parser.parse_node::<TypeName>()?;
            let array_size = parser.optional(parse_array_index);

            match array_size {
                Some(array_size) => Ok(Expression::NewArray {
                    element_type: type_name,
                    size: array_size,
                }),
                None => Ok(Expression::NewStructure(type_name)),
            }
        }
        _ => parse_func_or_id_expression(parser),
    }
}

/// ```ebnf
/// <array atom> ::= <atom> ['[' <expression> ']']
/// ```
fn parse_array_atom_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let atom = parser.with_node(parse_atom_expression)?;
    let array_index = parser.optional(parse_array_index);

    match array_index {
        Some(array_index) => Ok(Expression::ArrayAccess {
            array: atom,
            index: array_index,
        }),
        None => Ok(atom.into_inner()),
    }
}

/// ```ebnf
/// <dot atom> ::= (<array atom> ('.' <array func or id>)*) | <constant>
/// ```
fn parse_dot_atom_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let literal = parser.parse_node_optional::<Literal>();
    match literal {
        Some(literal) => return Ok(Expression::Literal(literal)),
        None => {}
    }

    let mut expression = parser.with_node(parse_array_atom_expression)?;
    while parser.peek() == Some(&Token::Operator(OperatorKind::Access)) {
        parser.consume()?;
        let rhs = parser.with_node(parse_array_func_or_id_expression)?;
        let range = expression.range_union(&rhs);

        expression = Node::new(
            Expression::MemberAccess {
                lhs: expression,
                rhs,
            },
            range,
        )
    }

    Ok(expression.into_inner())
}

/// ```ebnf
/// <cast atom> ::= <dot atom> [ ( 'as' | 'is' ) <type>]
/// ```
fn parse_cast_atom_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let expr = parser.with_node(parse_dot_atom_expression)?;

    match parser.peek() {
        Some(Token::Operator(OperatorKind::CastAs)) => {
            parser.consume()?;
            let rhs = parser.parse_node::<TypeName>()?;
            Ok(Expression::Cast { lhs: expr, rhs })
        }
        Some(Token::Operator(OperatorKind::CastIs)) => {
            parser.consume()?;
            let rhs = parser.parse_node::<TypeName>()?;
            Ok(Expression::TypeCheck { lhs: expr, rhs })
        }
        _ => Ok(expr.into_inner()),
    }
}

/// ```ebnf
/// <unary expression> ::= ['-' | '!'] <cast atom>
/// ```
fn parse_unary_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    match parser.peek() {
        Some(Token::Operator(OperatorKind::Subtraction)) => {
            parser.consume()?;
            Ok(Expression::Unary {
                kind: UnaryKind::Negative,
                rhs: parser.with_node(parse_cast_atom_expression)?,
            })
        }
        Some(Token::Operator(OperatorKind::LogicalNot)) => {
            parser.consume()?;
            Ok(Expression::Unary {
                kind: UnaryKind::LogicalNot,
                rhs: parser.with_node(parse_cast_atom_expression)?,
            })
        }
        _ => parse_cast_atom_expression(parser),
    }
}

/// ```ebnf
/// <mult expression>  ::= <unary expression> (('*' | '/' | '%') <unary expression>)*
/// ```
fn parse_mult_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let mut expr = parser.with_node(parse_unary_expression)?;

    loop {
        let kind = match parser.peek() {
            Some(Token::Operator(OperatorKind::Multiplication)) => Some(BinaryKind::Multiplication),
            Some(Token::Operator(OperatorKind::Division)) => Some(BinaryKind::Division),
            Some(Token::Operator(OperatorKind::Modulus)) => Some(BinaryKind::Modulus),
            _ => None,
        };

        match kind {
            Some(kind) => {
                parser.consume()?;
                let rhs = parser.with_node(parse_unary_expression)?;
                let range = expr.range_union(&rhs);

                expr = Node::new(
                    Expression::Binary {
                        lhs: expr,
                        kind,
                        rhs,
                    },
                    range,
                );
            }
            None => break,
        }
    }

    Ok(expr.into_inner())
}

/// ```ebnf
/// <add expression>   ::= <mult expression> (('+' | '-') <mult expression>)*
/// ```
fn parse_add_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let mut expr = parser.with_node(parse_mult_expression)?;

    loop {
        let kind = match parser.peek() {
            Some(Token::Operator(OperatorKind::Addition)) => Some(BinaryKind::Addition),
            Some(Token::Operator(OperatorKind::Subtraction)) => Some(BinaryKind::Subtraction),
            _ => None,
        };

        match kind {
            Some(kind) => {
                parser.consume()?;
                let rhs = parser.with_node(parse_mult_expression)?;
                let range = expr.range_union(&rhs);

                expr = Node::new(
                    Expression::Binary {
                        lhs: expr,
                        kind,
                        rhs,
                    },
                    range,
                );
            }
            None => break,
        }
    }

    Ok(expr.into_inner())
}

/// ```ebnf
/// <bool expression>  ::= <add expression> (<comparison operator> <add expression>)*
/// ```
fn parse_bool_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let mut expr = parser.with_node(parse_add_expression)?;

    loop {
        let kind = match parser.peek() {
            Some(Token::Operator(OperatorKind::EqualTo)) => Some(ComparisonKind::EqualTo),
            Some(Token::Operator(OperatorKind::NotEqualTo)) => Some(ComparisonKind::NotEqualTo),
            Some(Token::Operator(OperatorKind::GreaterThan)) => Some(ComparisonKind::GreaterThan),
            Some(Token::Operator(OperatorKind::LessThan)) => Some(ComparisonKind::LessThan),
            Some(Token::Operator(OperatorKind::GreaterThanOrEqualTo)) => {
                Some(ComparisonKind::GreaterThanOrEqualTo)
            }
            Some(Token::Operator(OperatorKind::LessThanOrEqualTo)) => {
                Some(ComparisonKind::LessThanOrEqualTo)
            }
            _ => None,
        };

        match kind {
            Some(kind) => {
                parser.consume()?;
                let rhs = parser.with_node(parse_add_expression)?;
                let range = expr.range_union(&rhs);

                expr = Node::new(
                    Expression::Comparison {
                        lhs: expr,
                        kind,
                        rhs,
                    },
                    range,
                );
            }
            None => break,
        }
    }

    Ok(expr.into_inner())
}

/// ```ebnf
/// <and expression> ::= <bool expression> ('&&' <bool expression>)*
/// ```
fn parse_and_expression<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Expression<'source>> {
    let mut expression = parser.with_node(parse_bool_expression)?;
    while parser.peek() == Some(&Token::Operator(OperatorKind::LogicalAnd)) {
        parser.consume()?;
        let rhs = parser.with_node(parse_bool_expression)?;
        let range = expression.range_union(&rhs);

        expression = Node::new(
            Expression::LogicalOperation {
                lhs: expression,
                kind: LogicalKind::And,
                rhs,
            },
            range,
        )
    }

    Ok(expression.into_inner())
}

/// ```ebnf
/// <expression> ::= <and expression> ('||' <and expression>)*
/// ```
impl<'source> Parse<'source> for Expression<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let mut expression = parser.with_node(parse_and_expression)?;
        while parser.peek() == Some(&Token::Operator(OperatorKind::LogicalOr)) {
            parser.consume()?;
            let rhs = parser.with_node(parse_and_expression)?;
            let range = expression.range_union(&rhs);

            expression = Node::new(
                Expression::LogicalOperation {
                    lhs: expression,
                    kind: LogicalKind::Or,
                    rhs,
                },
                range,
            )
        }

        Ok(expression.into_inner())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::expression::{
        BinaryKind, ComparisonKind, Expression, FunctionArgument, LogicalKind, UnaryKind,
    };
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::types::{BaseType, TypeName};
    use crate::parser::test_utils::{run_test, run_tests};

    #[test]
    fn test_literal_expression() {
        let data = vec![
            ("none", Expression::Literal(Node::new(Literal::None, 0..4))),
            (
                "1",
                Expression::Literal(Node::new(Literal::Integer(1), 0..1)),
            ),
            (
                "1.0",
                Expression::Literal(Node::new(Literal::Float(1.0), 0..3)),
            ),
            (
                "false",
                Expression::Literal(Node::new(Literal::Boolean(false), 0..5)),
            ),
            (
                r#""Hello World!""#,
                Expression::Literal(Node::new(Literal::String("Hello World!"), 0..14)),
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_self_parent_expression() {
        let data = vec![("self", Expression::Self_), ("parent", Expression::Parent)];

        run_tests(data);
    }

    #[test]
    fn test_function_call_expression() {
        let data = vec![
            (
                r#"MyFunc(someArgument, anotherArgument, 1, 1.0, false, "Hi!", name = none)"#,
                Expression::FunctionCall {
                    name: Node::new(Expression::Identifier(Node::new("MyFunc", 0..6)), 0..6),
                    arguments: Some(vec![
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Identifier(Node::new("someArgument", 7..19)),
                                7..19,
                            )),
                            7..19,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Identifier(Node::new("anotherArgument", 21..36)),
                                21..36,
                            )),
                            21..36,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Node::new(Literal::Integer(1), 38..39)),
                                38..39,
                            )),
                            38..39,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Node::new(Literal::Float(1.0), 41..44)),
                                41..44,
                            )),
                            41..44,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Node::new(Literal::Boolean(false), 46..51)),
                                46..51,
                            )),
                            46..51,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Node::new(Literal::String("Hi!"), 53..58)),
                                53..58,
                            )),
                            53..58,
                        ),
                        Node::new(
                            FunctionArgument::Named {
                                name: Node::new("name", 60..64),
                                value: Node::new(
                                    Expression::Literal(Node::new(Literal::None, 67..71)),
                                    67..71,
                                ),
                            },
                            60..71,
                        ),
                    ]),
                },
            ),
            (
                "MyFunc()",
                Expression::FunctionCall {
                    name: Node::new(Expression::Identifier(Node::new("MyFunc", 0..6)), 0..6),
                    arguments: None,
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_nested_parentheses() {
        let src = r#"(((((((((("Help I'm Stuck!"))))))))))"#;
        let expected = Expression::Literal(Node::new(Literal::String("Help I'm Stuck!"), 10..27));

        run_test(src, expected);
    }

    #[test]
    fn test_new_array_expression() {
        let src = "new int[100]";
        let expected = Expression::NewArray {
            element_type: Node::new(TypeName::BaseType(BaseType::Int), 4..7),
            size: Node::new(
                Expression::Literal(Node::new(Literal::Integer(100), 8..11)),
                8..11,
            ),
        };

        run_test(src, expected);
    }

    #[test]
    fn test_new_structure_expression() {
        let src = "new CoolStruct";
        let expected =
            Expression::NewStructure(Node::new(TypeName::Identifier("CoolStruct"), 4..14));

        run_test(src, expected);
    }

    #[test]
    fn test_array_access_expression() {
        let src = "myCoolArray[10]";
        let expected = Expression::ArrayAccess {
            array: Node::new(
                Expression::Identifier(Node::new("myCoolArray", 0..11)),
                0..11,
            ),
            index: Node::new(
                Expression::Literal(Node::new(Literal::Integer(10), 12..14)),
                12..14,
            ),
        };

        run_test(src, expected);
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
                                Expression::Identifier(Node::new("MyObject", 0..8)),
                                0..8,
                            ),
                            rhs: Node::new(
                                Expression::Identifier(Node::new("AnotherObject", 9..22)),
                                9..22,
                            ),
                        },
                        0..22,
                    ),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("MyProperty", 23..33)),
                        23..33,
                    ),
                },
            ),
            (
                "MyObject.MyProperty",
                Expression::MemberAccess {
                    lhs: Node::new(Expression::Identifier(Node::new("MyObject", 0..8)), 0..8),
                    rhs: Node::new(
                        Expression::Identifier(Node::new("MyProperty", 9..19)),
                        9..19,
                    ),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_cast_expression() {
        let src = "x as int";
        let expected = Expression::Cast {
            lhs: Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1),
            rhs: Node::new(TypeName::BaseType(BaseType::Int), 5..8),
        };

        run_test(src, expected);
    }

    #[test]
    fn test_type_check_expression() {
        let src = "x is int";
        let expected = Expression::TypeCheck {
            lhs: Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1),
            rhs: Node::new(TypeName::BaseType(BaseType::Int), 5..8),
        };

        run_test(src, expected);
    }

    #[test]
    fn test_unary_expression() {
        let data = vec![
            (
                "!x",
                Expression::Unary {
                    kind: UnaryKind::LogicalNot,
                    rhs: Node::new(Expression::Identifier(Node::new("x", 1..2)), 1..2),
                },
            ),
            (
                "-x",
                Expression::Unary {
                    kind: UnaryKind::Negative,
                    rhs: Node::new(Expression::Identifier(Node::new("x", 1..2)), 1..2),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_binary_expression() {
        let lhs = Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1);
        let rhs = Node::new(Expression::Identifier(Node::new("y", 4..5)), 4..5);

        let data = vec![
            (
                "x * y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: BinaryKind::Multiplication,
                    rhs: rhs.clone(),
                },
            ),
            (
                "x / y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: BinaryKind::Division,
                    rhs: rhs.clone(),
                },
            ),
            (
                "x % y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: BinaryKind::Modulus,
                    rhs: rhs.clone(),
                },
            ),
            (
                "x + y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: BinaryKind::Addition,
                    rhs: rhs.clone(),
                },
            ),
            (
                "x - y",
                Expression::Binary {
                    lhs: lhs.clone(),
                    kind: BinaryKind::Subtraction,
                    rhs: rhs.clone(),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_comparison_expression() {
        let lhs = Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1);

        let single_rhs = Node::new(Expression::Identifier(Node::new("y", 4..5)), 4..5);

        let double_rhs = Node::new(Expression::Identifier(Node::new("y", 5..6)), 5..6);

        let data = vec![
            (
                "x == y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::EqualTo,
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x != y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::NotEqualTo,
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x > y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::GreaterThan,
                    rhs: single_rhs.clone(),
                },
            ),
            (
                "x < y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::LessThan,
                    rhs: single_rhs.clone(),
                },
            ),
            (
                "x >= y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::GreaterThanOrEqualTo,
                    rhs: double_rhs.clone(),
                },
            ),
            (
                "x <= y",
                Expression::Comparison {
                    lhs: lhs.clone(),
                    kind: ComparisonKind::LessThanOrEqualTo,
                    rhs: double_rhs.clone(),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_logical_operation_expression() {
        let lhs = Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1);
        let rhs = Node::new(Expression::Identifier(Node::new("y", 5..6)), 5..6);

        let data = vec![
            (
                "x && y",
                Expression::LogicalOperation {
                    lhs: lhs.clone(),
                    kind: LogicalKind::And,
                    rhs: rhs.clone(),
                },
            ),
            (
                "x || y",
                Expression::LogicalOperation {
                    lhs: lhs.clone(),
                    kind: LogicalKind::Or,
                    rhs: rhs.clone(),
                },
            ),
        ];

        run_tests(data);
    }
}
