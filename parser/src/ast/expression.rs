use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::TypeName;
use crate::choose_result;
use crate::parser::{Parse, Parser};
use crate::parser_error::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::cmp::Ordering;
use std::collections::VecDeque;

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
    Literal(Literal<'source>),
    Identifier(Identifier<'source>),
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
        choose_result!(
            parser.optional_result(|parser| parser
                .expect_keyword(KeywordKind::Parent)
                .map(|_| Expression::Parent)),
            parser.optional_result(|parser| parser
                .expect_keyword(KeywordKind::Self_)
                .map(|_| Expression::Self_)),
        )
    });

    match parent_or_self_expression {
        Some(expression) => return Ok(expression),
        None => {}
    }

    let identifier =
        parser.with_node(|parser| Ok(Expression::Identifier(Identifier::parse(parser)?)))?;

    let function_call_arguments = parser.optional(|parser| {
        parser.expect_operator(OperatorKind::ParenthesisOpen)?;

        let arguments = parser.optional_separated(
            |parser| parser.parse_node::<FunctionArgument>(),
            OperatorKind::Comma,
            OperatorKind::ParenthesisClose,
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
    let token = parser.peek_token();
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
    let literal = parser.optional(|parser| Literal::parse(parser));
    match literal {
        Some(literal) => return Ok(Expression::Literal(literal)),
        None => {}
    }

    let mut expression = parser.with_node(parse_array_atom_expression)?;
    while parser.peek_token() == Some(&Token::Operator(OperatorKind::Access)) {
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

    match parser.peek_token() {
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
    match parser.peek_token() {
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
/// <expression> ::= <and expression> ('||' <and expression>)*
/// <and expression> ::= <bool expression> ('&&' <bool expression>)*
/// <bool expression> ::= <add expression> (<comparison operator> <add expression>)*
/// <add expression> ::= <mult expression> (('+' | '-') <mult expression>)*
/// <mult expression> ::= <unary expression> (('*' | '/' | '%') <unary expression>)*
/// ```
impl<'source> Parse<'source> for Expression<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let mut queue = VecDeque::<(Precedence, Node<Expression>)>::new();
        let mut first_expression: Option<Node<Expression>> = None;

        loop {
            if first_expression.is_none() {
                let expression = parser.with_node(parse_unary_expression)?;
                first_expression = Some(expression);
                continue;
            }

            let next_token = parser.peek_token();
            let precedence = match next_token {
                Some(token) => Precedence::of(token),
                None => None,
            };

            match precedence {
                Some(precedence) => {
                    // consume the previously peeked token
                    parser.consume()?;

                    let expression = parser.with_node(parse_unary_expression)?;
                    queue.push_back((precedence, expression));
                }
                None => {
                    if queue.is_empty() {
                        return Ok(first_expression.unwrap().into_inner());
                    }

                    break;
                }
            }
        }

        let mut expression = first_expression.unwrap();
        let mut is_expression_lhs = true;
        let mut aside_queue = VecDeque::<(Precedence, Node<Expression>)>::new();

        while let Some((precedence, other_expression)) = queue.pop_front() {
            let next_precedence = queue.front().map(|(precedence, _)| precedence);
            let next_precedence_order =
                next_precedence.map(|next_precedence| precedence.cmp(next_precedence));

            let set_is_expression_lhs;

            match next_precedence_order {
                Some(Ordering::Less) => {
                    // the current operator has a lesser precedence than the next
                    // we "set aside" the current operator and expression and deal
                    // with the more important one first

                    aside_queue.push_back((precedence, expression));
                    expression = other_expression;
                    continue;
                }
                _ => {
                    // when we pushed back the previous operator and expression into the
                    // "aside queue", we also changed the position of the expressions which we need
                    // to correct
                    set_is_expression_lhs = aside_queue.is_empty();

                    while let Some(tuple) = aside_queue.pop_front() {
                        queue.push_front(tuple);
                    }
                }
            };

            let (lhs, rhs) = match is_expression_lhs {
                true => (expression, other_expression),
                false => (other_expression, expression),
            };

            let range = lhs.range_union(&rhs);

            expression = match precedence {
                Precedence::Or | Precedence::And => {
                    let kind = match precedence {
                        Precedence::Or => LogicalKind::Or,
                        _ => LogicalKind::And,
                    };

                    Node::new(Expression::LogicalOperation { lhs, kind, rhs }, range)
                }
                Precedence::Comparison(kind) => {
                    Node::new(Expression::Comparison { lhs, kind, rhs }, range)
                }
                Precedence::Addition(kind) | Precedence::Multiplication(kind) => {
                    Node::new(Expression::Binary { lhs, kind, rhs }, range)
                }
            };

            is_expression_lhs = set_is_expression_lhs;
        }

        Ok(expression.into_inner())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Precedence {
    Or,
    And,
    Comparison(ComparisonKind),
    Addition(BinaryKind),
    Multiplication(BinaryKind),
}

impl Precedence {
    fn of(token: &Token) -> Option<Precedence> {
        match token {
            Token::Operator(operator) => match operator {
                OperatorKind::LogicalOr => Some(Precedence::Or),
                OperatorKind::LogicalAnd => Some(Precedence::And),
                OperatorKind::EqualTo => Some(Precedence::Comparison(ComparisonKind::EqualTo)),
                OperatorKind::NotEqualTo => {
                    Some(Precedence::Comparison(ComparisonKind::NotEqualTo))
                }
                OperatorKind::GreaterThan => {
                    Some(Precedence::Comparison(ComparisonKind::GreaterThan))
                }
                OperatorKind::LessThan => Some(Precedence::Comparison(ComparisonKind::LessThan)),
                OperatorKind::GreaterThanOrEqualTo => {
                    Some(Precedence::Comparison(ComparisonKind::GreaterThanOrEqualTo))
                }
                OperatorKind::LessThanOrEqualTo => {
                    Some(Precedence::Comparison(ComparisonKind::LessThanOrEqualTo))
                }
                OperatorKind::Addition => Some(Precedence::Addition(BinaryKind::Addition)),
                OperatorKind::Subtraction => Some(Precedence::Addition(BinaryKind::Subtraction)),
                OperatorKind::Multiplication => {
                    Some(Precedence::Multiplication(BinaryKind::Multiplication))
                }
                OperatorKind::Division => Some(Precedence::Multiplication(BinaryKind::Division)),
                OperatorKind::Modulus => Some(Precedence::Multiplication(BinaryKind::Modulus)),
                _ => None,
            },
            _ => None,
        }
    }

    fn value(&self) -> u8 {
        match self {
            Precedence::Or => 0,
            Precedence::And => 1,
            Precedence::Comparison(_) => 2,
            Precedence::Addition(_) => 3,
            Precedence::Multiplication(_) => 4,
        }
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let x = self.value();
        let y = other.value();
        Some(x.cmp(&y))
    }
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> Ordering {
        let x = self.value();
        let y = other.value();
        x.cmp(&y)
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
            ("none", Expression::Literal(Literal::None)),
            ("1", Expression::Literal(Literal::Integer(1))),
            ("1.0", Expression::Literal(Literal::Float(1.0))),
            ("false", Expression::Literal(Literal::Boolean(false))),
            (
                r#""Hello World!""#,
                Expression::Literal(Literal::String("Hello World!")),
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
                    name: Node::new(Expression::Identifier("MyFunc"), 0..6),
                    arguments: Some(vec![
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Identifier("someArgument"),
                                7..19,
                            )),
                            7..19,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Identifier("anotherArgument"),
                                21..36,
                            )),
                            21..36,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Literal::Integer(1)),
                                38..39,
                            )),
                            38..39,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Literal::Float(1.0)),
                                41..44,
                            )),
                            41..44,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Literal::Boolean(false)),
                                46..51,
                            )),
                            46..51,
                        ),
                        Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Literal(Literal::String("Hi!")),
                                53..58,
                            )),
                            53..58,
                        ),
                        Node::new(
                            FunctionArgument::Named {
                                name: Node::new("name", 60..64),
                                value: Node::new(Expression::Literal(Literal::None), 67..71),
                            },
                            60..71,
                        ),
                    ]),
                },
            ),
            (
                "MyFunc()",
                Expression::FunctionCall {
                    name: Node::new(Expression::Identifier("MyFunc"), 0..6),
                    arguments: None,
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_nested_parentheses() {
        let src = r#"(((((((((("Help I'm Stuck!"))))))))))"#;
        let expected = Expression::Literal(Literal::String("Help I'm Stuck!"));

        run_test(src, expected);
    }

    #[test]
    fn test_new_array_expression() {
        let src = "new int[100]";
        let expected = Expression::NewArray {
            element_type: Node::new(TypeName::BaseType(BaseType::Int), 4..7),
            size: Node::new(Expression::Literal(Literal::Integer(100)), 8..11),
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
            array: Node::new(Expression::Identifier("myCoolArray"), 0..11),
            index: Node::new(Expression::Literal(Literal::Integer(10)), 12..14),
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
                            lhs: Node::new(Expression::Identifier("MyObject"), 0..8),
                            rhs: Node::new(Expression::Identifier("AnotherObject"), 9..22),
                        },
                        0..22,
                    ),
                    rhs: Node::new(Expression::Identifier("MyProperty"), 23..33),
                },
            ),
            (
                "MyObject.MyProperty",
                Expression::MemberAccess {
                    lhs: Node::new(Expression::Identifier("MyObject"), 0..8),
                    rhs: Node::new(Expression::Identifier("MyProperty"), 9..19),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_cast_expression() {
        let src = "x as int";
        let expected = Expression::Cast {
            lhs: Node::new(Expression::Identifier("x"), 0..1),
            rhs: Node::new(TypeName::BaseType(BaseType::Int), 5..8),
        };

        run_test(src, expected);
    }

    #[test]
    fn test_type_check_expression() {
        let src = "x is int";
        let expected = Expression::TypeCheck {
            lhs: Node::new(Expression::Identifier("x"), 0..1),
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
                    rhs: Node::new(Expression::Identifier("x"), 1..2),
                },
            ),
            (
                "-x",
                Expression::Unary {
                    kind: UnaryKind::Negative,
                    rhs: Node::new(Expression::Identifier("x"), 1..2),
                },
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_binary_expression() {
        let lhs = Node::new(Expression::Identifier("x"), 0..1);
        let rhs = Node::new(Expression::Identifier("y"), 4..5);

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
        let lhs = Node::new(Expression::Identifier("x"), 0..1);

        let single_rhs = Node::new(Expression::Identifier("y"), 4..5);

        let double_rhs = Node::new(Expression::Identifier("y"), 5..6);

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
        let lhs = Node::new(Expression::Identifier("x"), 0..1);
        let rhs = Node::new(Expression::Identifier("y"), 5..6);

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
            (
                "false || true && true",
                Expression::LogicalOperation {
                    lhs: Node::new(Expression::Literal(Literal::Boolean(false)), 0..5),
                    kind: LogicalKind::Or,
                    rhs: Node::new(
                        Expression::LogicalOperation {
                            lhs: Node::new(Expression::Literal(Literal::Boolean(true)), 9..13),
                            kind: LogicalKind::And,
                            rhs: Node::new(Expression::Literal(Literal::Boolean(true)), 17..21),
                        },
                        9..21,
                    ),
                },
            ),
            (
                "true && true || false",
                Expression::LogicalOperation {
                    lhs: Node::new(
                        Expression::LogicalOperation {
                            lhs: Node::new(Expression::Literal(Literal::Boolean(true)), 0..4),
                            kind: LogicalKind::And,
                            rhs: Node::new(Expression::Literal(Literal::Boolean(true)), 8..12),
                        },
                        0..12,
                    ),
                    kind: LogicalKind::Or,
                    rhs: Node::new(Expression::Literal(Literal::Boolean(false)), 16..21),
                },
            ),
            (
                "true && true || false && true || false",
                Expression::LogicalOperation {
                    lhs: Node::new(
                        Expression::LogicalOperation {
                            lhs: Node::new(
                                Expression::LogicalOperation {
                                    lhs: Node::new(
                                        Expression::Literal(Literal::Boolean(true)),
                                        0..4,
                                    ),
                                    kind: LogicalKind::And,
                                    rhs: Node::new(
                                        Expression::Literal(Literal::Boolean(true)),
                                        8..12,
                                    ),
                                },
                                0..12,
                            ),
                            kind: LogicalKind::Or,
                            rhs: Node::new(
                                Expression::LogicalOperation {
                                    lhs: Node::new(
                                        Expression::Literal(Literal::Boolean(false)),
                                        16..21,
                                    ),
                                    kind: LogicalKind::And,
                                    rhs: Node::new(
                                        Expression::Literal(Literal::Boolean(true)),
                                        25..29,
                                    ),
                                },
                                16..29,
                            ),
                        },
                        0..29,
                    ),
                    kind: LogicalKind::Or,
                    rhs: Node::new(Expression::Literal(Literal::Boolean(false)), 33..38),
                },
            ),
        ];

        run_tests(data);
    }
}
