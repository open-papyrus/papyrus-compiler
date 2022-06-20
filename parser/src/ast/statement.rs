use crate::ast::expression::{expression_parser, Expression};
use crate::ast::identifier::Identifier;
use crate::ast::node::{display_nodes, display_optional_nodes, Node};
use crate::ast::types::{type_with_identifier_parser, Type};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    /// 'int x = 1'
    VariableDefinition {
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        expression: Option<Node<Expression<'a>>>,
    },
    /// 'Return x'
    Return(Option<Node<Expression<'a>>>),
    /// 'x = y'
    Assignment {
        lhs: Node<Expression<'a>>,
        kind: Node<AssignmentKind>,
        rhs: Node<Expression<'a>>,
    },
    If {
        if_path: Node<ConditionalPath<'a>>,
        other_paths: Option<Vec<Node<ConditionalPath<'a>>>>,
        else_path: Option<Vec<Node<Statement<'a>>>>,
    },
    While(Node<ConditionalPath<'a>>),
    Expression(Node<Expression<'a>>),
}

pub fn display_statements<'a>(
    statements: &Option<Vec<Node<Statement<'a>>>>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    match statements.as_ref() {
        Some(statements) => {
            for statement in statements {
                write!(f, "\n{}", statement)?;
            }
        }
        None => {}
    };

    Ok(())
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::VariableDefinition {
                type_node,
                name,
                expression,
            } => match expression.as_ref() {
                Some(expression) => write!(f, "{} {} = {}", type_node, name, expression),
                None => write!(f, "{} {}", type_node, name),
            },
            Statement::Return(expression) => match expression.as_ref() {
                Some(expression) => write!(f, "Return {}", expression),
                None => write!(f, "Return"),
            },
            Statement::Assignment { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
            Statement::If {
                if_path,
                other_paths,
                else_path,
            } => {
                write!(f, "If {}", if_path)?;

                display_optional_nodes(other_paths, "\nElseIf ", f)?;

                match else_path.as_ref() {
                    Some(else_path) => {
                        write!(f, "\nElse")?;
                        display_nodes(else_path, "\n", f)?;
                    }
                    None => {}
                }

                write!(f, "\nEndIf")?;

                Ok(())
            }
            Statement::While(path) => write!(f, "While {}\nEndWhile", path),
            Statement::Expression(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConditionalPath<'a> {
    pub condition: Node<Expression<'a>>,
    pub statements: Option<Vec<Node<Statement<'a>>>>,
}

impl<'a> ConditionalPath<'a> {
    pub fn new(
        condition: Node<Expression<'a>>,
        statements: Option<Vec<Node<Statement<'a>>>>,
    ) -> Self {
        Self {
            condition,
            statements,
        }
    }
}

impl<'a> Display for ConditionalPath<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.condition)?;

        display_optional_nodes(&self.statements, "\n", f)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AssignmentKind {
    /// '='
    Normal,
    /// '+='
    Addition,
    /// '-='
    Subtraction,
    /// '*='
    Multiplication,
    /// '/='
    Division,
    /// '%='
    Modulus,
}

impl Display for AssignmentKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignmentKind::Normal => write!(f, "="),
            AssignmentKind::Addition => write!(f, "+="),
            AssignmentKind::Subtraction => write!(f, "-="),
            AssignmentKind::Multiplication => write!(f, "*="),
            AssignmentKind::Division => write!(f, "/="),
            AssignmentKind::Modulus => write!(f, "%="),
        }
    }
}

pub fn assignment_kind_parser<'a>() -> impl TokenParser<'a, AssignmentKind> {
    select! {
        Token::Operator(OperatorKind::Assignment) => AssignmentKind::Normal,
        Token::Operator(OperatorKind::AdditionAssignment) => AssignmentKind::Addition,
        Token::Operator(OperatorKind::SubtractionAssignment) => AssignmentKind::Subtraction,
        Token::Operator(OperatorKind::MultiplicationAssignment) => AssignmentKind::Multiplication,
        Token::Operator(OperatorKind::DivisionAssignment) => AssignmentKind::Division,
        Token::Operator(OperatorKind::ModulusAssignment) => AssignmentKind::Modulus,
    }
}

/// ```ebnf
/// <define statement> ::= <type> <identifier> ['=' <expression>]
///
/// <assign statement> ::= (<l-value> '=' <expression>) |
///                        (<l-value> '+=' <expression>) |
///                        (<l-value> '-=' <expression>) |
///                        (<l-value> '*=' <expression>) |
///                        (<l-value> '/=' <expression>) |
///                        (<l-value> '%=' <expression>)
/// <l-value>          ::= ([<expression> '.'] <identifier>) |
///                        (<expression> '[' <expression> ']')
///
/// 'Return' [<expression>]
///
/// <if statement> ::= 'If' '(' <expression> ')'
///                      <statement>*
///                    ['ElseIf' '(' <expression> ')'
///                      <statement>*]*
///                    ['Else'
///                      <statement>*]
///                    'EndIf'
///
/// 'While' '(' <expression> ')'
///   <statement>*
/// 'EndWhile'
/// ```
pub fn statement_parser<'a>() -> impl TokenParser<'a, Statement<'a>> {
    recursive(|statement| {
        let define_statement = type_with_identifier_parser()
            .then(
                just(Token::Operator(OperatorKind::Assignment))
                    .ignore_then(expression_parser().map_with_span(Node::new))
                    .or_not(),
            )
            .map(|output| {
                let ((type_node, identifier), initial_value) = output;
                Statement::VariableDefinition {
                    type_node,
                    name: identifier,
                    expression: initial_value,
                }
            });

        let assignment_statement = expression_parser()
            .map_with_span(Node::new)
            .then(assignment_kind_parser().map_with_span(Node::new))
            .then(expression_parser().map_with_span(Node::new))
            .map(|output| {
                let ((lhs, kind), rhs) = output;
                Statement::Assignment { lhs, kind, rhs }
            });

        let return_statement = just(Token::Keyword(KeywordKind::Return))
            .ignore_then(expression_parser().map_with_span(Node::new).or_not())
            .map(Statement::Return);

        let conditional_path = expression_parser()
            .map_with_span(Node::new)
            .then(
                statement
                    .clone()
                    .map_with_span(Node::new)
                    .repeated()
                    .at_least(1)
                    .or_not(),
            )
            .map(|output| {
                let (condition, statements) = output;
                ConditionalPath::new(condition, statements)
            })
            .map_with_span(Node::new);

        let if_statement = just(Token::Keyword(KeywordKind::If))
            .ignore_then(conditional_path.clone())
            .then(
                just(Token::Keyword(KeywordKind::ElseIf))
                    .ignore_then(conditional_path.clone())
                    .repeated()
                    .at_least(1)
                    .or_not(),
            )
            .then(
                just(Token::Keyword(KeywordKind::Else))
                    .ignore_then(
                        statement
                            .clone()
                            .map_with_span(Node::new)
                            .repeated()
                            .at_least(1)
                            .or_not(),
                    )
                    .or_not()
                    .map(|else_statements| match else_statements {
                        Some(else_statements) => else_statements,
                        None => None,
                    }),
            )
            .then_ignore(just(Token::Keyword(KeywordKind::EndIf)))
            .map(|output| {
                let ((if_path, other_paths), else_path) = output;
                Statement::If {
                    if_path,
                    other_paths,
                    else_path,
                }
            });

        let while_statement = just(Token::Keyword(KeywordKind::While))
            .ignore_then(conditional_path.clone())
            .then_ignore(just(Token::Keyword(KeywordKind::EndWhile)))
            .map(Statement::While);

        let expression_statement = expression_parser()
            .map_with_span(Node::new)
            .map(Statement::Expression);

        choice((
            define_statement,
            assignment_statement,
            return_statement,
            if_statement,
            while_statement,
            expression_statement,
        ))
    })
}

#[cfg(test)]
mod test {
    use crate::ast::expression::{ComparisonKind, Expression, FunctionArgument};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::statement::{statement_parser, AssignmentKind, ConditionalPath, Statement};
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parse::test_utils::{run_test, run_tests};

    #[test]
    fn test_define_statement() {
        let src = "int x = 1";
        let expected = Statement::VariableDefinition {
            type_node: Node::new(
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
                0..3,
            ),
            name: Node::new("x", 4..5),
            expression: Some(Node::new(
                Expression::Literal(Node::new(Literal::Integer(1), 8..9)),
                8..9,
            )),
        };

        run_test(src, expected, statement_parser);
    }

    #[test]
    fn test_return_statement() {
        let data = vec![
            ("Return", Statement::Return(None)),
            (
                "Return 1",
                Statement::Return(Some(Node::new(
                    Expression::Literal(Node::new(Literal::Integer(1), 7..8)),
                    7..8,
                ))),
            ),
        ];

        run_tests(data, statement_parser);
    }

    #[test]
    fn test_assignment_statement() {
        let lhs = Node::new(Expression::Identifier(Node::new("x", 0..1)), 0..1);

        let rhs = Node::new(
            Expression::Literal(Node::new(Literal::Integer(1), 5..6)),
            5..6,
        );

        let data = vec![
            (
                "x = 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Normal, 2..3),
                    rhs: Node::new(
                        Expression::Literal(Node::new(Literal::Integer(1), 4..5)),
                        4..5,
                    ),
                },
            ),
            (
                "x += 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Addition, 2..4),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x -= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Subtraction, 2..4),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x *= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Multiplication, 2..4),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x /= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Division, 2..4),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x %= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Modulus, 2..4),
                    rhs: rhs.clone(),
                },
            ),
            (
                "Pages[i] = Pages[j]",
                Statement::Assignment {
                    lhs: Node::new(
                        Expression::ArrayAccess {
                            array: Node::new(
                                Expression::Identifier(Node::new("Pages", 0..5)),
                                0..5,
                            ),
                            index: Node::new(Expression::Identifier(Node::new("i", 6..7)), 6..7),
                        },
                        0..8,
                    ),
                    kind: Node::new(AssignmentKind::Normal, 9..10),
                    rhs: Node::new(
                        Expression::ArrayAccess {
                            array: Node::new(
                                Expression::Identifier(Node::new("Pages", 11..16)),
                                11..16,
                            ),
                            index: Node::new(
                                Expression::Identifier(Node::new("j", 17..18)),
                                17..18,
                            ),
                        },
                        11..19,
                    ),
                },
            ),
        ];

        run_tests(data, statement_parser);
    }

    #[test]
    fn test_if_statement() {
        let data = vec![
            (
                "If (true) EndIf",
                Statement::If {
                    if_path: Node::new(
                        ConditionalPath {
                            condition: Node::new(
                                Expression::Literal(Node::new(Literal::Boolean(true), 4..8)),
                                3..9,
                            ),
                            statements: None,
                        },
                        3..9,
                    ),
                    other_paths: None,
                    else_path: None,
                },
            ),
            (
                "If (true) ElseIf (true) ElseIf (false) Else EndIf",
                Statement::If {
                    if_path: Node::new(
                        ConditionalPath {
                            condition: Node::new(
                                Expression::Literal(Node::new(Literal::Boolean(true), 4..8)),
                                3..9,
                            ),
                            statements: None,
                        },
                        3..9,
                    ),
                    other_paths: Some(vec![
                        Node::new(
                            ConditionalPath {
                                condition: Node::new(
                                    Expression::Literal(Node::new(Literal::Boolean(true), 18..22)),
                                    17..23,
                                ),
                                statements: None,
                            },
                            17..23,
                        ),
                        Node::new(
                            ConditionalPath {
                                condition: Node::new(
                                    Expression::Literal(Node::new(Literal::Boolean(false), 32..37)),
                                    31..38,
                                ),
                                statements: None,
                            },
                            31..38,
                        ),
                    ]),
                    else_path: None,
                },
            ),
            (
                r#"if x == 0
    Return y
elseif x == 1
    Return y
else
    Return y
endif"#,
                Statement::If {
                    if_path: Node::new(
                        ConditionalPath::new(
                            Node::new(
                                Expression::Comparison {
                                    lhs: Node::new(
                                        Expression::Identifier(Node::new("x", 3..4)),
                                        3..4,
                                    ),
                                    kind: Node::new(ComparisonKind::EqualTo, 5..7),
                                    rhs: Node::new(
                                        Expression::Literal(Node::new(Literal::Integer(0), 8..9)),
                                        8..9,
                                    ),
                                },
                                3..9,
                            ),
                            Some(vec![Node::new(
                                Statement::Return(Some(Node::new(
                                    Expression::Identifier(Node::new("y", 21..22)),
                                    21..22,
                                ))),
                                14..22,
                            )]),
                        ),
                        3..22,
                    ),
                    other_paths: Some(vec![Node::new(
                        ConditionalPath::new(
                            Node::new(
                                Expression::Comparison {
                                    lhs: Node::new(
                                        Expression::Identifier(Node::new("x", 30..31)),
                                        30..31,
                                    ),
                                    kind: Node::new(ComparisonKind::EqualTo, 32..34),
                                    rhs: Node::new(
                                        Expression::Literal(Node::new(Literal::Integer(1), 35..36)),
                                        35..36,
                                    ),
                                },
                                30..36,
                            ),
                            Some(vec![Node::new(
                                Statement::Return(Some(Node::new(
                                    Expression::Identifier(Node::new("y", 48..49)),
                                    48..49,
                                ))),
                                41..49,
                            )]),
                        ),
                        30..49,
                    )]),
                    else_path: Some(vec![Node::new(
                        Statement::Return(Some(Node::new(
                            Expression::Identifier(Node::new("y", 66..67)),
                            66..67,
                        ))),
                        59..67,
                    )]),
                },
            ),
        ];

        run_tests(data, statement_parser);
    }

    #[test]
    fn test_while_statement() {
        let src = "While (true) EndWhile";
        let expected = Statement::While(Node::new(
            ConditionalPath {
                condition: Node::new(
                    Expression::Literal(Node::new(Literal::Boolean(true), 7..11)),
                    6..12,
                ),
                statements: None,
            },
            6..12,
        ));

        run_test(src, expected, statement_parser);
    }

    #[test]
    fn test_expression_statement() {
        let src = "Debug.Trace(msg)";
        let expected = Statement::Expression(Node::new(
            Expression::MemberAccess {
                lhs: Node::new(Expression::Identifier(Node::new("Debug", 0..5)), 0..5),
                rhs: Node::new(
                    Expression::FunctionCall {
                        name: Node::new(Expression::Identifier(Node::new("Trace", 6..11)), 6..11),
                        arguments: Some(vec![Node::new(
                            FunctionArgument::Positional(Node::new(
                                Expression::Identifier(Node::new("msg", 12..15)),
                                12..15,
                            )),
                            12..15,
                        )]),
                    },
                    6..16,
                ),
            },
            0..16,
        ));

        run_test(src, expected, statement_parser);
    }
}
