use crate::ast::expression::{expression_parser, Expression};
use crate::ast::identifier::{identifier_parser, Identifier};
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

        let l_value = expression_parser()
            .map_with_span(Node::new)
            .then_ignore(just(Token::Operator(OperatorKind::Access)))
            .or_not()
            .then(identifier_parser().map_with_span(Node::new))
            .map(|(expression, identifier)| {
                let identifier_span = identifier.span();
                let identifier = Expression::Identifier(identifier);

                match expression {
                    Some(lhs) => Expression::Access {
                        lhs,
                        rhs: Node::new(identifier, identifier_span),
                    },
                    None => identifier,
                }
            })
            .or(expression_parser()
                .map_with_span(Node::new)
                .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsOpen)))
                .then(expression_parser().map_with_span(Node::new))
                .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
                .map(|output| {
                    let (array, index) = output;
                    Expression::ArrayAccess { array, index }
                }))
            .boxed();

        let assignment_statement = l_value
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

        let conditional_path = just(Token::Operator(OperatorKind::ParenthesisOpen))
            .ignore_then(expression_parser().map_with_span(Node::new))
            .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
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

        choice((
            define_statement,
            assignment_statement,
            return_statement,
            if_statement,
            while_statement,
        ))
    })
}

#[cfg(test)]
mod test {
    use crate::ast::expression::Expression;
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
                Type::new(
                    Node::new(TypeName::BaseType(BaseType::Int), (0..3).into()),
                    false,
                ),
                (0..3).into(),
            ),
            name: Node::new("x", (4..5).into()),
            expression: Some(Node::new(
                Expression::Literal(Node::new(Literal::Integer(1), (8..9).into())),
                (8..9).into(),
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
                    Expression::Literal(Node::new(Literal::Integer(1), (7..8).into())),
                    (7..8).into(),
                ))),
            ),
        ];

        run_tests(data, statement_parser);
    }

    #[test]
    fn test_assignment_statement() {
        let lhs = Node::new(
            Expression::Identifier(Node::new("x", (0..1).into())),
            (0..1).into(),
        );

        let rhs = Node::new(
            Expression::Literal(Node::new(Literal::Integer(1), (5..6).into())),
            (5..6).into(),
        );

        let data = vec![
            (
                "x = 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Normal, (2..3).into()),
                    rhs: Node::new(
                        Expression::Literal(Node::new(Literal::Integer(1), (4..5).into())),
                        (4..5).into(),
                    ),
                },
            ),
            (
                "x += 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Addition, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x -= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Subtraction, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x *= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Multiplication, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x /= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Division, (2..4).into()),
                    rhs: rhs.clone(),
                },
            ),
            (
                "x %= 1",
                Statement::Assignment {
                    lhs: lhs.clone(),
                    kind: Node::new(AssignmentKind::Modulus, (2..4).into()),
                    rhs: rhs.clone(),
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
                                Expression::Literal(Node::new(
                                    Literal::Boolean(true),
                                    (4..8).into(),
                                )),
                                (4..8).into(),
                            ),
                            statements: None,
                        },
                        (3..9).into(),
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
                                Expression::Literal(Node::new(
                                    Literal::Boolean(true),
                                    (4..8).into(),
                                )),
                                (4..8).into(),
                            ),
                            statements: None,
                        },
                        (3..9).into(),
                    ),
                    other_paths: Some(vec![
                        Node::new(
                            ConditionalPath {
                                condition: Node::new(
                                    Expression::Literal(Node::new(
                                        Literal::Boolean(true),
                                        (18..22).into(),
                                    )),
                                    (18..22).into(),
                                ),
                                statements: None,
                            },
                            (17..23).into(),
                        ),
                        Node::new(
                            ConditionalPath {
                                condition: Node::new(
                                    Expression::Literal(Node::new(
                                        Literal::Boolean(false),
                                        (32..37).into(),
                                    )),
                                    (32..37).into(),
                                ),
                                statements: None,
                            },
                            (31..38).into(),
                        ),
                    ]),
                    else_path: None,
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
                    Expression::Literal(Node::new(Literal::Boolean(true), (7..11).into())),
                    (7..11).into(),
                ),
                statements: None,
            },
            (6..12).into(),
        ));

        run_test(src, expected, statement_parser);
    }
}
