use crate::ast::node::{range_union, Node};
use papyrus_compiler_diagnostics::SourceRange;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum ParserError<'source> {
    ExpectedNodeWithName {
        name: &'static str,
        found: Token<'source>,
    },
    ExpectedOne {
        expected: Token<'static>,
        found: Token<'source>,
    },
    ExpectedOneOf {
        expected: Vec<Token<'static>>,
        found: Token<'source>,
    },
    ExpectedEOI {
        found: Token<'source>,
    },
    EOI,
}

impl<'source> Display for ParserError<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'source> std::error::Error for ParserError<'source> {}

pub(crate) type ParserResult<'source, TOk> = Result<TOk, ParserError<'source>>;

pub(crate) struct Parser<'source> {
    tokens: Vec<(Token<'source>, SourceRange)>,
    position: usize,
    eoi_range: Option<SourceRange>,
}

impl<'source> Parser<'source> {
    pub fn new(tokens: Vec<(Token<'source>, SourceRange)>) -> Self {
        let eoi_range = tokens.last().map(|(_, range)| range.clone());

        Self {
            tokens,
            position: 0,
            eoi_range,
        }
    }

    fn save_range(&self, position: usize) -> ParserResult<'source, SourceRange> {
        if position >= self.tokens.len() {
            match self.eoi_range.as_ref() {
                Some(range) => Ok(range.clone()),
                None => Err(ParserError::EOI),
            }
        } else {
            match self.tokens.get(position) {
                Some((_, range)) => Ok(range.clone()),
                None => Err(ParserError::EOI),
            }
        }
    }

    /// Peek at the next available Token, this function does not consume.
    pub fn peek(&self) -> Option<&Token<'source>> {
        if self.position < self.tokens.len() {
            self.tokens.get(self.position).map(|(token, _)| token)
        } else {
            None
        }
    }

    pub fn peek_result(&self) -> ParserResult<'source, &Token<'source>> {
        self.peek().ok_or(ParserError::EOI)
    }

    /// Consume the next Token.
    pub fn consume(&mut self) -> ParserResult<'source, &Token<'source>> {
        if self.position < self.tokens.len() {
            let element = self.tokens.get(self.position);
            self.position += 1;
            match element {
                Some((token, _)) => Ok(token),
                None => Err(ParserError::EOI),
            }
        } else {
            Err(ParserError::EOI)
        }
    }

    /// Consume the next Token and compare it with the expected Token,
    /// returning a [`ParserError`] if it does not match.
    pub fn expect(&mut self, expected: Token<'static>) -> ParserResult<'source, &Token<'source>> {
        let found = self.consume()?;
        if found == &expected {
            Ok(found)
        } else {
            Err(ParserError::ExpectedOne {
                found: *found,
                expected,
            })
        }
    }

    pub fn expect_keyword(
        &mut self,
        keyword: KeywordKind,
    ) -> ParserResult<'source, &Token<'source>> {
        self.expect(Token::Keyword(keyword))
    }

    pub fn expect_operator(
        &mut self,
        operator: OperatorKind,
    ) -> ParserResult<'source, &Token<'source>> {
        self.expect(Token::Operator(operator))
    }

    /// Expect the EOI, return a [`ParserError::ExpectedEOI`] if it's not the end.
    pub fn expect_eoi(&self) -> ParserResult<()> {
        match self.peek() {
            Some(found) => Err(ParserError::ExpectedEOI { found: *found }),
            None => Ok(()),
        }
    }

    /// Repeatedly call the provided function until it returns a [`ParserError`].
    pub fn repeated<O, F>(&mut self, mut f: F) -> ParserResult<'source, Vec<O>>
    where
        F: FnMut(&mut Self) -> ParserResult<'source, O>,
    {
        self.separated(f, None)
    }

    /// Similar to [`repeated`] except the parsed outputs must be seperated by an operator.
    pub fn separated<O, F>(
        &mut self,
        mut f: F,
        separator: Option<OperatorKind>,
    ) -> ParserResult<'source, Vec<O>>
    where
        F: FnMut(&mut Self) -> ParserResult<'source, O>,
    {
        let mut results = vec![];
        let mut last_valid_position = self.position;

        loop {
            // only parse the separator after the first element, eg:
            // <elem_1> <sep> <elem_2>
            let separator_result = match separator {
                Some(separator) if !results.is_empty() => {
                    self.expect_operator(separator).map(|_| ())
                }
                _ => Ok(()),
            };

            let res = f(self);
            match res {
                Ok(res) => {
                    // only evaluate the result of the separator parsing once we made sure
                    // that the next element is valid. This is required to end the sequence because
                    // the first invalid element will mark the end of the sequence, it can't start
                    // with the separator
                    separator_result?;

                    results.push(res);
                    last_valid_position = self.position;
                }
                Err(err) => {
                    return if results.is_empty() {
                        Err(err)
                    } else {
                        self.position = last_valid_position;
                        Ok(results)
                    }
                }
            };
        }
    }

    /// Parses `O` and creates a new [`Node`].
    pub fn parse_node<O>(&mut self) -> ParserResult<'source, Node<O>>
    where
        O: Parse<'source>,
    {
        let start_range = self.save_range(self.position)?;
        let res = O::parse(self)?;
        let end_range = self.save_range(self.position - 1)?;

        Ok(Node::new(res, range_union(start_range, end_range)))
    }

    /// Parses `O` repeatedly and creates a new [`Node`].
    pub fn parse_node_repeated<O>(&mut self) -> ParserResult<'source, Vec<Node<O>>>
    where
        O: Parse<'source>,
    {
        self.repeated(|parser| parser.parse_node::<O>())
    }

    /// Call the provided function but reset the position if the function was not successful.
    pub fn optional<O, F>(&mut self, f: F) -> Option<O>
    where
        F: FnOnce(&mut Self) -> ParserResult<'source, O>,
    {
        let start_position = self.position;
        let res = f(self);

        match res {
            Ok(value) => Some(value),
            Err(_) => {
                self.position = start_position;
                None
            }
        }
    }

    /// Parses `O` optionally and creates a new [`Node`].
    pub fn parse_node_optional<O>(&mut self) -> Option<Node<O>>
    where
        O: Parse<'source>,
    {
        self.optional(|parser| parser.parse_node::<O>())
    }

    /// Pares `O` repeatedly but optional.
    pub fn parse_node_optional_repeated<O>(&mut self) -> Option<Vec<Node<O>>>
    where
        O: Parse<'source>,
    {
        self.optional(|parser| parser.parse_node_repeated::<O>())
    }
}

pub(crate) trait Parse<'source>
where
    Self: Sized,
{
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self>;
}

#[macro_export]
macro_rules! select_tokens {
    ($parser:ident, $name:literal, $( $pattern:pat_param => $out:expr ),+ $(,)?) => {{
        let token = $parser.consume()?;

        match token {
            $( $pattern => ::core::result::Result::Ok($out) ),+,
            _ => ::core::result::Result::Err($crate::parser::ParserError::ExpectedNodeWithName {
                name: $name,
                found: *token
            }),
        }
    }}
}

#[macro_export]
macro_rules! choose_optional {
    ($parser: ident, $name:literal, $( $item:expr ),+ $(,)? ) => {{
        $( if let Some(res) = $item {
            return ::core::result::Result::Ok(res);
        } )*

        ::core::result::Result::Err($crate::parser::ParserError::ExpectedNodeWithName {
            name: $name,
            found: *$parser.peek_result()?
        })
    }}
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::parser::{Parse, Parser};
    use std::fmt::Debug;

    pub(crate) fn run_test<'source, O>(src: &'source str, expected: O)
    where
        O: Parse<'source> + PartialEq + Debug,
    {
        let tokens = papyrus_compiler_lexer::run_lexer(src);
        let mut parser = Parser::new(tokens);

        let res = O::parse(&mut parser).unwrap();
        parser.expect_eoi().unwrap();

        assert_eq!(res, expected, "{}", src);
    }

    pub(crate) fn run_tests<'source, O>(data: Vec<(&'static str, O)>)
    where
        O: Parse<'source> + PartialEq + Debug,
    {
        for (src, expected) in data {
            run_test(src, expected);
        }
    }
}
