use crate::ast::node::{range_union, Node};
use papyrus_compiler_diagnostics::SourceRange;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

type TokenWithRange<'source> = (Token<'source>, SourceRange);

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError<'source> {
    ExpectedToken {
        expected: Token<'static>,
        found: TokenWithRange<'source>,
    },
    UnexpectedEOI,
    AggregatedErrors(HashSet<ParserError<'source>>),
    ExpectedEOI {
        found: TokenWithRange<'source>,
    },
}

impl<'source> Display for ParserError<'source> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'source> std::error::Error for ParserError<'source> {}

impl<'source> Hash for ParserError<'source> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ParserError::ExpectedToken { expected, found } => {
                state.write_i8(1);
                expected.hash(state);
                found.hash(state)
            }
            ParserError::UnexpectedEOI => state.write_i8(2),
            ParserError::AggregatedErrors(_) => state.write_i8(3),
            ParserError::ExpectedEOI { .. } => state.write_i8(4),
        }
    }
}

impl<'source> Eq for ParserError<'source> {}

pub type ParserResult<'source, TOk> = Result<TOk, ParserError<'source>>;

fn flatten_error<'source>(error: ParserError<'source>) -> ParserError<'source> {
    match error {
        ParserError::AggregatedErrors(errors) => {
            let res =
                errors
                    .into_iter()
                    .fold(HashSet::<ParserError<'source>>::new(), |mut set, err| {
                        let err = flatten_error(err);
                        match err {
                            ParserError::AggregatedErrors(errors) => set.extend(errors),
                            _ => {
                                let _ = set.insert(err);
                            }
                        };

                        set
                    });

            if res.len() == 1 {
                res.into_iter().next().unwrap()
            } else {
                ParserError::AggregatedErrors(res)
            }
        }
        _ => error,
    }
}

pub fn flatten_result<TOk>(result: ParserResult<TOk>) -> ParserResult<TOk> {
    match result {
        Ok(value) => Ok(value),
        Err(err) => Err(flatten_error(err)),
    }
}

pub struct Parser<'source> {
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

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn save_range(&self, position: usize) -> ParserResult<'source, SourceRange> {
        if position >= self.tokens.len() {
            match self.eoi_range.as_ref() {
                Some(range) => Ok(range.clone()),
                None => Err(ParserError::UnexpectedEOI),
            }
        } else {
            match self.tokens.get(position) {
                Some((_, range)) => Ok(range.clone()),
                None => Err(ParserError::UnexpectedEOI),
            }
        }
    }

    /// Peek at the next available Token, this function does not consume and also returns the
    /// [`SourceRange`] of the Token.
    pub fn peek(&self) -> Option<&TokenWithRange<'source>> {
        if self.position < self.tokens.len() {
            self.tokens.get(self.position)
        } else {
            None
        }
    }

    /// Peek at the next available Token, this function does not consume and is a wrapper around
    /// [`Parser::peek`].
    pub fn peek_token(&self) -> Option<&Token<'source>> {
        self.peek().map(|(token, _)| token)
    }

    /// Consume the next Token.
    pub fn consume(&mut self) -> ParserResult<'source, &TokenWithRange<'source>> {
        if self.position < self.tokens.len() {
            let element = self.tokens.get(self.position);
            self.position += 1;
            match element {
                Some(element) => Ok(element),
                None => Err(ParserError::UnexpectedEOI),
            }
        } else {
            Err(ParserError::UnexpectedEOI)
        }
    }

    /// Consume the next Token and compare it with the expected Token,
    /// returns a [`ParserError`] if it does not match.
    pub fn expect(&mut self, expected: Token<'static>) -> ParserResult<'source, ()> {
        let (token, range) = self.consume()?;
        if token == &expected {
            Ok(())
        } else {
            Err(ParserError::ExpectedToken {
                found: (*token, range.clone()),
                expected,
            })
        }
    }

    /// Expect a Keyword. This is a wrapper around [`Parser::expect`].
    pub fn expect_keyword(&mut self, keyword: KeywordKind) -> ParserResult<'source, ()> {
        self.expect(Token::Keyword(keyword))
    }

    /// Expect an Operator. This is a wrapper around [`Parser::expect`].
    pub fn expect_operator(&mut self, operator: OperatorKind) -> ParserResult<'source, ()> {
        self.expect(Token::Operator(operator))
    }

    /// Expect the EOI, returns a [`ParserError`] if it's not the end.
    pub fn expect_eoi(&self) -> ParserResult<()> {
        match self.peek() {
            Some((token, range)) => Err(ParserError::ExpectedEOI {
                found: (*token, range.clone()),
            }),
            None => Ok(()),
        }
    }

    /// Repeatedly calls the provided function until it returns a [`ParserError`].
    pub fn repeated<O, F>(&mut self, f: F) -> ParserResult<'source, Vec<O>>
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

    /// Calls the provided function and puts the result in a [`Node`].
    pub fn with_node<O, F>(&mut self, f: F) -> ParserResult<'source, Node<O>>
    where
        F: FnOnce(&mut Self) -> ParserResult<'source, O>,
    {
        let start_range = self.save_range(self.position)?;
        let res = f(self)?;
        let end_range = self.save_range(self.position - 1)?;

        Ok(Node::new(res, range_union(start_range, end_range)))
    }

    /// Calls the provided function and resets the position if the function was not successful.
    pub fn optional<O, F>(&mut self, f: F) -> Option<O>
    where
        F: FnOnce(&mut Self) -> ParserResult<'source, O>,
    {
        self.optional_result(f).ok()
    }

    /// Calls the provided function and resets the position if the function was not successful.
    pub fn optional_result<O, F>(&mut self, f: F) -> ParserResult<'source, O>
    where
        F: FnOnce(&mut Self) -> ParserResult<'source, O>,
    {
        let start_position = self.position;
        let res = f(self);

        match res {
            Ok(value) => Ok(value),
            Err(err) => {
                self.position = start_position;
                Err(err)
            }
        }
    }

    /// Repeatedly calls the provided function but resets the position if the call was not successful.
    /// This is a combination of [`Parser::optional`] and [`Parser::repeated`].
    pub fn optional_repeated<O, F>(&mut self, f: F) -> Option<Vec<O>>
    where
        F: FnMut(&mut Self) -> ParserResult<'source, O>,
    {
        self.optional(|parser| parser.repeated(f))
    }

    pub fn optional_separated<O, F>(&mut self, f: F, separator: OperatorKind) -> Option<Vec<O>>
    where
        F: FnMut(&mut Self) -> ParserResult<'source, O>,
    {
        self.optional(|parser| parser.separated(f, Some(separator)))
    }

    /// Parses `O` and puts the result in a [`Node`]. This is a wrapper around [`Parser::with_node`].
    pub fn parse_node<O>(&mut self) -> ParserResult<'source, Node<O>>
    where
        O: Parse<'source>,
    {
        self.with_node(O::parse)
    }

    /// Repeatedly parses `O` and puts each result in a [`Node`]. This is a combination of
    /// [`Parser::repeated`] and [`Parser::parse_node`].
    pub fn parse_node_repeated<O>(&mut self) -> ParserResult<'source, Vec<Node<O>>>
    where
        O: Parse<'source>,
    {
        self.repeated(|parser| parser.parse_node::<O>())
    }

    /// Optional parses `O`. This is a combination of [`Parser::optional`] and [`Parse::parse`].
    pub fn parse_optional<O>(&mut self) -> Option<O>
    where
        O: Parse<'source>,
    {
        self.optional(|parser| O::parse(parser))
    }

    /// Optionally parses `O` and puts the result in a [`Node`]. This is a combination of
    /// [`Parser::optional`] and [`Parser::parse_node`].
    pub fn parse_node_optional<O>(&mut self) -> Option<Node<O>>
    where
        O: Parse<'source>,
    {
        self.optional(|parser| parser.parse_node::<O>())
    }

    /// Optionally parses `O` repeatedly. This is a combination of [`Parser::optional`] and
    /// [`Parser::parse_node_repeated`].
    pub fn parse_node_optional_repeated<O>(&mut self) -> Option<Vec<Node<O>>>
    where
        O: Parse<'source>,
    {
        self.optional(|parser| parser.parse_node_repeated::<O>())
    }
}

pub trait Parse<'source>
where
    Self: Sized,
{
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self>;
}

/// Chooses the first result that is Ok or returns a [`ParserError::AggregatedErrors`].
#[macro_export]
macro_rules! choose_result {
    ($( $item:expr ),+ $(,)? ) => {{
        // TODO: https://github.com/rust-lang/lang-team/blob/master/projects/declarative-macro-repetition-counts/charter.md
        let mut errors = ::std::collections::HashSet::<$crate::parser::ParserError>::with_capacity(${count(item)});

        $(
            let res = $item;
            match res {
                ::core::result::Result::Ok(res) => return ::core::result::Result::Ok(res),
                ::core::result::Result::Err(err) => errors.insert(err),
            };
        )*

        if errors.len() == 1 {
            ::core::result::Result::Err(errors.into_iter().next().unwrap())
        } else {
            ::core::result::Result::Err($crate::parser::ParserError::AggregatedErrors(errors))
        }
    }}
}

#[cfg(test)]
pub mod test_utils {
    use crate::parser::{flatten_result, Parse, Parser};
    use std::fmt::Debug;

    pub fn run_test<'source, O>(src: &'source str, expected: O)
    where
        O: Parse<'source> + PartialEq + Debug,
    {
        let tokens = papyrus_compiler_lexer::run_lexer(src);
        let mut parser = Parser::new(tokens);

        let res = flatten_result(O::parse(&mut parser));
        assert!(res.is_ok(), "{}\n{:#?}", src, res);
        let res = res.unwrap();

        assert!(parser.expect_eoi().is_ok(), "{}\n{:#?}", src, res);
        assert_eq!(res, expected, "{}", src);
    }

    pub fn run_tests<'source, O>(data: Vec<(&'static str, O)>)
    where
        O: Parse<'source> + PartialEq + Debug,
    {
        for (src, expected) in data {
            run_test(src, expected);
        }
    }
}
