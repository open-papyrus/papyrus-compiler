use crate::ast::node::{range_union, Node};
use chumsky::chain::Chain;
use papyrus_compiler_diagnostics::SourceRange;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum ParserError<'source> {
    ExpectedOne {
        expected: Token<'static>,
        found: &'source Token<'source>,
    },
    ExpectedOneOf {
        expected: Vec<Token<'static>>,
        found: &'source Token<'source>,
    },
    ExpectedEOI {
        found: &'source Token<'source>,
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

pub(crate) struct CustomParser<'source> {
    tokens: &'source Vec<(Token<'source>, SourceRange)>,
    offset: usize,
    eoi_range: Option<SourceRange>,
}

impl<'source> CustomParser<'source> {
    pub fn new(tokens: &'source Vec<(Token<'source>, SourceRange)>) -> Self {
        let eoi_range = tokens.get(tokens.len() - 1).map(|(_, range)| range.clone());

        Self {
            tokens,
            offset: 0,
            eoi_range,
        }
    }

    fn save_range(&self, offset: usize) -> ParserResult<'source, SourceRange> {
        if offset >= self.tokens.len() {
            match self.eoi_range.as_ref() {
                Some(range) => Ok(range.clone()),
                None => Err(ParserError::EOI),
            }
        } else {
            match self.tokens.get(offset) {
                Some((_, range)) => Ok(range.clone()),
                None => Err(ParserError::EOI),
            }
        }
    }

    pub fn peek(&self) -> Option<&'source Token<'source>> {
        if self.offset < self.tokens.len() {
            self.tokens.get(self.offset).map(|(token, _)| token)
        } else {
            None
        }
    }

    pub fn consume(&mut self) -> ParserResult<'source, &'source Token<'source>> {
        if self.offset < self.tokens.len() {
            let element = self.tokens.get(self.offset);
            self.offset += 1;
            match element {
                Some((token, _)) => Ok(token),
                None => Err(ParserError::EOI),
            }
        } else {
            Err(ParserError::EOI)
        }
    }

    pub fn expect(
        &mut self,
        expected: Token<'static>,
    ) -> ParserResult<'source, &'source Token<'source>> {
        let found = self.consume()?;
        if found == &expected {
            Ok(found)
        } else {
            Err(ParserError::ExpectedOne { found, expected })
        }
    }

    pub fn consume_expected(
        &mut self,
        expected: Token<'static>,
    ) -> Option<&'source Token<'source>> {
        match self.peek() {
            Some(token) => {
                if token == &expected {
                    self.offset += 1;
                    Some(token)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub fn repeated<T, F: FnMut(&mut Self) -> ParserResult<'source, T>>(
        &mut self,
        mut f: F,
        at_least: usize,
    ) -> ParserResult<'source, Vec<T>> {
        let mut results = vec![];

        loop {
            let res = f(self);
            match res {
                Ok(res) => results.push(res),
                Err(err) => {
                    return if results.len() >= at_least {
                        Ok(results)
                    } else {
                        Err(err)
                    }
                }
            };
        }
    }

    pub fn expect_eoi(&self) -> ParserResult<()> {
        match self.peek() {
            Some(found) => Err(ParserError::ExpectedEOI { found }),
            None => Ok(()),
        }
    }

    pub fn map_with_span<T, F: FnOnce(&mut Self) -> ParserResult<'source, T>>(
        &mut self,
        f: F,
    ) -> ParserResult<'source, Node<T>> {
        let start_range = self.save_range(self.offset)?;
        let res = f(self)?;
        let end_range = self.save_range(self.offset - 1)?;

        Ok(Node::new(res, range_union(start_range, end_range)))
    }
}

#[macro_export]
macro_rules! select_tokens {
    ($parser:ident, $( $pattern:pat_param => $out:expr ),+ $(,)? ;  $expected:expr) => {{
        let token = $parser.consume()?;

        match token {
            $( $pattern => ::core::result::Result::Ok($out) ),+,
            _ => ::core::result::Result::Err($crate::parser::ParserError::ExpectedOneOf {
                found: token,
                expected: $expected
            }),
        }
    }}
}

// #[macro_export]
// macro_rules! one_of_tokens {
//     ($parser:ident, $( $pattern:pat_param ),+ $(,)?) => {{
//         let next_token = $parser.peak();
//
//         match Some(next_token) {
//             $( $pattern =>  ),+,
//         }
//     }}
// }
