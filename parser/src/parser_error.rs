use papyrus_compiler_diagnostics::SourceRange;
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

pub(crate) type TokenWithRange<'source> = (Token<'source>, SourceRange);

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError<'source> {
    ExpectedToken {
        expected: Token<'static>,
        found: TokenWithRange<'source>,
    },
    ExpectedOneOf {
        expected: HashSet<Token<'static>>,
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
            ParserError::ExpectedOneOf { .. } => state.write_i8(2),
            ParserError::UnexpectedEOI => state.write_i8(3),
            ParserError::AggregatedErrors(_) => state.write_i8(4),
            ParserError::ExpectedEOI { .. } => state.write_i8(5),
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

fn extract_best_error(error: ParserError) -> ParserError {
    match error {
        ParserError::AggregatedErrors(errors) => {
            let mut best_errors = errors
                .into_iter()
                .filter_map(|err| match err {
                    ParserError::ExpectedToken { expected, found } => {
                        let (token, range) = found;
                        Some((range, (expected, token)))
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();

            best_errors.sort_by(|(range_a, _), (range_b, _)| {
                let start_a = range_a.start;
                let start_b = range_b.start;

                start_a.cmp(&start_b)
            });

            let (last_range, _) = best_errors.last().unwrap();
            let last_range = last_range.clone();

            let best_errors = best_errors
                .into_iter()
                .filter(|(range, _)| last_range.start == range.start)
                .collect::<Vec<_>>();

            if best_errors.len() == 1 {
                let (range, (expected, found)) = best_errors.into_iter().next().unwrap();

                ParserError::ExpectedToken {
                    expected,
                    found: (found, range),
                }
            } else {
                let (range, (_, found)) = best_errors.get(0).unwrap().clone();

                ParserError::ExpectedOneOf {
                    expected: best_errors
                        .into_iter()
                        .map(|(_, (expected, _))| expected)
                        .collect(),
                    found: (found, range),
                }
            }
        }
        _ => error,
    }
}

pub fn flatten_result<TOk>(result: ParserResult<TOk>) -> ParserResult<TOk> {
    match result {
        Ok(value) => Ok(value),
        Err(err) => Err(extract_best_error(flatten_error(err))),
    }
}
