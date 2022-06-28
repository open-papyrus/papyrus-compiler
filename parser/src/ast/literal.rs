use crate::choose_result;
use crate::parser::{Parse, Parser};
use crate::parser_error::*;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'source> {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(&'source str),
    None,
}

impl<'source> Parse<'source> for Literal<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let (token, range) = parser.consume()?;

        choose_result!(
            match token {
                Token::BooleanLiteral(value) => Ok(Literal::Boolean(*value)),
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::BooleanLiteral(false),
                    found: (*token, range.clone())
                }),
            },
            match token {
                Token::IntegerLiteral(value) => Ok(Literal::Integer(*value)),
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::BooleanLiteral(false),
                    found: (*token, range.clone())
                }),
            },
            match token {
                Token::FloatLiteral(value) => Ok(Literal::Float(*value)),
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::BooleanLiteral(false),
                    found: (*token, range.clone())
                }),
            },
            match token {
                Token::StringLiteral(value) => Ok(Literal::String(*value)),
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::BooleanLiteral(false),
                    found: (*token, range.clone())
                }),
            },
            match token {
                Token::NoneLiteral => Ok(Literal::None),
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::BooleanLiteral(false),
                    found: (*token, range.clone())
                }),
            },
        )
    }
}
