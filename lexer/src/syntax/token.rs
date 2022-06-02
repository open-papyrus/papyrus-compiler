use crate::syntax::keyword_kind::KeywordKind;
use crate::syntax::operator_kind::OperatorKind;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum Token {
    #[token("(", callback = |_| OperatorKind::ParenthesisOpen)]
    #[token(")", callback = |_| OperatorKind::ParenthesisClose)]
    #[token("[", callback = |_| OperatorKind::SquareBracketsOpen)]
    #[token("]", callback = |_| OperatorKind::SquareBracketsClose)]
    #[token(",", callback = |_| OperatorKind::Comma)]
    #[token("=", callback = |_| OperatorKind::Assignment)]
    #[token("+", callback = |_| OperatorKind::Addition)]
    #[token("-", callback = |_| OperatorKind::Subtraction)]
    #[token("*", callback = |_| OperatorKind::Multiplication)]
    #[token("/", callback = |_| OperatorKind::Division)]
    #[token("%", callback = |_| OperatorKind::Modulus)]
    #[token(".", callback = |_| OperatorKind::Access)]
    #[token("\"", callback = |_| OperatorKind::DoubleQuotes)]
    #[token("!", callback = |_| OperatorKind::LogicalNot)]
    #[token("==", callback = |_| OperatorKind::EqualTo)]
    #[token("!=", callback = |_| OperatorKind::NotEqualTo)]
    #[token(">", callback = |_| OperatorKind::GreaterThan)]
    #[token("<", callback = |_| OperatorKind::LessThan)]
    #[token(">=", callback = |_| OperatorKind::GreaterThanOrEqualTo)]
    #[token("<=", callback = |_| OperatorKind::LessThanOrEqualTo)]
    #[token("||", callback = |_| OperatorKind::LogicalOr)]
    #[token("&&", callback = |_| OperatorKind::LogicalAnd)]
    #[token("+=", callback = |_| OperatorKind::AdditionAssignment)]
    #[token("-=", callback = |_| OperatorKind::SubtractionAssignment)]
    #[token("*=", callback = |_| OperatorKind::MultiplicationAssignment)]
    #[token("/=", callback = |_| OperatorKind::DivisionAssignment)]
    #[token("%=", callback = |_| OperatorKind::ModulusAssignment)]
    #[token("as", callback = |_| OperatorKind::CastAs)]
    #[token("is", callback = |_| OperatorKind::CastIs)]
    Operator(OperatorKind),

    #[token("Auto", callback = |_| KeywordKind::Auto, ignore(ascii_case))]
    #[token("AutoReadOnly", callback = |_| KeywordKind::AutoReadOnly, ignore(ascii_case))]
    #[token("BetaOnly", callback = |_| KeywordKind::BetaOnly, ignore(ascii_case))]
    #[token("bool", callback = |_| KeywordKind::Bool, ignore(ascii_case))]
    #[token("Const", callback = |_| KeywordKind::Const, ignore(ascii_case))]
    #[token("CustomEvent", callback = |_| KeywordKind::CustomEvent, ignore(ascii_case))]
    #[token("CustomEventName", callback = |_| KeywordKind::CustomEventName, ignore(ascii_case))]
    #[token("DebugOnly", callback = |_| KeywordKind::DebugOnly, ignore(ascii_case))]
    #[token("Else", callback = |_| KeywordKind::Else, ignore(ascii_case))]
    #[token("ElseIf", callback = |_| KeywordKind::ElseIf, ignore(ascii_case))]
    #[token("EndEvent", callback = |_| KeywordKind::EndEvent, ignore(ascii_case))]
    #[token("EndFunction", callback = |_| KeywordKind::EndFunction, ignore(ascii_case))]
    #[token("EndGroup", callback = |_| KeywordKind::EndGroup, ignore(ascii_case))]
    #[token("EndIf", callback = |_| KeywordKind::EndIf, ignore(ascii_case))]
    #[token("EndProperty", callback = |_| KeywordKind::EndProperty, ignore(ascii_case))]
    #[token("EndState", callback = |_| KeywordKind::EndState, ignore(ascii_case))]
    #[token("EndStruct", callback = |_| KeywordKind::EndStruct, ignore(ascii_case))]
    #[token("EndWhile", callback = |_| KeywordKind::EndWhile, ignore(ascii_case))]
    #[token("Event", callback = |_| KeywordKind::Event, ignore(ascii_case))]
    #[token("Extends", callback = |_| KeywordKind::Extends, ignore(ascii_case))]
    #[token("float", callback = |_| KeywordKind::Float, ignore(ascii_case))]
    #[token("Function", callback = |_| KeywordKind::Function, ignore(ascii_case))]
    #[token("Global", callback = |_| KeywordKind::Global, ignore(ascii_case))]
    #[token("Group", callback = |_| KeywordKind::Group, ignore(ascii_case))]
    #[token("If", callback = |_| KeywordKind::If, ignore(ascii_case))]
    #[token("Import", callback = |_| KeywordKind::Import, ignore(ascii_case))]
    #[token("int", callback = |_| KeywordKind::Int, ignore(ascii_case))]
    #[token("Length", callback = |_| KeywordKind::Length, ignore(ascii_case))]
    #[token("Native", callback = |_| KeywordKind::Native, ignore(ascii_case))]
    #[token("new", callback = |_| KeywordKind::New, ignore(ascii_case))]
    #[token("none", callback = |_| KeywordKind::None, ignore(ascii_case))]
    #[token("Property", callback = |_| KeywordKind::Property, ignore(ascii_case))]
    #[token("return", callback = |_| KeywordKind::Return, ignore(ascii_case))]
    #[token("ScriptName", callback = |_| KeywordKind::ScriptName, ignore(ascii_case))]
    #[token("ScriptEventName", callback = |_| KeywordKind::ScriptEventName, ignore(ascii_case))]
    #[token("State", callback = |_| KeywordKind::State, ignore(ascii_case))]
    #[token("string", callback = |_| KeywordKind::String, ignore(ascii_case))]
    #[token("Struct", callback = |_| KeywordKind::Struct, ignore(ascii_case))]
    #[token("StructVarName", callback = |_| KeywordKind::StructVarName, ignore(ascii_case))]
    #[token("var", callback = |_| KeywordKind::Var, ignore(ascii_case))]
    #[token("While", callback = |_| KeywordKind::While, ignore(ascii_case))]
    Keyword(KeywordKind),

    #[token("true", callback = |_| true, ignore(ascii_case))]
    #[token("false", callback = |_| false, ignore(ascii_case))]
    BooleanLiteral(bool),

    #[regex(r"[ \t\n\r]+")]
    Whitespace,

    // example: https://regex101.com/r/DBrRC0/1
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,

    #[error]
    Error,
}

#[cfg(test)]
mod test {
    use crate::syntax::keyword_kind::KeywordKind;
    use crate::syntax::operator_kind::OperatorKind;
    use crate::syntax::token::Token;
    use logos::{Lexer, Logos};

    fn test_data<T, F>(data: Vec<(&str, T)>, transform: F)
    where
        F: Fn(T) -> Token,
    {
        for (input, expected) in data {
            let expected = transform(expected);

            let mut lex: Lexer<Token> = Token::lexer(input);
            assert_eq!(lex.next(), Some(expected));
        }
    }

    fn test_data_with_variants<T, F>(data: Vec<(&str, T)>, transform: F)
    where
        F: Fn(T) -> Token,
    {
        for (input, expected) in data {
            let expected = transform(expected);
            let variants = vec![input.to_ascii_lowercase(), input.to_ascii_uppercase()];

            for variant in variants {
                let mut lex: Lexer<Token> = Token::lexer(variant.as_str());
                assert_eq!(lex.next(), Some(expected));
            }
        }
    }

    #[test]
    fn test_operators() {
        let data = vec![
            ("(", OperatorKind::ParenthesisOpen),
            (")", OperatorKind::ParenthesisClose),
            ("[", OperatorKind::SquareBracketsOpen),
            ("]", OperatorKind::SquareBracketsClose),
            (",", OperatorKind::Comma),
            ("=", OperatorKind::Assignment),
            ("+", OperatorKind::Addition),
            ("-", OperatorKind::Subtraction),
            ("*", OperatorKind::Multiplication),
            ("/", OperatorKind::Division),
            ("%", OperatorKind::Modulus),
            (".", OperatorKind::Access),
            ("\"", OperatorKind::DoubleQuotes),
            ("!", OperatorKind::LogicalNot),
            ("==", OperatorKind::EqualTo),
            ("!=", OperatorKind::NotEqualTo),
            (">", OperatorKind::GreaterThan),
            ("<", OperatorKind::LessThan),
            (">=", OperatorKind::GreaterThanOrEqualTo),
            ("<=", OperatorKind::LessThanOrEqualTo),
            ("||", OperatorKind::LogicalOr),
            ("&&", OperatorKind::LogicalAnd),
            ("+=", OperatorKind::AdditionAssignment),
            ("-=", OperatorKind::SubtractionAssignment),
            ("*=", OperatorKind::MultiplicationAssignment),
            ("/=", OperatorKind::DivisionAssignment),
            ("%=", OperatorKind::ModulusAssignment),
            ("as", OperatorKind::CastAs),
            ("is", OperatorKind::CastIs),
        ];

        test_data(data, |x| Token::Operator(x));
    }

    #[test]
    fn test_keywords() {
        let data = vec![
            ("Auto", KeywordKind::Auto),
            ("AutoReadOnly", KeywordKind::AutoReadOnly),
            ("BetaOnly", KeywordKind::BetaOnly),
            ("bool", KeywordKind::Bool),
            ("Const", KeywordKind::Const),
            ("CustomEvent", KeywordKind::CustomEvent),
            ("CustomEventName", KeywordKind::CustomEventName),
            ("DebugOnly", KeywordKind::DebugOnly),
            ("Else", KeywordKind::Else),
            ("ElseIf", KeywordKind::ElseIf),
            ("EndEvent", KeywordKind::EndEvent),
            ("EndFunction", KeywordKind::EndFunction),
            ("EndGroup", KeywordKind::EndGroup),
            ("EndIf", KeywordKind::EndIf),
            ("EndProperty", KeywordKind::EndProperty),
            ("EndState", KeywordKind::EndState),
            ("EndStruct", KeywordKind::EndStruct),
            ("EndWhile", KeywordKind::EndWhile),
            ("Event", KeywordKind::Event),
            ("Extends", KeywordKind::Extends),
            ("float", KeywordKind::Float),
            ("Function", KeywordKind::Function),
            ("Global", KeywordKind::Global),
            ("Group", KeywordKind::Group),
            ("If", KeywordKind::If),
            ("Import", KeywordKind::Import),
            ("int", KeywordKind::Int),
            ("Length", KeywordKind::Length),
            ("Native", KeywordKind::Native),
            ("new", KeywordKind::New),
            ("none", KeywordKind::None),
            ("Property", KeywordKind::Property),
            ("return", KeywordKind::Return),
            ("ScriptName", KeywordKind::ScriptName),
            ("ScriptEventName", KeywordKind::ScriptEventName),
            ("State", KeywordKind::State),
            ("string", KeywordKind::String),
            ("Struct", KeywordKind::Struct),
            ("StructVarName", KeywordKind::StructVarName),
            ("var", KeywordKind::Var),
            ("While", KeywordKind::While),
        ];

        test_data_with_variants(data, |x| Token::Keyword(x));
    }

    #[test]
    fn test_whitespaces() {
        let data = vec![
            (" ", Token::Whitespace),
            ("\t", Token::Whitespace),
            ("\n", Token::Whitespace),
            ("\r", Token::Whitespace),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_identifiers() {
        let data = vec![
            ("HelloWorld", Token::Identifier),
            ("_IAmCool", Token::Identifier),
            ("_4You", Token::Identifier),
            ("i", Token::Identifier),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_boolean_literals() {
        let data = vec![("true", true), ("false", false)];

        test_data_with_variants(data, |x| Token::BooleanLiteral(x));
    }
}
