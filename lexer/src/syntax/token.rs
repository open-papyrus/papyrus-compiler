use crate::syntax::keyword_kind::KeywordKind;
use crate::syntax::operator_kind::OperatorKind;
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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

    #[token("Auto", callback = |_| KeywordKind::Auto)]
    #[token("AutoReadOnly", callback = |_| KeywordKind::AutoReadOnly)]
    #[token("BetaOnly", callback = |_| KeywordKind::BetaOnly)]
    #[token("bool", callback = |_| KeywordKind::Bool)]
    #[token("Const", callback = |_| KeywordKind::Const)]
    #[token("CustomEvent", callback = |_| KeywordKind::CustomEvent)]
    #[token("CustomEventName", callback = |_| KeywordKind::CustomEventName)]
    #[token("DebugOnly", callback = |_| KeywordKind::DebugOnly)]
    #[token("Else", callback = |_| KeywordKind::Else)]
    #[token("ElseIf", callback = |_| KeywordKind::ElseIf)]
    #[token("EndEvent", callback = |_| KeywordKind::EndEvent)]
    #[token("EndFunction", callback = |_| KeywordKind::EndFunction)]
    #[token("EndGroup", callback = |_| KeywordKind::EndGroup)]
    #[token("EndIf", callback = |_| KeywordKind::EndIf)]
    #[token("EndProperty", callback = |_| KeywordKind::EndProperty)]
    #[token("EndState", callback = |_| KeywordKind::EndState)]
    #[token("EndStruct", callback = |_| KeywordKind::EndStruct)]
    #[token("EndWhile", callback = |_| KeywordKind::EndWhile)]
    #[token("Event", callback = |_| KeywordKind::Event)]
    #[token("Extends", callback = |_| KeywordKind::Extends)]
    #[token("False", callback = |_| KeywordKind::False)]
    #[token("float", callback = |_| KeywordKind::Float)]
    #[token("Function", callback = |_| KeywordKind::Function)]
    #[token("Global", callback = |_| KeywordKind::Global)]
    #[token("Group", callback = |_| KeywordKind::Group)]
    #[token("If", callback = |_| KeywordKind::If)]
    #[token("Import", callback = |_| KeywordKind::Import)]
    #[token("int", callback = |_| KeywordKind::Int)]
    #[token("Length", callback = |_| KeywordKind::Length)]
    #[token("Native", callback = |_| KeywordKind::Native)]
    #[token("new", callback = |_| KeywordKind::New)]
    #[token("none", callback = |_| KeywordKind::None)]
    #[token("Property", callback = |_| KeywordKind::Property)]
    #[token("return", callback = |_| KeywordKind::Return)]
    #[token("ScriptName", callback = |_| KeywordKind::ScriptName)]
    #[token("ScriptEventName", callback = |_| KeywordKind::ScriptEventName)]
    #[token("State", callback = |_| KeywordKind::State)]
    #[token("string", callback = |_| KeywordKind::String)]
    #[token("Struct", callback = |_| KeywordKind::Struct)]
    #[token("StructVarName", callback = |_| KeywordKind::StructVarName)]
    #[token("true", callback = |_| KeywordKind::True)]
    #[token("var", callback = |_| KeywordKind::Var)]
    #[token("While", callback = |_| KeywordKind::While)]
    Keyword(KeywordKind),

    #[error]
    Error,
}

#[cfg(test)]
mod test {
    use crate::syntax::keyword_kind::KeywordKind;
    use crate::syntax::operator_kind::OperatorKind;
    use crate::syntax::token::Token;
    use logos::Logos;

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

        for (input, expected) in data {
            let mut lex = Token::lexer(input);
            assert_eq!(lex.next(), Some(Token::Operator(expected)));
        }
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
            ("False", KeywordKind::False),
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
            ("true", KeywordKind::True),
            ("var", KeywordKind::Var),
            ("While", KeywordKind::While),
        ];

        for (input, expected) in data {
            let mut lex = Token::lexer(input);
            assert_eq!(lex.next(), Some(Token::Keyword(expected)));
        }
    }
}
