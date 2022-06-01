use crate::syntax::keyword_kind::KeywordKind;
use crate::syntax::operator_kind::OperatorKind;
use logos::Logos;

#[derive(Default)]
pub struct SubTokens {
    pub operator: Option<OperatorKind>,
    pub keyword: Option<KeywordKind>,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = SubTokens)]
pub enum Token {
    #[token("(", callback = operator_parenthesis_open)]
    #[token(")", callback = operator_parenthesis_close)]
    #[token("[", callback = operator_square_brackets_open)]
    #[token("]", callback = operator_square_brackets_close)]
    #[token(",", callback = operator_comma)]
    #[token("=", callback = operator_assignment)]
    #[token("+", callback = operator_addition)]
    #[token("-", callback = operator_subtraction)]
    #[token("*", callback = operator_multiplication)]
    #[token("/", callback = operator_division)]
    #[token("%", callback = operator_modulus)]
    #[token(".", callback = operator_access)]
    #[token("\"", callback = operator_double_quotes)]
    #[token("!", callback = operator_logical_not)]
    #[token("==", callback = operator_equal_to)]
    #[token("!=", callback = operator_not_equal_to)]
    #[token(">", callback = operator_greater_than)]
    #[token("<", callback = operator_less_than)]
    #[token(">=", callback = operator_greater_than_or_equal_to)]
    #[token("<=", callback = operator_less_than_or_equal_to)]
    #[token("||", callback = operator_logical_or)]
    #[token("&&", callback = operator_logical_and)]
    #[token("+=", callback = operator_addition_assignment)]
    #[token("-=", callback = operator_subtraction_assignment)]
    #[token("*=", callback = operator_multiplication_assignment)]
    #[token("/=", callback = operator_division_assignment)]
    #[token("%=", callback = operator_modulus_assignment)]
    #[token("as", callback = operator_cast_as)]
    #[token("is", callback = operator_cast_is)]
    Operator,

    #[token("Auto", callback = keyword_auto)]
    #[token("AutoReadOnly", callback = keyword_auto_read_only)]
    #[token("BetaOnly", callback = keyword_beta_only)]
    #[token("bool", callback = keyword_bool)]
    #[token("Const", callback = keyword_const)]
    #[token("CustomEvent", callback = keyword_custom_event)]
    #[token("CustomEventName", callback = keyword_custom_event_name)]
    #[token("DebugOnly", callback = keyword_debug_only)]
    #[token("Else", callback = keyword_else)]
    #[token("ElseIf", callback = keyword_else_if)]
    #[token("EndEvent", callback = keyword_end_event)]
    #[token("EndFunction", callback = keyword_end_function)]
    #[token("EndGroup", callback = keyword_end_group)]
    #[token("EndIf", callback = keyword_end_if)]
    #[token("EndProperty", callback = keyword_end_property)]
    #[token("EndState", callback = keyword_end_state)]
    #[token("EndStruct", callback = keyword_end_struct)]
    #[token("EndWhile", callback = keyword_end_while)]
    #[token("Event", callback = keyword_event)]
    #[token("Extends", callback = keyword_extends)]
    #[token("False", callback = keyword_false)]
    #[token("float", callback = keyword_float)]
    #[token("Function", callback = keyword_function)]
    #[token("Global", callback = keyword_global)]
    #[token("Group", callback = keyword_group)]
    #[token("If", callback = keyword_if)]
    #[token("Import", callback = keyword_import)]
    #[token("int", callback = keyword_int)]
    #[token("Length", callback = keyword_length)]
    #[token("Native", callback = keyword_native)]
    #[token("new", callback = keyword_new)]
    #[token("none", callback = keyword_none)]
    #[token("Property", callback = keyword_property)]
    #[token("return", callback = keyword_return)]
    #[token("ScriptName", callback = keyword_script_name)]
    #[token("ScriptEventName", callback = keyword_script_event_name)]
    #[token("State", callback = keyword_state)]
    #[token("string", callback = keyword_string)]
    #[token("Struct", callback = keyword_struct)]
    #[token("StructVarName", callback = keyword_struct_var_name)]
    #[token("true", callback = keyword_true)]
    #[token("var", callback = keyword_var)]
    #[token("While", callback = keyword_while)]
    Keyword,

    #[error]
    Error,
}

fn operator_parenthesis_open(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::ParenthesisOpen)
}
fn operator_parenthesis_close(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::ParenthesisClose)
}
fn operator_square_brackets_open(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::SquareBracketsOpen)
}
fn operator_square_brackets_close(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::SquareBracketsClose)
}
fn operator_comma(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Comma)
}
fn operator_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Assignment)
}
fn operator_addition(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Addition)
}
fn operator_subtraction(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Subtraction)
}
fn operator_multiplication(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Multiplication)
}
fn operator_division(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Division)
}
fn operator_modulus(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Modulus)
}
fn operator_access(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::Access)
}
fn operator_double_quotes(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::DoubleQuotes)
}
fn operator_logical_not(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::LogicalNot)
}
fn operator_equal_to(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::EqualTo)
}
fn operator_not_equal_to(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::NotEqualTo)
}
fn operator_greater_than(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::GreaterThan)
}
fn operator_less_than(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::LessThan)
}
fn operator_greater_than_or_equal_to(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::GreaterThanOrEqualTo)
}
fn operator_less_than_or_equal_to(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::LessThanOrEqualTo)
}
fn operator_logical_or(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::LogicalOr)
}
fn operator_logical_and(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::LogicalAnd)
}
fn operator_addition_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::AdditionAssignment)
}
fn operator_subtraction_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::SubtractionAssignment)
}
fn operator_multiplication_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::MultiplicationAssignment)
}
fn operator_division_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::DivisionAssignment)
}
fn operator_modulus_assignment(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::ModulusAssignment)
}
fn operator_cast_as(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::CastAs)
}
fn operator_cast_is(lex: &mut logos::Lexer<Token>) {
    lex.extras.operator = Some(OperatorKind::CastIs)
}

fn keyword_auto(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Auto)
}
fn keyword_auto_read_only(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::AutoReadOnly)
}
fn keyword_beta_only(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::BetaOnly)
}
fn keyword_bool(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Bool)
}
fn keyword_const(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Const)
}
fn keyword_custom_event(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::CustomEvent)
}
fn keyword_custom_event_name(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::CustomEventName)
}
fn keyword_debug_only(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::DebugOnly)
}
fn keyword_else(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Else)
}
fn keyword_else_if(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::ElseIf)
}
fn keyword_end_event(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndEvent)
}
fn keyword_end_function(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndFunction)
}
fn keyword_end_group(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndGroup)
}
fn keyword_end_if(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndIf)
}
fn keyword_end_property(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndProperty)
}
fn keyword_end_state(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndState)
}
fn keyword_end_struct(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndStruct)
}
fn keyword_end_while(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::EndWhile)
}
fn keyword_event(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Event)
}
fn keyword_extends(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Extends)
}
fn keyword_false(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::False)
}
fn keyword_float(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Float)
}
fn keyword_function(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Function)
}
fn keyword_global(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Global)
}
fn keyword_group(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Group)
}
fn keyword_if(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::If)
}
fn keyword_import(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Import)
}
fn keyword_int(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Int)
}
fn keyword_length(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Length)
}
fn keyword_native(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Native)
}
fn keyword_new(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::New)
}
fn keyword_none(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::None)
}
fn keyword_property(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Property)
}
fn keyword_return(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Return)
}
fn keyword_script_name(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::ScriptName)
}
fn keyword_script_event_name(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::ScriptEventName)
}
fn keyword_state(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::State)
}
fn keyword_string(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::String)
}
fn keyword_struct(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Struct)
}
fn keyword_struct_var_name(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::StructVarName)
}
fn keyword_true(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::True)
}
fn keyword_var(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::Var)
}
fn keyword_while(lex: &mut logos::Lexer<Token>) {
    lex.extras.keyword = Some(KeywordKind::While)
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
            assert_eq!(lex.next(), Some(Token::Operator));
            assert_eq!(lex.extras.operator, Some(expected));
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
            assert_eq!(lex.next(), Some(Token::Keyword));
            assert_eq!(lex.extras.keyword, Some(expected));
        }
    }
}
