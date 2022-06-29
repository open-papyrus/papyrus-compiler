use crate::syntax::keyword_kind::KeywordKind;
use crate::syntax::operator_kind::OperatorKind;
use crate::LexerDiagnosticsKind;
use logos::Logos;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[derive(Default, Debug)]
pub struct LexerExtras {
    pub parsing_errors: Vec<(LexerDiagnosticsKind, logos::Span)>,
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(extras = LexerExtras)]
pub enum Token<'a> {
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
    #[token("as", callback = |_| OperatorKind::CastAs, ignore(ascii_case))]
    #[token("is", callback = |_| OperatorKind::CastIs, ignore(ascii_case))]
    Operator(OperatorKind),

    #[token("Auto", callback = |_| KeywordKind::Auto, ignore(ascii_case))]
    #[token("AutoReadOnly", callback = |_| KeywordKind::AutoReadOnly, ignore(ascii_case))]
    #[token("BetaOnly", callback = |_| KeywordKind::BetaOnly, ignore(ascii_case))]
    #[token("bool", callback = |_| KeywordKind::Bool, ignore(ascii_case))]
    #[token("CustomEvent", callback = |_| KeywordKind::CustomEvent, ignore(ascii_case))]
    #[token("CustomEventName", callback = |_| KeywordKind::CustomEventName, ignore(ascii_case))]
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
    #[token("Group", callback = |_| KeywordKind::Group, ignore(ascii_case))]
    #[token("If", callback = |_| KeywordKind::If, ignore(ascii_case))]
    #[token("Import", callback = |_| KeywordKind::Import, ignore(ascii_case))]
    #[token("int", callback = |_| KeywordKind::Int, ignore(ascii_case))]
    #[token("new", callback = |_| KeywordKind::New, ignore(ascii_case))]
    #[token("Parent", callback = |_| KeywordKind::Parent, ignore(ascii_case))]
    #[token("Property", callback = |_| KeywordKind::Property, ignore(ascii_case))]
    #[token("return", callback = |_| KeywordKind::Return, ignore(ascii_case))]
    #[token("ScriptName", callback = |_| KeywordKind::ScriptName, ignore(ascii_case))]
    #[token("ScriptEventName", callback = |_| KeywordKind::ScriptEventName, ignore(ascii_case))]
    #[token("Self", callback = |_| KeywordKind::Self_, ignore(ascii_case))]
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

    #[regex(r"-?\d+", callback = parse_integer)]
    #[regex(r"0[xX][0-9a-fA-F]+", callback = parse_hex_integer)]
    IntegerLiteral(i32),

    #[regex(r"-?\d+\.\d+", callback = parse_float)]
    FloatLiteral(f32),

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\"|\\\\)*""#, callback = parse_string)]
    StringLiteral(&'a str),

    #[token("none", ignore(ascii_case))]
    NoneLiteral,

    // example: https://regex101.com/r/DBrRC0/1
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", callback = |lex| lex.slice())]
    Identifier(&'a str),

    #[regex(r";[^/][^\r\n]*[\r\n]*", callback = |lex| lex.slice())]
    SingleLineComment(&'a str),

    #[regex(r";/(?:[^/]|/[^;])*/;", callback = |lex| lex.slice())]
    MultiLineComment(&'a str),

    #[regex(r"\{[^\}]*\}", callback = |lex| lex.slice())]
    DocumentationComment(&'a str),

    #[error]
    #[token(r"\", logos::skip)]
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    Error,
}

fn parse_integer<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Option<i32> {
    let slice: &'a str = lex.slice();
    let res = i32::from_str(slice);
    match res {
        Ok(value) => Some(value),
        Err(err) => {
            lex.extras
                .parsing_errors
                .push((LexerDiagnosticsKind::ParseIntError(err), lex.span()));

            None
        }
    }
}

fn parse_hex_integer<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Option<i32> {
    let slice: &'a str = lex.slice();
    // slice without the leading '0x'
    let res = i32::from_str_radix(&slice[2..], 16);
    match res {
        Ok(value) => Some(value),
        Err(err) => {
            lex.extras
                .parsing_errors
                .push((LexerDiagnosticsKind::ParseIntError(err), lex.span()));

            None
        }
    }
}

fn parse_float<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Option<f32> {
    let slice: &'a str = lex.slice();
    let res = f32::from_str(slice);
    match res {
        Ok(value) => {
            if value.is_finite() {
                Some(value)
            } else {
                lex.extras
                    .parsing_errors
                    .push((LexerDiagnosticsKind::FloatNotFinite, lex.span()));

                None
            }
        }
        Err(err) => {
            lex.extras
                .parsing_errors
                .push((LexerDiagnosticsKind::ParseFloatError(err), lex.span()));

            None
        }
    }
}

fn parse_string<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> &'a str {
    let slice: &'a str = lex.slice();
    let length = slice.len();
    // slice without the leading and trailing double quotes
    &slice[1..length - 1]
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Operator(operator) => write!(f, "{}", operator),
            Token::Keyword(keyword) => write!(f, "{}", keyword),
            Token::BooleanLiteral(value) => write!(f, "{}", value),
            Token::IntegerLiteral(value) => write!(f, "{}", value),
            Token::FloatLiteral(value) => write!(f, "{}", value),
            Token::StringLiteral(value) => write!(f, "{}", value),
            Token::NoneLiteral => write!(f, "none"),
            Token::Identifier(identifier) => write!(f, "{}", identifier),
            Token::SingleLineComment(comment) => write!(f, "{}", comment),
            Token::MultiLineComment(comment) => write!(f, "{}", comment),
            Token::DocumentationComment(comment) => write!(f, "{}", comment),
            Token::Error => write!(f, "ERROR"),
        }
    }
}

impl<'a> Hash for Token<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::Operator(operator) => operator.hash(state),
            Token::Keyword(keyword) => keyword.hash(state),
            Token::BooleanLiteral(value) => value.hash(state),
            Token::IntegerLiteral(value) => value.hash(state),
            Token::FloatLiteral(value) => state.write_u32(value.to_bits()),
            Token::StringLiteral(value) => value.hash(state),
            Token::NoneLiteral => state.write(b"none"),
            Token::Identifier(value) => value.hash(state),
            Token::SingleLineComment(comment) => comment.hash(state),
            Token::MultiLineComment(comment) => comment.hash(state),
            Token::DocumentationComment(comment) => comment.hash(state),
            Token::Error => state.write(b"error"),
        }
    }
}

impl<'a> Eq for Token<'a> {}

impl<'a> Token<'a> {
    pub fn error_display(&self) -> String {
        match self {
            Token::Operator(operator) => format!("Operator: '{}'", operator),
            Token::Keyword(keyword) => format!("Keyword: '{}'", keyword),
            Token::BooleanLiteral(_) => "Boolean Literal".to_string(),
            Token::IntegerLiteral(_) => "Integer Literal".to_string(),
            Token::FloatLiteral(_) => "Float Literal".to_string(),
            Token::StringLiteral(_) => "String Literal".to_string(),
            Token::NoneLiteral => "none".to_string(),
            Token::Identifier(_) => "Identifier".to_string(),
            Token::SingleLineComment(_) => "Single Line Comment".to_string(),
            Token::MultiLineComment(_) => "Multi Line Comment".to_string(),
            Token::DocumentationComment(_) => "Documentation Comment".to_string(),
            Token::Error => "ERROR".to_string(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::keyword_kind::KeywordKind;
    use crate::syntax::operator_kind::OperatorKind;
    use crate::syntax::token::{LexerExtras, Token};
    use logos::{Lexer, Logos};

    fn test_data<'a, T, F>(data: Vec<(&'a str, T)>, transform: F)
    where
        F: Fn(T) -> Token<'a>,
    {
        for (input, expected) in data {
            let expected = transform(expected);

            let mut lex: Lexer<Token> = Token::lexer(input);
            assert_eq!(lex.next(), Some(expected), "{}", input);
            assert_eq!(lex.next(), None, "{}", input)
        }
    }

    fn test_data_with_variants<'a, T, F>(data: Vec<(&'a str, T)>, transform: F)
    where
        F: Fn(T) -> Token<'a>,
    {
        for (input, expected) in data {
            let expected = transform(expected);
            let variants = vec![input.to_ascii_lowercase(), input.to_ascii_uppercase()];

            for variant in variants {
                let mut lex: Lexer<Token> = Token::lexer(variant.as_str());
                assert_eq!(lex.next(), Some(expected), "{}", input);
                assert_eq!(lex.next(), None, "{}", input)
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

        test_data_with_variants(data, Token::Operator);
    }

    #[test]
    fn test_keywords() {
        let data = vec![
            ("Auto", KeywordKind::Auto),
            ("AutoReadOnly", KeywordKind::AutoReadOnly),
            ("BetaOnly", KeywordKind::BetaOnly),
            ("bool", KeywordKind::Bool),
            ("CustomEvent", KeywordKind::CustomEvent),
            ("CustomEventName", KeywordKind::CustomEventName),
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
            ("Group", KeywordKind::Group),
            ("If", KeywordKind::If),
            ("Import", KeywordKind::Import),
            ("int", KeywordKind::Int),
            ("new", KeywordKind::New),
            ("Parent", KeywordKind::Parent),
            ("Property", KeywordKind::Property),
            ("return", KeywordKind::Return),
            ("ScriptName", KeywordKind::ScriptName),
            ("ScriptEventName", KeywordKind::ScriptEventName),
            ("Self", KeywordKind::Self_),
            ("State", KeywordKind::State),
            ("string", KeywordKind::String),
            ("Struct", KeywordKind::Struct),
            ("StructVarName", KeywordKind::StructVarName),
            ("var", KeywordKind::Var),
            ("While", KeywordKind::While),
        ];

        test_data_with_variants(data, Token::Keyword);
    }

    #[test]
    fn test_identifiers() {
        let data = vec![
            ("HelloWorld", Token::Identifier("HelloWorld")),
            ("_IAmCool", Token::Identifier("_IAmCool")),
            ("_4You", Token::Identifier("_4You")),
            ("i", Token::Identifier("i")),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_boolean_literals() {
        let data = vec![("true", true), ("false", false)];
        test_data_with_variants(data, Token::BooleanLiteral);
    }

    #[test]
    fn test_integer_literals() {
        let data: Vec<(&str, i32)> = vec![
            ("0", 0),
            ("1", 1),
            ("-1", -1),
            ("2147483647", i32::MAX),
            ("-2147483648", i32::MIN),
        ];

        test_data(data, Token::IntegerLiteral);
    }

    #[test]
    fn test_integer_hex_literals() {
        let data: Vec<(&str, i32)> = vec![
            ("0x0", 0x0),
            ("0x1", 0x1),
            ("0xF", 0xF),
            ("0x10", 0x10),
            ("0x00000010", 0x10),
            ("0x7fffffff", i32::MAX),
        ];

        test_data_with_variants(data, Token::IntegerLiteral);
    }

    #[test]
    fn test_float_literals() {
        let data: Vec<(&str, f32)> = vec![("0.0", 0.0), ("1.0", 1.0), ("-1.0", -1.0)];

        test_data(data, Token::FloatLiteral);
    }

    #[test]
    fn test_string_literals() {
        let data = vec![
            (r#""""#, Token::StringLiteral("")),
            (r#""Hello World!""#, Token::StringLiteral("Hello World!")),
            (r#""\t\u\n\"""#, Token::StringLiteral(r#"\t\u\n\""#)),
            (
                r#""skyui\\icons_category_psychosteve.swf""#,
                Token::StringLiteral(r#"skyui\\icons_category_psychosteve.swf"#),
            ),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_none_literal() {
        let data = vec![("none", Token::NoneLiteral)];
        test_data_with_variants(data, |x| x);
    }

    #[test]
    fn test_single_line_comments() {
        let data = vec![
            ("; ", Token::SingleLineComment("; ")),
            (
                "; This is a Single Line Comment!",
                Token::SingleLineComment("; This is a Single Line Comment!"),
            ),
            (
                "; This is a Single Line Comment!\n",
                Token::SingleLineComment("; This is a Single Line Comment!\n"),
            ),
            (
                "; This is a Single Line Comment!\r",
                Token::SingleLineComment("; This is a Single Line Comment!\r"),
            ),
            (
                "; This is a Single Line Comment!\r\n",
                Token::SingleLineComment("; This is a Single Line Comment!\r\n"),
            ),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_multi_line_comments() {
        let data = vec![
            (";//;", Token::MultiLineComment(";//;")),
            (
                ";/ Hello World! /;",
                Token::MultiLineComment(";/ Hello World! /;"),
            ),
            (
                ";/ Hello\nWorld! /;",
                Token::MultiLineComment(";/ Hello\nWorld! /;"),
            ),
            (
                ";/ Hello/World! /;",
                Token::MultiLineComment(";/ Hello/World! /;"),
            ),
            (";/ ; /;", Token::MultiLineComment(";/ ; /;")),
            ("\r\n;/ \r\n /;\r\n", Token::MultiLineComment(";/ \r\n /;")),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_documentation_comments() {
        let data = vec![
            ("{}", Token::DocumentationComment("{}")),
            (
                "{ Hello World! }",
                Token::DocumentationComment("{ Hello World! }"),
            ),
            (
                "{ Hello \n World! }",
                Token::DocumentationComment("{ Hello \n World! }"),
            ),
            (
                "{ Hello \r World! }",
                Token::DocumentationComment("{ Hello \r World! }"),
            ),
            (
                "{ Hello \r\n World! }",
                Token::DocumentationComment("{ Hello \r\n World! }"),
            ),
        ];

        test_data(data, |x| x);
    }

    #[test]
    fn test_line_terminator() {
        let mut lexer = Token::lexer(r"\");
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_error() {
        let mut lexer = Token::lexer("^");
        assert_eq!(lexer.next(), Some(Token::Error));
    }

    #[test]
    fn test_extra_errors() {
        let data = vec![
            "2147483648",                                  // max int + 1
            "0x80000000",                                  // max int + 1 in hex
            "3402823470000000000000000000000000000000.0",  // max float * 10
            "-2147483649",                                 // min int - 1
            "-3402823470000000000000000000000000000000.0", // min float * 10
        ];

        for src in data {
            let mut lexer = Token::lexer(src);
            assert_eq!(lexer.next(), Some(Token::Error), "{}", src);

            let extra: LexerExtras = lexer.extras;
            assert!(!extra.parsing_errors.is_empty(), "{}", src);
        }
    }

    #[test]
    #[cfg(feature = "test-external-scripts")]
    fn test_scripts() {
        let data = vec![
            ("MrOctopus/nl_mcm/main/scripts/source/nl_mcm.psc", 769),
            (
                "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_globalinfo.psc",
                38,
            ),
            (
                "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_module.psc",
                2358,
            ),
            (
                "MrOctopus/nl_mcm/main/scripts/source/nl_mcm_playerloadalias.psc",
                35,
            ),
            ("MrOctopus/nl_mcm/main/source/nl_mcm.psc", 4013),
            ("MrOctopus/nl_mcm/main/source/nl_mcm_globalinfo.psc", 23),
            ("MrOctopus/nl_mcm/main/source/nl_mcm_module.psc", 3183),
            (
                "MrOctopus/nl_mcm/main/source/nl_mcm_playerloadalias.psc",
                63,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_ActiveEffectsWidget.psc",
                448,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigBase.psc",
                4127,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigManager.psc",
                1302,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_ConfigMenu.psc",
                6556,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_Main.psc",
                1097,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_PlayerLoadGameAlias.psc",
                21,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_QF_ConfigManagerInstance.psc",
                43,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_QuestBase.psc",
                80,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_SettingsManager.psc",
                431,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetBase.psc",
                920,
            ),
            (
                "schlangster/skyui/dist/Data/Scripts/Source/SKI_WidgetManager.psc",
                617,
            ),
        ];

        for (script_path, expected_count) in data {
            let script_path = format!("../extern/{}", script_path);
            let path = std::path::Path::new(script_path.as_str());
            assert!(path.exists(), "{}", script_path);

            let script = std::fs::read_to_string(path).unwrap();

            let lexer = Token::lexer(script.as_str());
            let mut count: usize = 0;

            for token in lexer {
                assert_ne!(token, Token::Error);
                count += 1
            }

            assert_eq!(count, expected_count);
        }
    }
}
