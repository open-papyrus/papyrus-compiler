#[derive(PartialEq, Debug, Copy, Clone)]
pub enum OperatorKind {
    ParenthesisOpen,
    ParenthesisClose,
    SquareBracketsOpen,
    SquareBracketsClose,
    Comma,
    Assignment,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Access,
    DoubleQuotes,
    LogicalNot,
    EqualTo,
    NotEqualTo,
    GreaterThan,
    LessThan,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
    LogicalOr,
    LogicalAnd,
    AdditionAssignment,
    SubtractionAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModulusAssignment,
    CastAs,
    CastIs,
}
