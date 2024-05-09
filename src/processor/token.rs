#[derive(Debug)]
pub enum Token {
    // basic tokens
    OpenParenthesis,
    CloseParentesis,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Semicolon,
    Integer(u32),
    Keyword(Keyword),
    Identifier(String),

    // 'normal' operators
    BitNot,
    LogicNot,
    Negation,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    LogicAnd,
    LogicOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,

    // 'special' operators
    Comma,
    QuestionMark,
    Colon,
    
    // assignment operators
    Assign,
    AssignAdd,
    AssignSub,
    AssignMult,
    AssignDiv,
    AssignMod,
    AssignShiftLeft,
    AssignShiftRight,
    AssignBitAnd,
    AssignBitOr,
    AssignBitXor,
    Increment,
    Decrement,
}

#[derive(Debug)]
pub enum Keyword {
    Int,
    Return,
    If,
    Else,
}

