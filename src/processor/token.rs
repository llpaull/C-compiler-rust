use super::ast::{BinOp, UnOp};
use std::error::Error;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    // basic tokens
    OpenParenthesis,
    CloseParenthesis,
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

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    Int,
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
}

pub trait IntoResult<T>: Sized {
    fn into_result(self) -> Result<T, Box<dyn Error>>;
}

impl IntoResult<BinOp> for Token {
    fn into_result(self) -> Result<BinOp, Box<dyn Error>> {
        match self {
            Token::Addition => Ok(BinOp::Addition),
            Token::Negation => Ok(BinOp::Subtraction),
            Token::Multiplication => Ok(BinOp::Multiplication),
            Token::Division => Ok(BinOp::Division),
            Token::Modulus => Ok(BinOp::Modulus),
            Token::LessThan => Ok(BinOp::LessThan),
            Token::LessThanOrEqual => Ok(BinOp::LessThanOrEqual),
            Token::GreaterThan => Ok(BinOp::GreaterThan),
            Token::GreaterThanOrEqual => Ok(BinOp::GreaterThanOrEqual),
            Token::Equal => Ok(BinOp::Equal),
            Token::NotEqual => Ok(BinOp::NotEqual),
            Token::LogicAnd => Ok(BinOp::LogicAnd),
            Token::LogicOr => Ok(BinOp::LogicOr),
            Token::BitwiseAnd => Ok(BinOp::BitwiseAnd),
            Token::BitwiseOr => Ok(BinOp::BitwiseOr),
            Token::BitwiseXor => Ok(BinOp::BitwiseXor),
            Token::ShiftLeft => Ok(BinOp::ShiftLeft),
            Token::ShiftRight => Ok(BinOp::ShiftRight),
            Token::Comma => Ok(BinOp::Comma),
            err => Err(format!("Cannot convert {:?} into BinOp", err).into()),
        }
    }
}

impl IntoResult<UnOp> for Token {
    fn into_result(self) -> Result<UnOp, Box<dyn Error>> {
        match self {
            Token::Negation => Ok(UnOp::Negation),
            Token::BitNot => Ok(UnOp::BitNot),
            Token::LogicNot => Ok(UnOp::LogicNot),
            err => Err(format!(
                "Unsupported token {:?}, expected unary operator !, ~, or -",
                err
            )
            .into()),
        }
    }
}
