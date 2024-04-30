const KEYWORDS: [&str; 2] = ["int", "return"];

pub fn lex(s: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = s.chars().peekable();
    while let Some(char) = iter.next() {
        match char {
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            ';' => tokens.push(Token::Semicolon),
            '~' => tokens.push(Token::Operator(Operation::BitNot)),
            '-' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::MinusAssign));
                },
                _ => tokens.push(Token::Operator(Operation::Minus)),
            },
            '+' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::PlusAssign));
                },
                _ => tokens.push(Token::Operator(Operation::Plus)),
            },
            '*' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::MultAssign));
                },
                _ => tokens.push(Token::Operator(Operation::Mult)),
            },
            '/' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::DivAssign));
                },
                _ => tokens.push(Token::Operator(Operation::Div)),
            },
            '!' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::NotEqual));
                },
                _ => tokens.push(Token::Operator(Operation::LogicalNot)),
            },
            '<' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::LessThanOrEqual));
                },
                Some('<') => {
                    iter.next();
                    match iter.peek() {
                        Some('=') => {
                            iter.next();
                            tokens.push(Token::Operator(Operation::LeftShiftAssign));
                        },
                        _ => tokens.push(Token::Operator(Operation::LeftShift)),
                    }
                },
                _ => tokens.push(Token::Operator(Operation::LessThan)),
            },
            '>' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::GreaterThanOrEqual));
                },
                Some('>') => {
                    iter.next();
                    match iter.peek() {
                        Some('=') => {
                            iter.next();
                            tokens.push(Token::Operator(Operation::RightShiftAssign));
                        },
                        _ => tokens.push(Token::Operator(Operation::RightShift)),
                    }
                },
                _ => tokens.push(Token::Operator(Operation::GreaterThan)),
            },
            '&' => match iter.peek() {
                Some('&') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::LogicalAnd));
                },
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::BitAndAssign));
                },
                _ => tokens.push(Token::Operator(Operation::BitAnd)),
            },
            '|' => match iter.peek() {
                Some('|') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::LogicalOr));
                },
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::BitOrAssign));
                },
                _ => tokens.push(Token::Operator(Operation::BitOr)),
            },
            '%' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::ModAssign));
                },
                _ => tokens.push(Token::Operator(Operation::Mod)),
            },
            '^' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::BitXorAssign));
                },
                _ => tokens.push(Token::Operator(Operation::BitXor)),
            },
            '=' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Equal));
                },
                _ => tokens.push(Token::Operator(Operation::Assign)),
            },
            ' ' | '\n' | '\r' | '\t' => {},
            _ => {
                if char.is_numeric() {
                    let mut num = char.to_digit(10).unwrap() as i64;
                    while let Some(&char) = iter.peek() {
                        if char.is_numeric() {
                            let digit = char.to_digit(10).unwrap() as i64;
                            num = num * 10 + digit;
                            iter.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::Integer(num));
                }
                else {
                    let mut keyword = String::new();
                    keyword.push(char);
                    while let Some(&char) = iter.peek() {
                        if char.is_alphanumeric() {
                            keyword.push(char);
                            iter.next();
                        } else {
                            break;
                        }
                    }
                    if KEYWORDS.contains(&keyword.as_str()) {
                        tokens.push(Token::Keyword(keyword));
                    } else {
                        tokens.push(Token::Identifier(keyword));
                    }
                }
            }
        }
    }
    tokens
}

#[derive(Debug)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Keyword(String),
    Identifier(String),
    Integer(i64),
    Operator(Operation),
}

#[derive(Debug)]
pub enum Operation {
    BitNot,
    LogicalNot,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Assign,
    PlusAssign,
    MinusAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
}
