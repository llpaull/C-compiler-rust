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
            ',' => tokens.push(Token::Comma),
            '~' => tokens.push(Token::Operator(Operation::Basic(BasicOp::BitNot))),
            '-' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Minus)));
                },
                Some('-') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Decrement)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::Minus))),
            },
            '+' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Plus)));
                },
                Some('+') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Increment)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::Plus))),
            },
            '*' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Mult)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::Mult))),
            },
            '/' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Div)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::Div))),
            },
            '!' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::NotEqual)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::LogicalNot))),
            },
            '<' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::LessEqual)));
                },
                Some('<') => {
                    iter.next();
                    match iter.peek() {
                        Some('=') => {
                            iter.next();
                            tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::LShift)));
                        },
                        _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::LShift))),
                    }
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::LessThan))),
            },
            '>' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::GreaterEqual)));
                },
                Some('>') => {
                    iter.next();
                    match iter.peek() {
                        Some('=') => {
                            iter.next();
                            tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::RShift)));
                        },
                        _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::RShift))),
                    }
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::GreaterThan))),
            },
            '&' => match iter.peek() {
                Some('&') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::LogicalAnd)));
                },
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::BitAnd)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::BitAnd))),
            },
            '|' => match iter.peek() {
                Some('|') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::LogicalOr)));
                },
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::BitOr)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::BitOr))),
            },
            '%' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Mod)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::Mod))),
            },
            '^' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::BitXor)));
                },
                _ => tokens.push(Token::Operator(Operation::Basic(BasicOp::BitXor))),
            },
            '=' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::Operator(Operation::Basic(BasicOp::Equal)));
                },
                _ => tokens.push(Token::Operator(Operation::Assignment(AssignmentOp::Assign))),
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
    Comma,
    Keyword(String),
    Identifier(String),
    Integer(i64),
    Operator(Operation),
}

#[derive(Debug)]
pub enum Operation {
    Basic(BasicOp),
    Assignment(AssignmentOp),
}

#[derive(Debug)]
pub enum BasicOp {
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
    LShift,
    RShift,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum AssignmentOp {
    Assign,
    Plus,
    Increment,
    Minus,
    Decrement,
    Mult,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
}
