use super::token::*;
use std::error::Error;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    tokens: Vec<Token>,
    iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(source: &str) -> Lexer {
        Lexer {
            tokens: vec![],
            iter: source.chars().peekable(),
        }
    }

    fn push(&mut self, token: Token) {
        self.iter.next();
        self.tokens.push(token);
    }

    fn add(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn drop(&mut self) {
        self.iter.next();
    }

    fn get_string<F>(&mut self, cond: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        let mut result = String::new();
        while let Some(&c) = self.iter.peek() {
            if cond(&c) {
                result.push(c);
                self.iter.next();
            } else {
                break;
            }
        }
        result
    }

    pub fn lex(s: &str) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut lexer = Lexer::new(s);
        while let Some(&c) = lexer.peek() {
            match c {
                '(' => lexer.push(Token::OpenParenthesis),
                ')' => lexer.push(Token::CloseParenthesis),
                '{' => lexer.push(Token::OpenBrace),
                '}' => lexer.push(Token::CloseBrace),
                '[' => lexer.push(Token::OpenBracket),
                ']' => lexer.push(Token::CloseBracket),
                ';' => lexer.push(Token::Semicolon),
                ':' => lexer.push(Token::Colon),
                '?' => lexer.push(Token::QuestionMark),
                '~' => lexer.push(Token::BitNot),
                ',' => lexer.push(Token::Comma),
                ' ' | '\t' | '\n' | '\r' => lexer.drop(),
                'a'..='z' | 'A'..='Z' => {
                    let word: &str = &lexer.get_string(|x| x.is_alphanumeric());
                    match word {
                        "int" => lexer.add(Token::Keyword(Keyword::Int)),
                        "return" => lexer.add(Token::Keyword(Keyword::Return)),
                        "if" => lexer.add(Token::Keyword(Keyword::If)),
                        "else" => lexer.add(Token::Keyword(Keyword::Else)),
                        "for" => lexer.add(Token::Keyword(Keyword::For)),
                        "while" => lexer.add(Token::Keyword(Keyword::While)),
                        "do" => lexer.add(Token::Keyword(Keyword::Do)),
                        "break" => lexer.add(Token::Keyword(Keyword::Break)),
                        "continue" => lexer.add(Token::Keyword(Keyword::Continue)),
                        _ => lexer.add(Token::Identifier(word.to_string())),
                    }
                }
                '0'..='9' => {
                    let num = lexer.get_string(|x| x.is_numeric());
                    let num = num.parse::<u32>()?;
                    lexer.add(Token::Integer(num));
                }
                other => match (lexer.next().unwrap(), lexer.peek()) {
                    ('&', Some(&'&')) => lexer.push(Token::LogicAnd),
                    ('|', Some(&'|')) => lexer.push(Token::LogicOr),
                    ('<', Some(&'=')) => lexer.push(Token::LessThanOrEqual),
                    ('>', Some(&'=')) => lexer.push(Token::GreaterThanOrEqual),
                    ('=', Some(&'=')) => lexer.push(Token::Equal),
                    ('!', Some(&'=')) => lexer.push(Token::NotEqual),
                    ('+', Some(&'=')) => lexer.push(Token::AssignAdd),
                    ('-', Some(&'=')) => lexer.push(Token::AssignSub),
                    ('*', Some(&'=')) => lexer.push(Token::AssignMult),
                    ('/', Some(&'=')) => lexer.push(Token::AssignDiv),
                    ('%', Some(&'=')) => lexer.push(Token::AssignMod),
                    ('&', Some(&'=')) => lexer.push(Token::AssignBitAnd),
                    ('|', Some(&'=')) => lexer.push(Token::AssignBitOr),
                    ('^', Some(&'=')) => lexer.push(Token::AssignBitXor),
                    ('+', Some(&'+')) => lexer.push(Token::Increment),
                    ('-', Some(&'-')) => lexer.push(Token::Decrement),
                    ('!', _) => lexer.add(Token::LogicNot),
                    ('&', _) => lexer.add(Token::BitwiseAnd),
                    ('|', _) => lexer.add(Token::BitwiseOr),
                    ('=', _) => lexer.add(Token::Assign),
                    ('+', _) => lexer.add(Token::Addition),
                    ('-', _) => lexer.add(Token::Negation),
                    ('*', _) => lexer.add(Token::Multiplication),
                    ('/', _) => lexer.add(Token::Division),
                    ('%', _) => lexer.add(Token::Modulus),
                    ('^', _) => lexer.add(Token::BitwiseXor),
                    ('<', Some(&'<')) => {
                        lexer.next();
                        match lexer.peek() {
                            Some(&'=') => lexer.push(Token::AssignShiftLeft),
                            _ => lexer.add(Token::ShiftLeft),
                        }
                    }
                    ('<', _) => lexer.add(Token::LessThan),
                    ('>', Some(&'>')) => {
                        lexer.next();
                        match lexer.peek() {
                            Some(&'=') => lexer.push(Token::AssignShiftRight),
                            _ => lexer.add(Token::ShiftRight),
                        }
                    }
                    ('>', _) => lexer.add(Token::GreaterThan),
                    _ => return Err(format!("Unknown symbol {:?}", other).into()),
                },
            }
        }
        Ok(lexer.tokens)
    }
}
