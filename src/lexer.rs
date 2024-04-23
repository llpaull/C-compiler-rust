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
}

const KEYWORDS: [&str; 2] = ["int", "return"];

pub struct Lexer {}

impl Lexer {
    pub fn lex(s: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut iter = s.split_whitespace();
        while let Some(word) = iter.next() {
            let mut char_iter = word.chars().peekable();
            while let Some(char) = char_iter.next() {
                match char {
                    '(' => tokens.push(Token::LParen),
                    ')' => tokens.push(Token::RParen),
                    '{' => tokens.push(Token::LBrace),
                    '}' => tokens.push(Token::RBrace),
                    '[' => tokens.push(Token::LBracket),
                    ']' => tokens.push(Token::RBracket),
                    ';' => tokens.push(Token::Semicolon),
                    _ => {
                        if char.is_numeric() {
                            let mut num = char.to_digit(10).unwrap() as i64;
                            while let Some(&char) = char_iter.peek() {
                                if char.is_numeric() {
                                    let digit = char.to_digit(10).unwrap() as i64;
                                    num = num * 10 + digit;
                                    char_iter.next();
                                } else {
                                    break;
                                }
                            }
                            tokens.push(Token::Integer(num));
                        }
                        else {
                            let mut keyword = String::new();
                            keyword.push(char);
                            while let Some(&char) = char_iter.peek() {
                                if char.is_alphabetic() {
                                    keyword.push(char);
                                    char_iter.next();
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
        }
        tokens
    }
}
