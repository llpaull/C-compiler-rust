use crate::lexer::Token;

pub struct Parser{}

impl Parser {
    pub fn parse(tokens: Vec<Token>) {
        let mut iter = tokens.iter();
        while let Some(token) = iter.next() {
            match token {
                Token::LParen => {
                    println!("Left Parenthesis");
                },
                Token::RParen => {
                    println!("Right Parenthesis");
                },
                Token::LBrace => {
                    println!("Left Brace");
                },
                Token::RBrace => {
                    println!("Right Brace");
                },
                Token::LBracket => {
                    println!("Left Bracket");
                },
                Token::RBracket => {
                    println!("Right Bracket");
                },
                Token::Semicolon => {
                    println!("Semicolon");
                },
                Token::Keyword(k) => {
                    println!("Keyword: {}", k);
                },
                Token::Identifier(i) => {
                    println!("Identifier: {}", i);
                },
                Token::Integer(i) => {
                    println!("Integer: {}", i);
                },
            }
        }
    }
}
