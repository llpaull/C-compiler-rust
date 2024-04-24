use crate::lexer::Token;

pub struct Parser{}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<ASTNode, &'static str> {
        let mut iter = tokens.iter();
        let ret = match iter.next() {
            Some(token) => match token {
                Token::Keyword(kw) if kw == "int" => {
                    let name = match iter.next() {
                        Some(Token::Identifier(name)) => name,
                        _ => return Err("Expected identifier"),
                    };
                    let _ = match iter.next() {
                        Some(Token::LParen) => (),
                        _ => return Err("Expected ("),
                    };
                    let _ = match iter.next() {
                        Some(Token::RParen) => (),
                        _ => return Err("Expected )"),
                    };
                    let _ = match iter.next() {
                        Some(Token::LBrace) => (),
                        _ => return Err("Expected {"),
                    };

                    let body = Self::parse_statement(&mut iter)?;
                    ASTNode::Program(Function{name: name.to_string(), body})
                },
                _ => return Err("Expected int"),
            }
            _ => return Err("No tokens"),
        };
        Ok(ret)
    }

    fn parse_statement(iter: &mut std::slice::Iter<Token>) -> Result<Statement, &'static str> {
        match iter.next() {
            Some(Token::Keyword(kw)) if kw == "return" => {
                let expr = Self::parse_expression(iter)?;
                Ok(Statement::Return(expr))
            },
            _ => Err("Expected return"),
        }
    }

    fn parse_expression(iter: &mut std::slice::Iter<Token>) -> Result<Expression, &'static str> {
        match iter.next() {
            Some(Token::Integer(i)) => Ok(Expression::Integer(*i)),
            _ => Err("Expected integer"),
        }
    }
}

#[derive(Debug)]
pub enum ASTNode {
    Program(Function)
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

#[derive(Debug)]
pub enum Expression {
    Integer(i64)
}
