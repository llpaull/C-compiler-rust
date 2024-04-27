use std::iter::Peekable;
use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    let mut iter = tokens.iter().peekable();
    match iter.next() {
        Some(token) => {
            match token {
                Token::Keyword(kw) if kw == "int" => {
                    let name = match iter.next() {
                        Some(Token::Identifier(name)) => name,
                        _ => return Err("Expected function name"),
                    };
                    match iter.next() {
                        Some(Token::LParen) => {},
                        _ => return Err("Expected left parenthesis"),
                    }
                    match iter.next() {
                        Some(Token::RParen) => {},
                        _ => return Err("Expected right parenthesis"),
                    }
                    match iter.next() {
                        Some(Token::LBrace) => {},
                        _ => return Err("Expected left brace"),
                    }
                    let mut statements: Vec<Statement> = Vec::new();
                    let statement = parse_statement(&mut iter)?;
                    statements.push(statement);
                    match iter.next() {
                        Some(Token::RBrace) => {},
                        _ => return Err("Expected right brace"),
                    }
                    let mut funcs: Vec<FunDecl> = Vec::new();
                    let func = FunDecl {name: name.to_string(), body: statements};
                    funcs.push(func);
                    Ok(Program {funcs})
                },
                _ => return Err("Expected int"),
            }
        },
        None => return Err("No tokens found"),
    }
}

fn parse_statement(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Statement, &'static str> {
    match iter.next() {
        Some(Token::Keyword(kw)) if kw == "return" => {
            let exp = parse_expression(iter)?;
            match iter.next() {
                Some(Token::Semicolon) => Ok(Statement::Return(exp)),
                _ => Err("expected semicolon at end of statement"),
            }
        },
        _ => Err("Expected return statement"),
    }
}

fn parse_expression(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Exp, &'static str> {
    let mut term = parse_term(iter)?;
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => {
                match op.as_str() {
                    "+" | "-" => {
                        iter.next();
                        let op = match op.as_str() {
                            "+" => BinOp::Add,
                            "-" => BinOp::Sub,
                            _ => return Err("How the fuck did this return an error"),
                        };
                        let next_term = parse_term(iter)?;
                        term = Term::BinOp(op, Box::new(term), Box::new(next_term));
                    },
                    _ => break,
                }
            },
            _ => break,
        }
    }
    Ok(Exp::Term(term))
}

fn parse_term(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Term, &'static str> {
    let mut factor = parse_factor(iter)?;
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => {
                match op.as_str() {
                    "*" | "/" => {
                        iter.next();
                        let op = match op.as_str() {
                            "*" => BinOp::Mul,
                            "/" => BinOp::Div,
                            _ => return Err("How the fuck did we get to THIS error"),
                        };
                        let next_factor = parse_factor(iter)?;
                        factor = Factor::BinOp(op, Box::new(factor), Box::new(next_factor));
                    },
                    _ => break,
                }
            },
            _ => break,
        }
    }
    Ok(Term::Factor(factor))
}

fn parse_factor(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Factor, &'static str> {
    match iter.next() {
        Some(token) => match token {
            Token::LParen => {
                let exp = parse_expression(iter)?;
                match iter.next() {
                    Some(Token::RParen) => {},
                    _ => return Err("unclosed braces"),
                }
                Ok(Factor::Paren(Box::new(exp)))
            },
            Token::Operator(op) => match op.as_str() {
                "-" => Ok(Factor::UnOp(UnOp::Neg, Box::new(parse_factor(iter)?))),
                "~" => Ok(Factor::UnOp(UnOp::BitNot, Box::new(parse_factor(iter)?))),
                "!" => Ok(Factor::UnOp(UnOp::Not, Box::new(parse_factor(iter)?))),
                _ => Err("BinOp inside of factor not possible"),
            },
            Token::Integer(n) => Ok(Factor::Integer(*n)),
            _ => Err("Error parsing factor, incorrect token"),
        },
        _ => Err("Reached end of tokens while parsing factor"),
    }
}

#[derive(Debug)]
pub struct Program {pub funcs: Vec<FunDecl>}

#[derive(Debug)]
pub struct FunDecl {pub name: String, pub body: Vec<Statement>}

#[derive(Debug)]
pub enum Statement {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
    Term(Term),
}

#[derive(Debug)]
pub enum Term {
    Factor(Factor),
    BinOp(BinOp, Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum Factor {
    Integer(i64),
    Paren(Box<Exp>),
    UnOp(UnOp, Box<Factor>),
    BinOp(BinOp, Box<Factor>, Box<Factor>),
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

