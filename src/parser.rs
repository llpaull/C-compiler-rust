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
    let mut ret = Exp::LogicAnd(parse_logic_and_exp(iter)?); 
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                "||" => {
                    iter.next();
                    let new_op = LogicOrOp::LogicOr;
                    let next = Exp::LogicAnd(parse_logic_and_exp(iter)?);
                    ret = Exp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_logic_and_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<LogicAndExp, &'static str> {
    let mut ret = LogicAndExp::Equality(parse_equality_exp(iter)?); 
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                "&&" => {
                    iter.next();
                    let new_op = LogicAndOp::LogicAnd;
                    let next = LogicAndExp::Equality(parse_equality_exp(iter)?);
                    ret = LogicAndExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_equality_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<EqualityExp, &'static str> {
    let mut ret = EqualityExp::Rel(parse_rel_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                s@("==" | "!=") => {
                    iter.next();
                    let new_op = match s {
                        "==" => EqualityOp::Equals,
                        "!=" => EqualityOp::NotEquals,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = EqualityExp::Rel(parse_rel_exp(iter)?);
                    ret = EqualityExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_rel_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<RelExp, &'static str> {
    let mut ret = RelExp::Additive(parse_additive_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                s@("<" | ">" | "<=" | ">=") => {
                    iter.next();
                    let new_op = match s {
                        "<" => RelationOp::LessThan,
                        ">" => RelationOp::GreaterThan,
                        "<=" => RelationOp::LessEqual,
                        ">=" => RelationOp::GreaterEqual,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = RelExp::Additive(parse_additive_exp(iter)?);
                    ret = RelExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_additive_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<AdditiveExp, &'static str> {
    let mut ret = AdditiveExp::Term(parse_term(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                s@("+" | "-") => {
                    iter.next();
                    let new_op = match s {
                        "+" => AdditiveOp::Add,
                        "-" => AdditiveOp::Sub,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = AdditiveExp::Term(parse_term(iter)?);
                    ret = AdditiveExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_term(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Term, &'static str> {
    let mut ret = Term::Factor(parse_factor(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match *op {
                s@("*" | "/") => {
                    iter.next();
                    let new_op = match s {
                        "*" => TermOp::Mult,
                        "/" => TermOp::Div,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = Term::Factor(parse_factor(iter)?);
                    ret = Term::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_factor(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Factor, &'static str> {
    match iter.next() {
        Some(Token::Integer(i)) => Ok(Factor::Integer(*i)),
        Some(Token::Operator(op)) => {
            match *op {
                "-" => {
                    let factor = parse_factor(iter)?;
                    Ok(Factor::Operator(FactorOp::Neg, Box::new(factor)))
                },
                "!" => {
                    let factor = parse_factor(iter)?;
                    Ok(Factor::Operator(FactorOp::Not, Box::new(factor)))
                },
                "~" => {
                    let factor = parse_factor(iter)?;
                    Ok(Factor::Operator(FactorOp::BitNot, Box::new(factor)))
                },
                _ => Err("Expected unary operator"),
            }
        },
        Some(Token::LParen) => {
            let exp = parse_expression(iter)?;
            match iter.next() {
                Some(Token::RParen) => Ok(Factor::Paren(Box::new(exp))),
                _ => Err("Expected right parenthesis"),
            }
        },
        _ => Err("Expected factor"),
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
    LogicAnd(LogicAndExp),
    Operator(LogicOrOp, Box<Exp>, Box<Exp>),
}

#[derive(Debug)]
pub enum LogicAndExp {
    Equality(EqualityExp),
    Operator(LogicAndOp, Box<LogicAndExp>, Box<LogicAndExp>),
}

#[derive(Debug)]
pub enum EqualityExp {
    Rel(RelExp),
    Operator(EqualityOp, Box<EqualityExp>, Box<EqualityExp>),
}

#[derive(Debug)]
pub enum RelExp {
    Additive(AdditiveExp),
    Operator(RelationOp, Box<RelExp>, Box<RelExp>),
}

#[derive(Debug)]
pub enum AdditiveExp {
    Term(Term),
    Operator(AdditiveOp, Box<AdditiveExp>, Box<AdditiveExp>),
}

#[derive(Debug)]
pub enum Term {
    Factor(Factor),
    Operator(TermOp, Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum Factor {
    Integer(i64),
    Operator(FactorOp, Box<Factor>),
    Paren(Box<Exp>),
}

#[derive(Debug)]
pub enum FactorOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug)]
pub enum TermOp {
    Mult,
    Div,
}

#[derive(Debug)]
pub enum AdditiveOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum RelationOp {
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

#[derive(Debug)]
pub enum EqualityOp {
    Equals,
    NotEquals,
}

#[derive(Debug)]
pub enum LogicAndOp {
    LogicAnd,
}

#[derive(Debug)]
pub enum LogicOrOp {
    LogicOr,
}
