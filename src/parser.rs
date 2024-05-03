use std::iter::Peekable;
use crate::lexer::Token;
use crate::lexer::Operation;

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

                    while let Some(exp) = iter.peek() {
                        match exp {
                            Token::RBrace => break,
                            _ => statements.push(parse_statement(&mut iter)?),
                        }
                    }

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
    let ret = match iter.peek() {
        Some(Token::Keyword(kw)) if kw == "return" => {
            iter.next();
            let exp = parse_expression(iter)?;
            Statement::Return(exp)
        },
        Some(Token::Keyword(kw)) if kw == "int" => {
            iter.next();
            let name = match iter.next() {
                Some(Token::Identifier(name)) => name,
                _ => return Err("Expected identifier"),
            };
            match iter.peek() {
                Some(Token::Semicolon) => Statement::Declaration(name.to_string(), None),
                Some(Token::Operator(Operation::Assign)) => {
                    iter.next();
                    Statement::Declaration(name.to_string(), Some(parse_expression(iter)?))
                },
                _ => return Err("Unexpected token when declaring variable"),
            }
        },
        Some(_) => Statement::Expression(parse_expression(iter)?),
        None => return Err("ran out of tokens while parsing statement"),
    };

    match iter.next() {
        Some(Token::Semicolon) => {},
        _ => return Err("Expected semicolon at end of statement"),
    }

    Ok(ret)
}

fn parse_expression(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Exp, &'static str> {
    match iter.peek() {
        Some(Token::Identifier(name)) => {
            let mut clone = iter.clone();
            iter.next();
            match iter.peek() {
                Some(Token::Semicolon) => Ok(Exp::LogicOr(parse_logic_or_exp(&mut clone)?)),
                Some(Token::Operator(op)) => {
                    iter.next();
                    let new_op = match op {
                        Operation::Assign => AssignmentOp::Assignment,
                        _ => return Err("Expected assignment operator"),
                    };
                    Ok(Exp::Operator(new_op, name.to_string(), Box::new(parse_expression(iter)?)))
                },
                _ => Err("Expected operator or semicolon"),
            }
        },
        Some(_) => Ok(Exp::LogicOr(parse_logic_or_exp(iter)?)),
        None => Err("ran out of tokens while parsing expression"),
    }
}

fn parse_logic_or_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<LogicOrExp, &'static str> {
    let mut ret = LogicOrExp::LogicAnd(parse_logic_and_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::LogicalOr => {
                    iter.next();
                    let new_op = LogicOrOp::LogicOr;
                    let next = LogicOrExp::LogicAnd(parse_logic_and_exp(iter)?);
                    ret = LogicOrExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_logic_and_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<LogicAndExp, &'static str> {
    let mut ret = LogicAndExp::BitOr(parse_bit_or_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::LogicalAnd => {
                    iter.next();
                    let new_op = LogicAndOp::LogicAnd;
                    let next = LogicAndExp::BitOr(parse_bit_or_exp(iter)?);
                    ret = LogicAndExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_bit_or_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<BitOrExp, &'static str> {
    let mut ret = BitOrExp::BitXor(parse_bit_xor_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::BitOr => {
                    iter.next();
                    let next = BitOrExp::BitXor(parse_bit_xor_exp(iter)?);
                    ret = BitOrExp::Operator(BitOrOp::BitOr, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_bit_xor_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<BitXorExp, &'static str> {
    let mut ret = BitXorExp::BitAnd(parse_bit_and_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::BitXor => {
                    iter.next();
                    let next = BitXorExp::BitAnd(parse_bit_and_exp(iter)?);
                    ret = BitXorExp::Operator(BitXorOp::BitXor, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_bit_and_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<BitAndExp, &'static str> {
    let mut ret = BitAndExp::Equality(parse_equality_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::BitAnd => {
                    iter.next();
                    let next = BitAndExp::Equality(parse_equality_exp(iter)?);
                    ret = BitAndExp::Operator(BitAndOp::BitAnd, Box::new(ret), Box::new(next));
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
            Token::Operator(op) => match op {
                Operation::Equal | Operation::NotEqual => {
                    iter.next();
                    let new_op = match op {
                        Operation::Equal => EqualityOp::Equals,
                        Operation::NotEqual => EqualityOp::NotEquals,
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
    let mut ret = RelExp::Shift(parse_shift_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::LessThan | Operation::LessThanOrEqual | Operation::GreaterThan | Operation::GreaterThanOrEqual => {
                    iter.next();
                    let new_op = match op {
                        Operation::LessThan => RelationOp::LessThan,
                        Operation::LessThanOrEqual => RelationOp::LessEqual,
                        Operation::GreaterThan => RelationOp::GreaterThan,
                        Operation::GreaterThanOrEqual => RelationOp::GreaterEqual,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = RelExp::Shift(parse_shift_exp(iter)?);
                    ret = RelExp::Operator(new_op, Box::new(ret), Box::new(next));
                }
                _ => break,
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_shift_exp(iter : &mut Peekable<std::slice::Iter<Token>>) -> Result<ShiftExp, &'static str> {
    let mut ret = ShiftExp::Additive(parse_additive_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(op) => match op {
                Operation::LeftShift | Operation::RightShift => {
                    iter.next();
                    let new_op = match op {
                        Operation::LeftShift => ShiftOp::LShift,
                        Operation::RightShift => ShiftOp::RShift,
                        _ => return Err("somehow no match although it matched"),
                    };
                    let next = ShiftExp::Additive(parse_additive_exp(iter)?);
                    ret = ShiftExp::Operator(new_op, Box::new(ret), Box::new(next));
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
            Token::Operator(op) => match op {
                Operation::Plus | Operation::Minus => {
                    iter.next();
                    let new_op = match op {
                        Operation::Plus => AdditiveOp::Add,
                        Operation::Minus => AdditiveOp::Sub,
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
            Token::Operator(op) => match op {
                Operation::Mult | Operation::Div | Operation::Mod => {
                    iter.next();
                    let new_op = match op {
                        Operation::Mult => TermOp::Mult,
                        Operation::Div => TermOp::Div,
                        Operation::Mod => TermOp::Mod,
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
            match op {
                Operation::Minus => {
                    let factor = parse_factor(iter)?;
                    Ok(Factor::Operator(FactorOp::Negate, Box::new(factor)))
                },
                Operation::LogicalNot => {
                    let factor = parse_factor(iter)?;
                    Ok(Factor::Operator(FactorOp::LogicalNot, Box::new(factor)))
                },
                Operation::BitNot => {
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
        Some(Token::Identifier(name)) => Ok(Factor::Variable(name.to_string())),
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
    Declaration(String, Option<Exp>),
    Expression(Exp),
}

#[derive(Debug)]
pub enum Exp {
    LogicOr(LogicOrExp),
    Operator(AssignmentOp, String, Box<Exp>),
}

#[derive(Debug)]
pub enum LogicOrExp {
    LogicAnd(LogicAndExp),
    Operator(LogicOrOp, Box<LogicOrExp>, Box<LogicOrExp>),
}

#[derive(Debug)]
pub enum LogicAndExp {
    BitOr(BitOrExp),
    Operator(LogicAndOp, Box<LogicAndExp>, Box<LogicAndExp>),
}

#[derive(Debug)]
pub enum BitOrExp {
    BitXor(BitXorExp),
    Operator(BitOrOp, Box<BitOrExp>, Box<BitOrExp>),
}

#[derive(Debug)]
pub enum BitXorExp {
    BitAnd(BitAndExp),
    Operator(BitXorOp, Box<BitXorExp>, Box<BitXorExp>),
}

#[derive(Debug)]
pub enum BitAndExp {
    Equality(EqualityExp),
    Operator(BitAndOp, Box<BitAndExp>, Box<BitAndExp>),
}

#[derive(Debug)]
pub enum EqualityExp {
    Rel(RelExp),
    Operator(EqualityOp, Box<EqualityExp>, Box<EqualityExp>),
}

#[derive(Debug)]
pub enum RelExp {
    Shift(ShiftExp),
    Operator(RelationOp, Box<RelExp>, Box<RelExp>),
}

#[derive(Debug)]
pub enum ShiftExp {
    Additive(AdditiveExp),
    Operator(ShiftOp, Box<ShiftExp>, Box<ShiftExp>),
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
    Variable(String),
    Operator(FactorOp, Box<Factor>),
    Paren(Box<Exp>),
}

#[derive(Debug)]
pub enum FactorOp {
    Negate,
    LogicalNot,
    BitNot,
}

#[derive(Debug)]
pub enum TermOp {
    Mult,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum AdditiveOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum ShiftOp {
    LShift,
    RShift,
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
pub enum BitAndOp {
    BitAnd,
}

#[derive(Debug)]
pub enum BitXorOp {
    BitXor,
}

#[derive(Debug)]
pub enum BitOrOp {
    BitOr,
}

#[derive(Debug)]
pub enum LogicAndOp {
    LogicAnd,
}

#[derive(Debug)]
pub enum LogicOrOp {
    LogicOr,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum AssignmentOp {
    Assignment,
    Mult,
    Div,
    Mod,
    Plus,
    Sub,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
}
