use std::iter::Peekable;
use crate::lexer::{self, Token, Operation, BasicOp};

pub fn parse(tokens: &Vec<Token>) -> Result<Program, &'static str> {
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
                Some(Token::Operator(Operation::Assignment(lexer::AssignmentOp::Assign))) => {
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
        _ => return Err("unknown token at end of statement"),
    }

    Ok(ret)
}

fn parse_expression(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Exp, &'static str> {
    let mut ret = Exp::Assignment(parse_assignment_expression(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Comma => {
                iter.next();
                let next = Exp::Assignment(parse_assignment_expression(iter)?);
                ret = Exp::Operator(CommaOp::Comma, Box::new(ret), Box::new(next));
            },
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_assignment_expression(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<AssignmentExp, &'static str> {
    match iter.peek() {
        Some(Token::Identifier(name)) => {
            let mut clone = iter.clone();
            clone.next();

            match clone.peek() {
                Some(Token::Operator(Operation::Assignment(op_token))) => {
                    let op = match op_token {
                        lexer::AssignmentOp::Assign => AssignmentOp::Assign,
                        lexer::AssignmentOp::Mult => AssignmentOp::Mult,
                        lexer::AssignmentOp::Div => AssignmentOp::Div,
                        lexer::AssignmentOp::Mod => AssignmentOp::Mod,
                        lexer::AssignmentOp::Plus => AssignmentOp::Plus,
                        lexer::AssignmentOp::Minus => AssignmentOp::Sub,
                        lexer::AssignmentOp::LShift => AssignmentOp::LShift,
                        lexer::AssignmentOp::RShift => AssignmentOp::RShift,
                        lexer::AssignmentOp::BitAnd => AssignmentOp::BitAnd,
                        lexer::AssignmentOp::BitOr => AssignmentOp::BitOr,
                        lexer::AssignmentOp::BitXor => AssignmentOp::BitXor,
                        lexer::AssignmentOp::Increment => AssignmentOp::PostInc,
                        lexer::AssignmentOp::Decrement => AssignmentOp::PostDec,
                    };
                    match op {
                        AssignmentOp::PostInc | AssignmentOp::PostDec => {
                            let var = AssignmentExp::LogicOr(parse_logic_or_exp(iter)?); // should only take one (1) token
                            iter.next();
                            Ok(AssignmentExp::Operator(op, name.to_string(), Box::new(var)))
                        },
                        _ => {
                            iter.next();
                            iter.next();
                            Ok(AssignmentExp::Operator(op, name.to_string(), Box::new(parse_assignment_expression(iter)?)))
                        },
                    }
                },
                _ => Ok(AssignmentExp::LogicOr(parse_logic_or_exp(iter)?)),
            }
        },
        _ => Ok(AssignmentExp::LogicOr(parse_logic_or_exp(iter)?)),
    }
}

fn parse_logic_or_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<LogicOrExp, &'static str> {
    let mut ret = LogicOrExp::LogicAnd(parse_logic_and_exp(iter)?);
    while let Some(token) = iter.peek() {
        match token {
            Token::Operator(Operation::Basic(BasicOp::LogicalOr)) => {
                iter.next();
                let next = LogicOrExp::LogicAnd(parse_logic_and_exp(iter)?);
                ret = LogicOrExp::Operator(LogicOrOp::LogicOr, Box::new(ret), Box::new(next));
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
            Token::Operator(Operation::Basic(BasicOp::LogicalAnd)) => {
                iter.next();
                let next = LogicAndExp::BitOr(parse_bit_or_exp(iter)?);
                ret = LogicAndExp::Operator(LogicAndOp::LogicAnd, Box::new(ret), Box::new(next));
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
            Token::Operator(Operation::Basic(BasicOp::BitOr)) => {
                iter.next();
                let next = BitOrExp::BitXor(parse_bit_xor_exp(iter)?);
                ret = BitOrExp::Operator(BitOrOp::BitOr, Box::new(ret), Box::new(next));
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
            Token::Operator(Operation::Basic(BasicOp::BitXor)) => {
                iter.next();
                let next = BitXorExp::BitAnd(parse_bit_and_exp(iter)?);
                ret = BitXorExp::Operator(BitXorOp::BitXor, Box::new(ret), Box::new(next));
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
            Token::Operator(Operation::Basic(BasicOp::BitAnd)) => {
                iter.next();
                let next = BitAndExp::Equality(parse_equality_exp(iter)?);
                ret = BitAndExp::Operator(BitAndOp::BitAnd, Box::new(ret), Box::new(next));
            }
            _ => break,
        }
    }
    Ok(ret)
}

fn parse_equality_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<EqualityExp, &'static str> {
    let mut ret = EqualityExp::Rel(parse_rel_exp(iter)?);
    while let Some(token) = iter.peek() {
        let op = match token {
            Token::Operator(Operation::Basic(BasicOp::Equal)) => EqualityOp::Equals,
            Token::Operator(Operation::Basic(BasicOp::NotEqual)) => EqualityOp::NotEquals,
            _ => break,
        };
        iter.next();
        let next = EqualityExp::Rel(parse_rel_exp(iter)?);
        ret = EqualityExp::Operator(op, Box::new(ret), Box::new(next));
    }
    Ok(ret)
}

fn parse_rel_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<RelExp, &'static str> {
    let mut ret = RelExp::Shift(parse_shift_exp(iter)?);
    while let Some(token) = iter.peek() {
        let op = match token {
            Token::Operator(Operation::Basic(BasicOp::LessThan)) => RelationOp::LessThan,
            Token::Operator(Operation::Basic(BasicOp::LessEqual)) => RelationOp::LessEqual,
            Token::Operator(Operation::Basic(BasicOp::GreaterThan)) => RelationOp::GreaterThan,
            Token::Operator(Operation::Basic(BasicOp::GreaterEqual)) => RelationOp::GreaterEqual,
            _ => break,
        };
        iter.next();
        let next = RelExp::Shift(parse_shift_exp(iter)?);
        ret = RelExp::Operator(op, Box::new(ret), Box::new(next));
    }
    Ok(ret)
}

fn parse_shift_exp(iter : &mut Peekable<std::slice::Iter<Token>>) -> Result<ShiftExp, &'static str> {
    let mut ret = ShiftExp::Additive(parse_additive_exp(iter)?);
    while let Some(token) = iter.peek() {
        let op = match token {
            Token::Operator(Operation::Basic(BasicOp::LShift)) => ShiftOp::LShift,
            Token::Operator(Operation::Basic(BasicOp::RShift)) => ShiftOp::RShift,
            _ => break,
        };
        iter.next();
        let next = ShiftExp::Additive(parse_additive_exp(iter)?);
        ret = ShiftExp::Operator(op, Box::new(ret), Box::new(next));
    }
    Ok(ret)
}

fn parse_additive_exp(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<AdditiveExp, &'static str> {
    let mut ret = AdditiveExp::Term(parse_term(iter)?);
    while let Some(token) = iter.peek() {
        let op = match token {
            Token::Operator(Operation::Basic(BasicOp::Plus)) => AdditiveOp::Add,
            Token::Operator(Operation::Basic(BasicOp::Minus)) => AdditiveOp::Sub,
            _ => break,
        };
        iter.next();
        let next = AdditiveExp::Term(parse_term(iter)?);
        ret = AdditiveExp::Operator(op, Box::new(ret), Box::new(next));
    }
    Ok(ret)
}

fn parse_term(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Term, &'static str> {
    let mut ret = Term::Factor(parse_factor(iter)?);
    while let Some(token) = iter.peek() {
        let op = match token {
            Token::Operator(Operation::Basic(BasicOp::Mult)) => TermOp::Mult,
            Token::Operator(Operation::Basic(BasicOp::Div)) => TermOp::Div,
            Token::Operator(Operation::Basic(BasicOp::Mod)) => TermOp::Mod,
            _ => break,
        };
        iter.next();
        let next = Term::Factor(parse_factor(iter)?);
        ret = Term::Operator(op, Box::new(ret), Box::new(next));
    }
    Ok(ret)
}

fn parse_factor(iter: &mut Peekable<std::slice::Iter<Token>>) -> Result<Factor, &'static str> {
    match iter.next() {
        Some(Token::Integer(i)) => Ok(Factor::Integer(*i)),
        Some(Token::Operator(Operation::Basic(op))) => {
            let op = match op {
                BasicOp::Minus => FactorOp::Negate,
                BasicOp::LogicalNot => FactorOp::LogicalNot,
                BasicOp::BitNot => FactorOp::BitNot,
                _ => return Err("Expected unary operator"),
            };
            let factor = parse_factor(iter)?;
            Ok(Factor::Operator(op, Box::new(factor)))
        },
        Some(Token::Operator(Operation::Assignment(op))) => {
            let op = match op {
                lexer::AssignmentOp::Increment => FactorOp::PreInc,
                lexer::AssignmentOp::Decrement => FactorOp::PreDec,
                _ => return Err("Expected unary operator"),
            };
            let factor = parse_factor(iter)?;
            match factor {
                Factor::Variable(_) => {},
                _ => return Err("cannot increment non-variable"),
            }
            // prefix increment/decrement
            Ok(Factor::Operator(op, Box::new(factor)))
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
    Assignment(AssignmentExp),
    Operator(CommaOp, Box<Exp>, Box<Exp>),
}

#[derive(Debug)]
pub enum AssignmentExp {
    LogicOr(LogicOrExp),
    Operator(AssignmentOp, String, Box<AssignmentExp>),
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
    PreInc,
    PreDec,
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

#[derive(Debug)]
pub enum AssignmentOp {
    Assign,
    Mult,
    Div,
    Mod,
    Plus,
    PostInc,
    Sub,
    PostDec,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Debug)]
pub enum CommaOp {
    Comma,
}
