use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    let mut iter = tokens.iter();
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

fn parse_statement(iter: &mut std::slice::Iter<Token>) -> Result<Statement, &'static str> {
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

fn parse_expression(iter: &mut std::slice::Iter<Token>) -> Result<Exp, &'static str> {
    match iter.next() {
        Some(Token::Integer(n)) => Ok(Exp::Integer(*n)),
        _ => Err("Expected return statement to return an int"),
    }
}

#[derive(Debug)]
pub struct Program {pub funcs: Vec<FunDecl>}
impl Program {}

#[derive(Debug)]
pub struct FunDecl {pub name: String, pub body: Vec<Statement>}
impl FunDecl {}

#[derive(Debug)]
pub enum Statement {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp{
    Integer(i64),
}
