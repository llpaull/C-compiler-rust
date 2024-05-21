use super::ast::*;
use super::token::*;
use std::error::Error;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser {
    iter: Peekable<IntoIter<Token>>,
}

// helper functions
impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            iter: tokens.into_iter().peekable(),
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.iter.peek()
    }

    fn drop(&mut self) {
        self.iter.next();
    }

    fn match_token(&mut self, token: Token) -> Result<(), String> {
        match self.next() {
            Some(ref t) if t == &token => Ok(()),
            err => Err(format!(
                "Token {:?} not found, instead found {:?}",
                token, err
            )),
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Result<(), String> {
        match self.next() {
            Some(Token::Keyword(ref kw)) if kw == &keyword => Ok(()),
            err => Err(format!(
                "Keyword {:?} not found, instead found {:?}",
                keyword, err
            )),
        }
    }

    fn peek_match_token(&mut self, token: Token) -> Result<(), String> {
        match self.peek() {
            Some(ref t) if t == &&token => Ok(()),
            err => Err(format!(
                "Token {:?} not found, instead found {:?}",
                token, err
            )),
        }
    }

    fn get_identifier(&mut self) -> Result<String, String> {
        match self.next() {
            Some(Token::Identifier(name)) => Ok(name),
            err => Err(format!("Expected identifier but found {:?}", err)),
        }
    }
}

// parsing functions
impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Program, Box<dyn Error>> {
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, Box<dyn Error>> {
        let mut funcs = vec![];
        while self.peek().is_some() {
            funcs.push(self.parse_function()?);
        }

        Ok(Program { funcs })
    }

    fn parse_function(&mut self) -> Result<Function, Box<dyn Error>> {
        self.match_keyword(Keyword::Int)?;
        let name = self.get_identifier()?;
        self.match_token(Token::OpenParenthesis)?;
        self.match_token(Token::CloseParenthesis)?;
        self.match_token(Token::OpenBrace)?;

        let mut body = vec![];

        while let Err(_) = self.peek_match_token(Token::CloseBrace) {
            body.push(self.parse_statement()?);
        }

        self.match_token(Token::CloseBrace)?;

        Ok(Function { name, body })
    }

    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        match self.peek() {
            Some(Token::Keyword(Keyword::Return)) => {
                self.drop();
                let exp = self.parse_expression()?;
                match exp {
                    Expression::Null => return Err("Cannot return nothing, for now".into()),
                    _ => {}
                }
                self.match_token(Token::Semicolon)?;
                Ok(Statement::Return(exp))
            }
            Some(Token::Keyword(Keyword::Int)) => {
                self.drop();
                let ret = self.parse_declaration();
                self.match_token(Token::Semicolon)?;
                ret
            }
            Some(Token::Keyword(Keyword::If)) => {
                self.drop();
                self.parse_if_statement()
            }
            Some(Token::OpenBrace) => {
                self.drop();
                let ret = self.parse_compound_statement();
                self.match_token(Token::CloseBrace)?;
                ret
            }
            Some(Token::Keyword(Keyword::For)) => {
                self.drop();
                self.match_token(Token::OpenParenthesis)?;
                let declaration = self.peek_match_token(Token::Keyword(Keyword::Int)).is_ok();

                if declaration {
                    self.drop();
                    let decl = self.parse_declaration()?;
                    match decl {
                        Statement::Declaration(_, _) => {}
                        _ => {
                            return Err(
                                format!("Declaration not a declaration, instead {:?}", decl).into()
                            )
                        }
                    }
                        self.match_token(Token::Semicolon)?;
                    let cond = self.parse_expression()?;
                    self.match_token(Token::Semicolon)?;
                    let post = self.parse_expression()?;
                    self.match_token(Token::CloseParenthesis)?;
                    let body = self.parse_statement()?;
                    Ok(Statement::ForDecl(
                        Box::new(decl),
                        cond,
                        post,
                        Box::new(body),
                    ))
                } else {
                    let exp = self.parse_expression()?;
                    self.match_token(Token::Semicolon)?;
                    let cond = self.parse_expression()?;
                    self.match_token(Token::Semicolon)?;
                    let post = self.parse_expression()?;
                    self.match_token(Token::CloseParenthesis)?;
                    let body = self.parse_statement()?;
                    Ok(Statement::For(exp, cond, post, Box::new(body)))
                }
            }
            Some(Token::Keyword(Keyword::While)) => {
                self.drop();
                self.match_token(Token::OpenParenthesis)?;
                let exp = self.parse_expression()?;
                self.match_token(Token::CloseParenthesis)?;
                let body = self.parse_statement()?;
                Ok(Statement::While(exp, Box::new(body)))
            }
            Some(Token::Keyword(Keyword::Do)) => {
                self.drop();
                let body = self.parse_statement()?;
                self.match_token(Token::Keyword(Keyword::While))?;
                self.match_token(Token::OpenParenthesis)?;
                let exp = self.parse_expression()?;
                self.match_token(Token::CloseParenthesis)?;
                self.match_token(Token::Semicolon)?;
                Ok(Statement::Do(Box::new(body), exp))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                self.drop();
                self.match_token(Token::Semicolon)?;
                Ok(Statement::Break)
            }
            Some(Token::Keyword(Keyword::Continue)) => {
                self.drop();
                self.match_token(Token::Semicolon)?;
                Ok(Statement::Continue)
            }
            Some(_) => {
                let exp = self.parse_expression()?;
                self.match_token(Token::Semicolon)?;
                Ok(Statement::Expression(exp))
            }
            None => Err("Ran out of tokens while parsing function body".into()),
        }
    }

    fn parse_declaration(&mut self) -> Result<Statement, Box<dyn Error>> {
        match (self.next(), self.peek()) {
            (Some(Token::Identifier(name)), Some(Token::Semicolon)) => {
                Ok(Statement::Declaration(name, None))
            }
            (Some(Token::Identifier(name)), Some(Token::Assign)) => {
                self.drop();
                let exp = self.parse_expression()?;
                Ok(Statement::Declaration(name, Some(exp)))
            }
            err => Err(format!("Expected either ; or = but got {:?}", err).into()),
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        self.match_token(Token::OpenParenthesis)?;
        let condition = self.parse_expression()?;
        self.match_token(Token::CloseParenthesis)?;
        let if_body = Box::new(self.parse_statement()?);

        match *if_body {
            Statement::Declaration(_, _) => {
                return Err("cannot have variable declaration inside unscoped if statement".into())
            }
            _ => {}
        }

        match self.peek() {
            Some(Token::Keyword(Keyword::Else)) => {
                self.drop();
                let else_body = Box::new(self.parse_statement()?);

                match *else_body {
                    Statement::Declaration(_, _) => {
                        return Err(
                            "cannot have variable declaration inside unscoped else statement"
                                .into(),
                        )
                    }
                    _ => {}
                }

                Ok(Statement::If(condition, if_body, Some(else_body)))
            }
            _ => Ok(Statement::If(condition, if_body, None)),
        }
    }

    fn parse_compound_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let mut statements = vec![];
        while let Some(token) = self.peek() {
            match *token {
                Token::CloseBrace => break,
                _ => statements.push(self.parse_statement()?),
            }
        }

        Ok(Statement::Compound(statements))
    }

    fn parse_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        if let Ok(_) = self.peek_match_token(Token::Semicolon) {
            return Ok(Expression::Null);
        }
        if let Ok(_) = self.peek_match_token(Token::CloseParenthesis) {
            return Ok(Expression::Null);
        }
        self.parse_comma_expression()
    }

    fn parse_comma_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::Comma], Self::parse_assignment_expression)
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        match self.peek() {
            Some(Token::Identifier(_)) => self.parse_assign(),
            _ => self.parse_conditional_expression(),
        }
    }

    fn parse_assign(&mut self) -> Result<Expression, Box<dyn Error>> {
        let var = self.parse_conditional_expression()?;
        match var {
            Expression::Var(ref varname) => {
                let op = match self.peek() {
                    Some(Token::AssignMult) => BinOp::Multiplication,
                    Some(Token::AssignDiv) => BinOp::Division,
                    Some(Token::AssignMod) => BinOp::Modulus,
                    Some(Token::AssignAdd) => BinOp::Addition,
                    Some(Token::AssignSub) => BinOp::Subtraction,
                    Some(Token::AssignShiftLeft) => BinOp::ShiftLeft,
                    Some(Token::AssignShiftRight) => BinOp::ShiftRight,
                    Some(Token::AssignBitAnd) => BinOp::BitwiseAnd,
                    Some(Token::AssignBitOr) => BinOp::BitwiseOr,
                    Some(Token::AssignBitXor) => BinOp::BitwiseXor,
                    Some(Token::Assign) => {
                        self.drop();
                        return Ok(Expression::Assign(
                            varname.clone(),
                            Box::new(self.parse_expression()?),
                        ));
                    }
                    _ => return Ok(var),
                };
                self.drop();
                let exp = Expression::Assign(
                    varname.clone(),
                    Box::new(Expression::BinOp(
                        op,
                        Box::new(Expression::Var(varname.clone())),
                        Box::new(self.parse_expression()?),
                    )),
                );
                Ok(exp)
            }
            _ => Ok(var),
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        let cond = self.parse_logicor_expression()?;
        match self.peek() {
            Some(Token::QuestionMark) => {
                self.drop();
                let if_body = self.parse_expression()?;
                self.match_token(Token::Colon)?;
                let else_body = self.parse_conditional_expression()?;
                Ok(Expression::Ternary(
                    Box::new(cond),
                    Box::new(if_body),
                    Box::new(else_body),
                ))
            }
            _ => Ok(cond),
        }
    }

    fn parse_logicor_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::LogicOr], Self::parse_logicand_expression)
    }

    fn parse_logicand_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::LogicAnd], Self::parse_bitor_expression)
    }

    fn parse_bitor_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::BitwiseOr], Self::parse_bitxor_expression)
    }

    fn parse_bitxor_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::BitwiseXor], Self::parse_bitand_expression)
    }

    fn parse_bitand_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(&[Token::BitwiseAnd], Self::parse_equality_expression)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(
            &[Token::Equal, Token::NotEqual],
            Self::parse_relational_expression,
        )
    }

    fn parse_relational_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(
            &[
                Token::LessThan,
                Token::LessThanOrEqual,
                Token::GreaterThan,
                Token::GreaterThanOrEqual,
            ],
            Self::parse_shift_expression,
        )
    }

    fn parse_shift_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(
            &[Token::ShiftLeft, Token::ShiftRight],
            Self::parse_additive_expression,
        )
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(
            &[Token::Addition, Token::Negation],
            Self::parse_multiplicative_expression,
        )
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.parse_generic_expression(
            &[Token::Multiplication, Token::Division, Token::Modulus],
            Self::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> Result<Expression, Box<dyn Error>> {
        match (self.next(), self.peek()) {
            (Some(Token::Integer(i)), _) => Ok(Expression::Num(i)),
            (Some(Token::Identifier(ref name)), Some(Token::Increment)) => {
                self.drop();
                Ok(Expression::BinOp(
                    BinOp::Comma,
                    Box::new(Expression::Assign(
                        name.clone(),
                        Box::new(Expression::BinOp(
                            BinOp::Addition,
                            Box::new(Expression::Var(name.clone())),
                            Box::new(Expression::Num(1)),
                        )),
                    )),
                    Box::new(Expression::BinOp(
                        BinOp::Subtraction,
                        Box::new(Expression::Var(name.clone())),
                        Box::new(Expression::Num(1)),
                    )),
                ))
            }
            (Some(Token::Identifier(ref name)), Some(Token::Decrement)) => {
                self.drop();
                Ok(Expression::BinOp(
                    BinOp::Comma,
                    Box::new(Expression::Assign(
                        name.clone(),
                        Box::new(Expression::BinOp(
                            BinOp::Subtraction,
                            Box::new(Expression::Var(name.clone())),
                            Box::new(Expression::Num(1)),
                        )),
                    )),
                    Box::new(Expression::BinOp(
                        BinOp::Addition,
                        Box::new(Expression::Var(name.clone())),
                        Box::new(Expression::Num(1)),
                    )),
                ))
            }
            (Some(Token::Increment), Some(Token::Identifier(_))) => {
                let name = self.get_identifier()?;
                Ok(Expression::Assign(
                    name.clone(),
                    Box::new(Expression::BinOp(
                        BinOp::Addition,
                        Box::new(Expression::Var(name.clone())),
                        Box::new(Expression::Num(1)),
                    )),
                ))
            }
            (Some(Token::Decrement), Some(Token::Identifier(_))) => {
                let name = self.get_identifier()?;
                Ok(Expression::Assign(
                    name.clone(),
                    Box::new(Expression::BinOp(
                        BinOp::Subtraction,
                        Box::new(Expression::Var(name.clone())),
                        Box::new(Expression::Num(1)),
                    )),
                ))
            }
            (Some(Token::OpenParenthesis), _) => {
                let exp = self.parse_expression();
                self.match_token(Token::CloseParenthesis)?;
                exp
            }
            (Some(Token::Identifier(name)), _) => Ok(Expression::Var(name)),
            (Some(op @ Token::Negation), _)
            | (Some(op @ Token::LogicNot), _)
            | (Some(op @ Token::BitNot), _) => {
                let factor = self.parse_factor()?;
                Ok(Expression::UnOp(op.into_result()?, Box::new(factor)))
            }
            err => Err(format!("{:?} is an unknown token", err).into()),
        }
    }

    fn parse_generic_expression<F>(
        &mut self,
        matching: &[Token],
        next: F,
    ) -> Result<Expression, Box<dyn Error>>
    where
        F: Fn(&mut Parser) -> Result<Expression, Box<dyn Error>>,
    {
        let mut exp = next(self)?;

        loop {
            match self.peek() {
                Some(ref token) if matching.contains(token) => {
                    let op = self.next().unwrap().into_result()?;
                    let next_exp = next(self)?;
                    exp = Expression::BinOp(op, Box::new(exp), Box::new(next_exp))
                }
                _ => break,
            }
        }
        Ok(exp)
    }
}
