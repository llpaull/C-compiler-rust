use super::ast::*;
use std::collections::HashMap;
use std::error::Error;

pub struct Validator {}
struct Entry {
    params: usize,
    fun_type: FunType,
}
#[derive(PartialEq)]
enum FunType {
    Definition,
    Declaration,
}

impl Validator {
    pub fn validate(ast: &Program) -> Result<(), Box<dyn Error>> {
        Self::validate_program(ast)
    }

    fn validate_program(ast: &Program) -> Result<(), Box<dyn Error>> {
        let mut map: HashMap<&String, Entry> = HashMap::new();

        ast.funcs
            .iter()
            .try_for_each(|func| -> Result<(), Box<dyn Error>> {
                let clone: Vec<Statement>;

                let fun_type = match func.body {
                    None => {
                        clone = vec![];
                        FunType::Declaration
                    }
                    Some(ref body) => {
                        clone = body.clone().to_vec();
                        FunType::Definition
                    }
                };

                if let Some(prev) = map.get(&func.name) {
                    if fun_type == FunType::Definition && prev.fun_type == FunType::Definition {
                        return Err(format!(
                            "Cannot have more than one definition for function {}",
                            func.name
                        )
                        .into());
                    }

                    if prev.params != func.params.len() {
                        return Err(format!(
                            "Cannot have differing amount of paramaters for function {}",
                            func.name
                        )
                        .into());
                    }

                    Self::validate_function(&clone, &map)?;

                    Ok(())
                } else {
                    map.insert(
                        &func.name,
                        Entry {
                            params: func.params.len(),
                            fun_type,
                        },
                    );

                    Self::validate_function(&clone, &map)?;

                    Ok(())
                }
            })
    }

    fn validate_function(
        statements: &Vec<Statement>,
        funcs: &HashMap<&String, Entry>,
    ) -> Result<(), Box<dyn Error>> {
        statements
            .iter()
            .try_for_each(|statement| Self::validate_statement(statement, funcs))?;
        Ok(())
    }

    fn validate_statement(
        statement: &Statement,
        funcs: &HashMap<&String, Entry>,
    ) -> Result<(), Box<dyn Error>> {
        match statement {
            Statement::Break | Statement::Continue => Ok(()),
            Statement::Expression(exp) | Statement::Return(exp) => {
                Self::validate_expression(exp, funcs)
            }
            Statement::Compound(statements) => statements
                .iter()
                .try_for_each(|statement| Self::validate_statement(statement, funcs)),
            Statement::Declaration(_, body) => match body {
                None => Ok(()),
                Some(exp) => Self::validate_expression(exp, funcs),
            },
            Statement::If(cond, if_body, else_body) => {
                Self::validate_expression(cond, funcs)?;
                Self::validate_statement(if_body, funcs)?;
                match else_body {
                    None => Ok(()),
                    Some(statement) => Self::validate_statement(statement, funcs),
                }
            }
            Statement::For(exp, cond, post, body) => {
                Self::validate_expression(exp, funcs)?;
                Self::validate_expression(cond, funcs)?;
                Self::validate_expression(post, funcs)?;
                Self::validate_statement(body, funcs)
            }
            Statement::ForDecl(decl, cond, post, body) => {
                Self::validate_statement(decl, funcs)?;
                Self::validate_expression(cond, funcs)?;
                Self::validate_expression(post, funcs)?;
                Self::validate_statement(body, funcs)
            }
            Statement::While(exp, body) => {
                Self::validate_expression(exp, funcs)?;
                Self::validate_statement(body, funcs)
            }
            Statement::Do(body, exp) => {
                Self::validate_statement(body, funcs)?;
                Self::validate_expression(exp, funcs)
            }
        }
    }

    fn validate_expression(
        exp: &Expression,
        funcs: &HashMap<&String, Entry>,
    ) -> Result<(), Box<dyn Error>> {
        match exp {
            Expression::Num(_) | Expression::Var(_) | Expression::Null => return Ok(()),
            Expression::Assign(_, exp) => Self::validate_expression(exp, funcs),
            Expression::UnOp(_, exp) => Self::validate_expression(exp, funcs),
            Expression::BinOp(_, left, right) => {
                Self::validate_expression(left, funcs)?;
                Self::validate_expression(right, funcs)
            }
            Expression::Ternary(cond, if_body, else_body) => {
                Self::validate_expression(cond, funcs)?;
                Self::validate_expression(if_body, funcs)?;
                Self::validate_expression(else_body, funcs)
            }
            Expression::FunctionCall(name, args) => {
                if let Some(entry) = funcs.get(&name) {
                    let num_args = Self::get_args(args);
                    if num_args != entry.params {
                        return Err(format!("Unequal amount of params and args for function call to {}, expected {} but got {}", name, entry.params, num_args).into());
                    }
                    Ok(())
                } else {
                    return Err(
                        format!("Tried to call function {} before it was declared", name).into(),
                    );
                }
            }
        }
    }

    fn get_args(exp: &Expression) -> usize {
        match exp {
            Expression::Null => 0,
            Expression::Num(_) => 1,
            Expression::Var(_) => 1,
            Expression::UnOp(_, _) => 1,
            Expression::Ternary(_, _, _) => 1,
            Expression::Assign(_, _) => 1,
            Expression::FunctionCall(_, _) => 1,
            Expression::BinOp(BinOp::Comma, left, right) => {
                Self::get_args(left) + Self::get_args(right)
            }
            Expression::BinOp(_, _, _) => 1,
        }
    }
}
