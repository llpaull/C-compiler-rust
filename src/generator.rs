use crate::parser;
use std::collections::HashMap;

pub fn generate(ast: &parser::Program) -> Result<String, String> {
    let mut res = String::new();
    ast.funcs
        .iter()
        .try_for_each(|func| generate_func(func, &mut res))?;

    Ok(res)
}

fn generate_func(func: &parser::FunDecl, res: &mut String) -> Result<(), String> {
    res.push_str(&format!(".globl {}\n{}:\n", func.name, func.name));

    res.push_str("push %rbp\n");
    res.push_str("mov %rsp, %rbp\n");
    let mut stack = StackFrame::new();

    func.body
        .iter()
        .try_for_each(|item| generate_block_item(item, &mut stack, res))?;

    let returns = match func.body.last() {
        None => false,
        Some(item) => check_returns(&item),
    };

    if !returns { res.push_str("mov $0, %rax\n"); }

    res.push_str("mov %rbp, %rsp\n");
    res.push_str("pop %rbp\n");
    res.push_str("ret\n");

    Ok(())
}

fn generate_block_item(item: &parser::BlockItem, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match item {
        parser::BlockItem::Statement(statement) => generate_statement(statement, stack, res)?,
        parser::BlockItem::Declaration(declaration) => generate_declaration(declaration, stack, res)?,
    }

    Ok(())
}

fn generate_statement(statement: &parser::Statement, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match statement {
        parser::Statement::Return(exp) | parser::Statement::Expression(exp) => {
            generate_exp(exp, stack, res)?;
        },
        parser::Statement::If(cond, t, f) => {
            generate_exp(cond, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            let falseid = unique_id();
            res.push_str(&format!("je {}\n", falseid));
            generate_statement(t, stack, res)?;
            match f {
                None => res.push_str(&format!("{}:\n", falseid)),
                Some(statement) => {
                    let postid = unique_id();
                    res.push_str(&format!("jmp {}\n", postid));
                    res.push_str(&format!("{}:\n", falseid));
                    generate_statement(statement, stack, res)?;
                    res.push_str(&format!("{}:\n", postid));
                },
            }
        },
    };
    Ok(())
}

fn generate_declaration(declaration: &parser::Declaration, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match declaration {
        parser::Declaration::Declare(name, opt) => {
            stack.new_var(name)?;
            match opt {
                Some(exp) => {
                    generate_exp(exp, stack, res)?;
                    res.push_str("push %rax\n");
                },
                None => res.push_str("push $0\n"),
            }
        },
    }

    Ok(())
}

fn generate_exp(exp: &parser::Exp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::Exp::Assignment(assignment) => generate_assignment(assignment, stack, res)?,
        parser::Exp::Operator(_op, l, r) => {
            generate_exp(l, stack, res)?;
            generate_exp(r, stack, res)?;
        },
    }
    Ok(())
}

fn generate_assignment(exp: &parser::AssignmentExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::AssignmentExp::Conditional(conditional) => generate_conditional(conditional, stack, res)?,
        parser::AssignmentExp::Operator(op, name, val) => {
            generate_assignment(val, stack, res)?;
            let offset = stack.get_var(name)?;
            match op {
                parser::AssignmentOp::Assign => res.push_str(&format!("mov %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::Plus => res.push_str(&format!("add %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::PostInc => res.push_str(&format!("inc {}(%rbp)\n", offset)),
                parser::AssignmentOp::Sub => res.push_str(&format!("sub %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::PostDec => res.push_str(&format!("dec {}(%rbp)\n", offset)),
                parser::AssignmentOp::Mult => {
                    res.push_str(&format!("imul {}(%rbp)\n", offset));
                    res.push_str(&format!("mov %rax, {}(%rbp)\n", offset));
                }
                parser::AssignmentOp::Div => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str(&format!("mov {}(%rbp), %rax\n", offset));
                    res.push_str("cqo\n");
                    res.push_str("idiv %rcx\n");
                    res.push_str(&format!("mov %rax, {}(%rbp)\n", offset));
                },
                parser::AssignmentOp::Mod => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str(&format!("mov {}(%rbp), %rax\n", offset));
                    res.push_str("cqo\n");
                    res.push_str("idiv %rcx\n");
                    res.push_str(&format!("mov %rdx, {}(%rbp)\n", offset));
               },
                parser::AssignmentOp::BitOr => res.push_str(&format!("or %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::BitAnd => res.push_str(&format!("and %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::BitXor => res.push_str(&format!("xor %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::LShift => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str(&format!("sal %rcx, {}(%rbp)\n", offset));
                },
                parser::AssignmentOp::RShift => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str(&format!("sar %rcx, {}(%rbp)\n", offset));
                },
            }
        }
    }
    Ok(())
}

fn generate_conditional(exp: &parser::ConditionalExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::ConditionalExp::LogicOr(logicor) => generate_logic_or(logicor, stack, res)?,
        parser::ConditionalExp::Operator(cond, t, f) => {
            generate_logic_or(cond, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            let falseid = unique_id();
            let postid = unique_id();
            res.push_str(&format!("je {}\n", falseid));
            generate_exp(t, stack, res)?;
            res.push_str(&format!("jmp {}\n", postid));
            res.push_str(&format!("{}:\n", falseid));
            generate_conditional(f, stack, res)?;
            res.push_str(&format!("{}:\n", postid));
        },
    }
    Ok(())
}

fn generate_logic_or(exp: &parser::LogicOrExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::LogicOrExp::LogicAnd(logic_and) => generate_logic_and(logic_and, stack, res)?,
        parser::LogicOrExp::Operator(_op, l, r) => {
            generate_logic_or(l, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("je {}\n", id));
            res.push_str("mov $1, %rax\n");
            let end = unique_id();
            res.push_str(&format!("jmp {}\n", end));
            res.push_str(&format!("{}:\n", id));
            generate_logic_or(r, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            res.push_str("mov $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("{}:\n", end));
        }
    }
    Ok(())
}

fn generate_logic_and(exp: &parser::LogicAndExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::LogicAndExp::BitOr(bit_or) => generate_bit_or(bit_or, stack, res)?,
        parser::LogicAndExp::Operator(_op, l, r) => {
            generate_logic_and(l, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("jne {}\n", id));
            let end = unique_id();
            res.push_str(&format!("jmp {}\n", end));
            res.push_str(&format!("{}:\n", id));
            generate_logic_and(r, stack, res)?;
            res.push_str("cmp $0, %rax\n");
            res.push_str("mov $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("{}:\n", end));
        }
    }
    Ok(())
}

fn generate_bit_or(exp: &parser::BitOrExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::BitOrExp::BitXor(bit_xor) => generate_bit_xor(bit_xor, stack, res)?,
        parser::BitOrExp::Operator(_op, l, r) => {
            generate_bit_or(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_bit_or(r, stack, res)?;
            res.push_str("pop %rcx\n");
            res.push_str("or %rcx, %rax\n");
        }
    }
    Ok(())
}

fn generate_bit_xor(exp: &parser::BitXorExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::BitXorExp::BitAnd(bit_and) => generate_bit_and(bit_and, stack, res)?,
        parser::BitXorExp::Operator(_op, l, r) => {
            generate_bit_xor(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_bit_xor(r, stack, res)?;
            res.push_str("pop %rcx\n");
            res.push_str("xor %rcx, %rax\n");
        }
    }
    Ok(())
}

fn generate_bit_and(exp: &parser::BitAndExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::BitAndExp::Equality(equality) => generate_equality(equality, stack, res)?,
        parser::BitAndExp::Operator(_op, l, r) => {
            generate_bit_and(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_bit_and(r, stack, res)?;
            res.push_str("pop %rcx\n");
            res.push_str("and %rcx, %rax\n");
        }
    }
    Ok(())
}

fn generate_equality(exp: &parser::EqualityExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::EqualityExp::Rel(rel) => generate_rel(rel, stack, res)?,
        parser::EqualityExp::Operator(op, l, r) => {
            generate_equality(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_equality(r, stack, res)?;
            res.push_str("pop %rcx\n");
            res.push_str("cmp %rax, %rcx\n");
            res.push_str("mov $0, %rax\n");
            match op {
                parser::EqualityOp::Equals => res.push_str("sete %al\n"),
                parser::EqualityOp::NotEquals => res.push_str("setne %al\n"),
            }
        }
    }
    Ok(())
}

fn generate_rel(exp: &parser::RelExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::RelExp::Shift(shift) => generate_shift(shift, stack, res)?,
        parser::RelExp::Operator(op, l, r) => {
            generate_rel(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_rel(r, stack, res)?;
            res.push_str("pop %rcx\n");
            res.push_str("cmp %rax, %rcx\n");
            res.push_str("mov $0, %rax\n");
            match op {
                parser::RelationOp::LessThan => res.push_str("setl %al\n"),
                parser::RelationOp::LessEqual => res.push_str("setle %al\n"),
                parser::RelationOp::GreaterThan => res.push_str("setg %al\n"),
                parser::RelationOp::GreaterEqual => res.push_str("setge %al\n"),
            }
        }
    }
    Ok(())
}

fn generate_shift(exp: &parser::ShiftExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::ShiftExp::Additive(additive) => generate_additive(additive, stack, res)?,
        parser::ShiftExp::Operator(op, l, r) => {
            generate_shift(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_shift(r, stack, res)?;
            res.push_str("mov %rax, %rcx\n");
            res.push_str("pop %rax\n");
            match op {
                parser::ShiftOp::LShift => res.push_str("sal %rcx, %rax\n"),
                parser::ShiftOp::RShift => res.push_str("sar %rcx, %rax\n"),
            }
        }
    }
    Ok(())
}

fn generate_additive(exp: &parser::AdditiveExp, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match exp {
        parser::AdditiveExp::Term(term) => generate_term(term, stack, res)?,
        parser::AdditiveExp::Operator(op, l, r) => {
            generate_additive(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_additive(r, stack, res)?;
            match op {
                parser::AdditiveOp::Add => {
                    res.push_str("pop %rcx\n");
                    res.push_str("add %rcx, %rax\n");
                }
                parser::AdditiveOp::Sub => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str("pop %rax\n");
                    res.push_str("sub %rcx, %rax\n");
                }
            }
        }
    }
    Ok(())
}

fn generate_term(term: &parser::Term, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match term {
        parser::Term::Factor(factor) => generate_factor(factor, stack, res)?,
        parser::Term::Operator(op, l, r) => {
            generate_term(l, stack, res)?;
            res.push_str("push %rax\n");
            generate_term(r, stack, res)?;
            match op {
                parser::TermOp::Mult => {
                    res.push_str("pop %rcx\n");
                    res.push_str("imul %rcx, %rax\n");
                }
                parser::TermOp::Div => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str("pop %rax\n");
                    res.push_str("cqo\n");
                    res.push_str("idiv %rcx\n");
                }
                parser::TermOp::Mod => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str("pop %rax\n");
                    res.push_str("cqo\n");
                    res.push_str("idiv %rcx\n");
                    res.push_str("mov %rdx, %rax\n");
                }
            }
        }
    }
    Ok(())
}

fn generate_factor(factor: &parser::Factor, stack: &mut StackFrame, res: &mut String) -> Result<(), String> {
    match factor {
        parser::Factor::Integer(i) => res.push_str(&format!("mov ${}, %rax\n", i)),
        parser::Factor::Paren(exp) => generate_exp(exp, stack, res)?,
        parser::Factor::Operator(op, exp) => {
            match op {
                parser::FactorOp::Negate => {
                    generate_factor(exp, stack, res)?;
                    res.push_str("neg %rax\n");
                },
                parser::FactorOp::BitNot => {
                    generate_factor(exp, stack, res)?;
                    res.push_str("not %rax\n")
                },
                parser::FactorOp::LogicalNot => {
                    generate_factor(exp, stack, res)?;
                    res.push_str("cmp $0, %rax\n");
                    res.push_str("mov $0, %rax\n");
                    res.push_str("sete %al\n");
                },
                parser::FactorOp::PreInc => {
                    let offset = match **exp {
                        parser::Factor::Variable(ref name) => stack.get_var(name)?,
                        _ => panic!("Invalid increment"),
                    };
                    res.push_str(&format!("inc {}(%rbp)\n", offset));
                    generate_factor(exp, stack, res)?;
                }
                parser::FactorOp::PreDec => {
                    let offset = match **exp {
                        parser::Factor::Variable(ref name) => stack.get_var(name)?,
                        _ => panic!("Invalid decrement"),
                    };
                    res.push_str(&format!("dec {}(%rbp)\n", offset));
                    generate_factor(exp, stack, res)?;
                },
            }
        }
        parser::Factor::Variable(name) => {
            let offset = stack.get_var(name)?;
            res.push_str(&format!("mov {}(%rbp), %rax\n", offset));
        },
    }
    Ok(())
}

static mut COUNTER: i32 = 0;

fn unique_id() -> String {
    unsafe {
        let ret = COUNTER;
        COUNTER += 1;
        format!("_{}", ret)
    }
}

fn check_returns(item: &parser::BlockItem) -> bool {
    match item {
        parser::BlockItem::Declaration(_) => false,
        parser::BlockItem::Statement(statement) => check_return_statement(&statement),
    }
}

fn check_return_statement(statement: &parser::Statement) -> bool {
    match statement {
        parser::Statement::Expression(_) => false,
        parser::Statement::Return(_) => true,
        parser::Statement::If(_, t, f) => {
            let t = check_return_statement(&t);
            let f = match f {
                Some(s) => check_return_statement(&s),
                None => false,
            };
            t && f
        }
    }
}

struct StackFrame {
    vars: HashMap<String, i32>,
    index: i32,
}
impl StackFrame {
    fn new() -> Self {
        StackFrame {
            vars: HashMap::new(),
            index: -8,
        }
    }

    fn new_var(&mut self, name: &String) -> Result<(), String> {
        match self.vars.get(name) {
            Some(_) => return Err(format!("cannot instantiate {} variable more than once", name)),
            None => {}
        }
        self.vars.insert(name.to_string(), self.index);
        self.index -= 8;
        Ok(())
    }

    fn get_var(&self, name: &String) -> Result<i32, String> {
        match self.vars.get(name) {
            Some(i) => Ok(*i),
            None => Err(format!("{} has not been instantiated yet", name))
        }
    }
}
