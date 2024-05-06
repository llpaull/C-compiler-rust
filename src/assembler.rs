use crate::parser;
use std::collections::HashMap;

pub fn assemble(ast: &parser::Program) -> String {
    let mut res = String::new();
    ast.funcs
        .iter()
        .for_each(|func| assemble_func(func, &mut res));

    res
}

fn assemble_func(func: &parser::FunDecl, res: &mut String) {
    res.push_str(&format!(".globl {}\n{}:\n", func.name, func.name));

    res.push_str("push %rbp\n");
    res.push_str("mov %rsp, %rbp\n");
    let mut stack = StackFrame::new();

    func.body
        .iter()
        .for_each(|statement| assemble_statement(statement, &mut stack, res));

    match func.body.last() {
        Some(parser::Statement::Return(_)) => {},
        _ => res.push_str("mov $0, %rax\n"),
    }

    res.push_str("mov %rbp, %rsp\n");
    res.push_str("pop %rbp\n");
    res.push_str("ret\n");
}

fn assemble_statement(statement: &parser::Statement, stack: &mut StackFrame, res: &mut String) {
    match statement {
        parser::Statement::Return(exp) | parser::Statement::Expression(exp) => {
            assemble_exp(exp, stack, res)
        }
        parser::Statement::Declaration(name, opt) => {
            stack.new_var(name);
            match opt {
                Some(exp) => {
                    assemble_exp(exp, stack, res);
                    res.push_str("push %rax\n");
                },
                None => res.push_str("push $0\n"),
            }
        }
    }
}

fn assemble_exp(exp: &parser::Exp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::Exp::LogicOr(logic_or) => assemble_logic_or(logic_or, stack, res),
        parser::Exp::Operator(op, name, val) => {
            assemble_exp(val, stack, res);
            let offset = stack.get_var(name);
            match op {
                parser::AssignmentOp::Assign => res.push_str(&format!("mov %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::Plus => res.push_str(&format!("add %rax, {}(%rbp)\n", offset)),
                parser::AssignmentOp::Sub => res.push_str(&format!("sub %rax, {}(%rbp)\n", offset)),
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
}

fn assemble_logic_or(exp: &parser::LogicOrExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::LogicOrExp::LogicAnd(logic_and) => assemble_logic_and(logic_and, stack, res),
        parser::LogicOrExp::Operator(_op, l, r) => {
            assemble_logic_or(l, stack, res);
            res.push_str("cmp $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("je _{}\n", id));
            res.push_str("mov $1, %rax\n");
            let end = unique_id();
            res.push_str(&format!("jmp _{}\n", end));
            res.push_str(&format!("_{}:\n", id));
            assemble_logic_or(r, stack, res);
            res.push_str("cmp $0, %rax\n");
            res.push_str("mov $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("_{}:\n", end));
        }
    }
}

fn assemble_logic_and(exp: &parser::LogicAndExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::LogicAndExp::BitOr(bit_or) => assemble_bit_or(bit_or, stack, res),
        parser::LogicAndExp::Operator(_op, l, r) => {
            assemble_logic_and(l, stack, res);
            res.push_str("cmp $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("jne _{}\n", id));
            let end = unique_id();
            res.push_str(&format!("jmp _{}\n", end));
            res.push_str(&format!("_{}:\n", id));
            assemble_logic_and(r, stack, res);
            res.push_str("cmp $0, %rax\n");
            res.push_str("mov $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("_{}:\n", end));
        }
    }
}

fn assemble_bit_or(exp: &parser::BitOrExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::BitOrExp::BitXor(bit_xor) => assemble_bit_xor(bit_xor, stack, res),
        parser::BitOrExp::Operator(_op, l, r) => {
            assemble_bit_or(l, stack, res);
            res.push_str("push %rax\n");
            assemble_bit_or(r, stack, res);
            res.push_str("pop %rcx\n");
            res.push_str("or %rcx, %rax\n");
        }
    }
}

fn assemble_bit_xor(exp: &parser::BitXorExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::BitXorExp::BitAnd(bit_and) => assemble_bit_and(bit_and, stack, res),
        parser::BitXorExp::Operator(_op, l, r) => {
            assemble_bit_xor(l, stack, res);
            res.push_str("push %rax\n");
            assemble_bit_xor(r, stack, res);
            res.push_str("pop %rcx\n");
            res.push_str("xor %rcx, %rax\n");
        }
    }
}

fn assemble_bit_and(exp: &parser::BitAndExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::BitAndExp::Equality(equality) => assemble_equality(equality, stack, res),
        parser::BitAndExp::Operator(_op, l, r) => {
            assemble_bit_and(l, stack, res);
            res.push_str("push %rax\n");
            assemble_bit_and(r, stack, res);
            res.push_str("pop %rcx\n");
            res.push_str("and %rcx, %rax\n");
        }
    }
}

fn assemble_equality(exp: &parser::EqualityExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::EqualityExp::Rel(rel) => assemble_rel(rel, stack, res),
        parser::EqualityExp::Operator(op, l, r) => {
            assemble_equality(l, stack, res);
            res.push_str("push %rax\n");
            assemble_equality(r, stack, res);
            res.push_str("pop %rcx\n");
            res.push_str("cmp %rax, %rcx\n");
            res.push_str("mov $0, %rax\n");
            match op {
                parser::EqualityOp::Equals => res.push_str("sete %al\n"),
                parser::EqualityOp::NotEquals => res.push_str("setne %al\n"),
            }
        }
    }
}

fn assemble_rel(exp: &parser::RelExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::RelExp::Shift(shift) => assemble_shift(shift, stack, res),
        parser::RelExp::Operator(op, l, r) => {
            assemble_rel(l, stack, res);
            res.push_str("push %rax\n");
            assemble_rel(r, stack, res);
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
}

fn assemble_shift(exp: &parser::ShiftExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::ShiftExp::Additive(additive) => assemble_additive(additive, stack, res),
        parser::ShiftExp::Operator(op, l, r) => {
            assemble_shift(l, stack, res);
            res.push_str("push %rax\n");
            assemble_shift(r, stack, res);
            res.push_str("mov %rax, %rcx\n");
            res.push_str("pop %rax\n");
            match op {
                parser::ShiftOp::LShift => res.push_str("sal %rcx, %rax\n"),
                parser::ShiftOp::RShift => res.push_str("sar %rcx, %rax\n"),
            }
        }
    }
}

fn assemble_additive(exp: &parser::AdditiveExp, stack: &mut StackFrame, res: &mut String) {
    match exp {
        parser::AdditiveExp::Term(term) => assemble_term(term, stack, res),
        parser::AdditiveExp::Operator(op, l, r) => {
            assemble_additive(l, stack, res);
            res.push_str("push %rax\n");
            assemble_additive(r, stack, res);
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
}

fn assemble_term(term: &parser::Term, stack: &mut StackFrame, res: &mut String) {
    match term {
        parser::Term::Factor(factor) => assemble_factor(factor, stack, res),
        parser::Term::Operator(op, l, r) => {
            assemble_term(l, stack, res);
            res.push_str("push %rax\n");
            assemble_term(r, stack, res);
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
}

fn assemble_factor(factor: &parser::Factor, stack: &mut StackFrame, res: &mut String) {
    match factor {
        parser::Factor::Integer(i) => res.push_str(&format!("mov ${}, %rax\n", i)),
        parser::Factor::Paren(exp) => assemble_exp(exp, stack, res),
        parser::Factor::Operator(op, exp) => {
            assemble_factor(exp, stack, res);
            match op {
                parser::FactorOp::Negate => res.push_str("neg %rax\n"),
                parser::FactorOp::BitNot => res.push_str("not %rax\n"),
                parser::FactorOp::LogicalNot => {
                    res.push_str("cmp $0, %rax\n");
                    res.push_str("mov $0, %rax\n");
                    res.push_str("sete %al\n");
                }
            }
        }
        parser::Factor::Variable(name) => {
            let offset = stack.get_var(name);
            res.push_str(&format!("mov {}(%rbp), %rax\n", offset));
        },
    }
}

static mut COUNTER: i32 = 0;

fn unique_id() -> String {
    unsafe {
        let ret = COUNTER;
        COUNTER += 1;
        ret.to_string()
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

    fn new_var(&mut self, name: &String) {
        match self.vars.get(name) {
            Some(_) => panic!("Variable name used twice"),
            None => {}
        }
        self.vars.insert(name.to_string(), self.index);
        self.index -= 8;
    }

    fn get_var(&self, name: &String) -> &i32 {
        self.vars.get(name).expect("Undefined variable")
    }
}
