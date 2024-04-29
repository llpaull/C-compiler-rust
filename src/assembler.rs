use crate::parser;

pub fn assemble(ast: &parser::Program) -> String {
    use parser::Statement;
    
    let mut res = String::new();
    for func in &ast.funcs {
        res.push_str(&format!(".globl {}\n{}:\n", func.name, func.name));
        for instr in &func.body {
            match instr {
                Statement::Return(exp) => {
                    assemble_exp(exp, &mut res);
                    res.push_str("ret\n");
                },
            }
        }
    }
    res
}

fn assemble_exp(exp: &parser::Exp, res: &mut String) {
    match exp {
        parser::Exp::LogicAnd(logic_and) => assemble_logic_and(logic_and, res),
        parser::Exp::Operator(_op, l, r) => {
            assemble_exp(l, res);
            res.push_str("cmpq $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("je _{}\n", id));
            res.push_str("movq $1, %rax\n");
            let end = unique_id();
            res.push_str(&format!("jmp _{}\n", end));
            res.push_str(&format!("_{}:\n", id));
            assemble_exp(r, res);
            res.push_str("cmpq $0, %rax\n");
            res.push_str("movq $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("_{}:\n", end));
        },
    }
}

fn assemble_logic_and(logic_and: &parser::LogicAndExp, res: &mut String) {
    match logic_and {
        parser::LogicAndExp::Equality(equality) => assemble_equality(equality, res),
        parser::LogicAndExp::Operator(_op, l, r) => {
            assemble_logic_and(l, res);
            res.push_str("cmpq $0, %rax\n");
            let id = unique_id();
            res.push_str(&format!("jne _{}\n", id));
            let end = unique_id();
            res.push_str(&format!("jmp _{}\n", end));
            res.push_str(&format!("_{}:\n", id));
            assemble_logic_and(r, res);
            res.push_str("cmpq $0, %rax\n");
            res.push_str("movq $0, %rax\n");
            res.push_str("setne %al\n");
            res.push_str(&format!("_{}:\n", end));
        },
    }
}

fn assemble_equality(equality: &parser::EqualityExp, res: &mut String) {
    match equality {
        parser::EqualityExp::Rel(rel) => assemble_rel(rel, res),
        parser::EqualityExp::Operator(op, l, r) => {
            assemble_equality(l, res);
            res.push_str("pushq %rax\n");
            assemble_equality(r, res);
            res.push_str("popq %rcx\n");
            res.push_str("cmpq %rax, %rcx\n");
            res.push_str("movq $0, %rax\n");
            match op {
                parser::EqualityOp::Equals => res.push_str("sete %al\n"),
                parser::EqualityOp::NotEquals => res.push_str("setne %al\n"),
            }
        },
    }
}

fn assemble_rel(rel: &parser::RelExp, res: &mut String) {
    match rel {
        parser::RelExp::Shift(shift) => assemble_shift(shift, res),
        parser::RelExp::Operator(op, l, r) => {
            assemble_rel(l, res);
            res.push_str("pushq %rax\n");
            assemble_rel(r, res);
            res.push_str("popq %rcx\n");
            res.push_str("cmpq %rax, %rcx\n");
            res.push_str("movq $0, %rax\n");
            match op {
                parser::RelationOp::LessThan => res.push_str("setl %al\n"),
                parser::RelationOp::LessEqual => res.push_str("setle %al\n"),
                parser::RelationOp::GreaterThan => res.push_str("setg %al\n"),
                parser::RelationOp::GreaterEqual => res.push_str("setge %al\n"),
            }
        },
    }
}

fn assemble_shift(shift: &parser::ShiftExp, res: &mut String) {
    match shift {
        parser::ShiftExp::Additive(additive) => assemble_additive(additive, res),
        parser::ShiftExp::Operator(op, l, r) => {
            assemble_shift(l, res);
            res.push_str("pushq %rax\n");
            assemble_shift(r, res);
            res.push_str("movq %rax, %rcx\n");
            res.push_str("popq %rax\n");
            match op {
                parser::ShiftOp::LShift => res.push_str("shl %rcx, %rax\n"),
                parser::ShiftOp::RShift => res.push_str("shr %rcx, %rax\n"),
            }
        },
    }
}

fn assemble_additive(additive: &parser::AdditiveExp, res: &mut String) {
    match additive {
        parser::AdditiveExp::Term(term) => assemble_term(term, res),
        parser::AdditiveExp::Operator(op, l, r) => {
            assemble_additive(l, res);
            res.push_str("pushq %rax\n");
            assemble_additive(r, res);
            match op {
                parser::AdditiveOp::Add => {
                    res.push_str("popq %rcx\n");
                    res.push_str("addq %rcx, %rax\n");
                },
                parser::AdditiveOp::Sub => {
                    res.push_str("movq %rax, %rcx\n");
                    res.push_str("popq %rax\n");
                    res.push_str("subq %rcx, %rax\n");
                },
            }
        },
    }
}

fn assemble_term(term: &parser::Term, res: &mut String) {
    match term {
        parser::Term::Factor(factor) => assemble_factor(factor, res),
        parser::Term::Operator(op, l, r) => {
            assemble_term(l, res);
            res.push_str("pushq %rax\n");
            assemble_term(r, res);
            match op {
                parser::TermOp::Mult => {
                    res.push_str("popq %rcx\n");
                    res.push_str("imulq %rcx, %rax\n");
                },
                parser::TermOp::Div => {
                    res.push_str("movq %rax, %rcx\n");
                    res.push_str("popq %rax\n");
                    res.push_str("cqo\n");
                    res.push_str("idivq %rcx\n");
                },
                parser::TermOp::Mod => {
                    res.push_str("movq %rax, %rcx\n");
                    res.push_str("popq %rax\n");
                    res.push_str("cqo\n");
                    res.push_str("idivq %rcx\n");
                    res.push_str("movq %rdx, %rax\n");
                },
            }
        },
    }
}

fn assemble_factor(factor: &parser::Factor, res: &mut String) {
    match factor {
        parser::Factor::Integer(i) => res.push_str(&format!("movq ${}, %rax\n", i)),
        parser::Factor::Paren(exp) => assemble_exp(exp, res), 
        parser::Factor::Operator(op, exp) => {
            assemble_factor(exp, res);
            match op {
                parser::FactorOp::Neg => res.push_str("negq %rax\n"),
                parser::FactorOp::BitNot => res.push_str("notq %rax\n"),
                parser::FactorOp::Not => {
                    res.push_str("cmpq $0, %rax\n");
                    res.push_str("movq $0, %rax\n");
                    res.push_str("sete %al\n");
                },
            }
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
