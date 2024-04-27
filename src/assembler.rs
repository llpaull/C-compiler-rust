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
    use parser::Exp;

    match exp {
        Exp::Term(term) => assemble_term(term, res),
    }
}

fn assemble_term(term: &parser::Term, res: &mut String) {
    use parser::Term;
    use parser::BinOp;

    match term {
        Term::Factor(factor) => assemble_factor(factor, res),
        Term::BinOp(op, l, r) => {
            assemble_term(l, res);
            res.push_str("push %rax\n");
            assemble_term(r, res);
            match op {
                BinOp::Add => {
                    res.push_str("pop %rcx\n");
                    res.push_str("add %rcx, %rax\n");
                },
                BinOp::Sub => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str("pop %rax\n");
                    res.push_str("sub %rcx, %rax\n");
                },
                _ => {},
            }
        },
    }
}

fn assemble_factor(factor: &parser::Factor, res: &mut String) {
    use parser::Factor;
    use parser::BinOp;
    use parser::UnOp;

    match factor {
        Factor::Integer(n) => res.push_str(&format!("mov ${}, %rax\n", *n)),
        Factor::Paren(exp) => assemble_exp(exp, res),
        Factor::UnOp(op, factor) => {
            assemble_factor(factor, res);
            match op {
                UnOp::Neg => res.push_str("neg %rax\n"),
                UnOp::BitNot => res.push_str("not %rax\n"),
                UnOp::Not => {
                    res.push_str("cmp $0, %rax\n");
                    res.push_str("mov $0, %rax\n");
                    res.push_str("sete %al\n");
                },
            }
        },
        Factor::BinOp(op, l, r) => {
            assemble_factor(l, res);
            res.push_str("push %rax\n");
            assemble_factor(r, res);
            match op {
                BinOp::Mul => {
                    res.push_str("pop %rcx\n");
                    res.push_str("imul %rcx, %rax\n");
                }
                BinOp::Div => {
                    res.push_str("mov %rax, %rcx\n");
                    res.push_str("pop %rax\n");
                    res.push_str("cqo\n");
                    res.push_str("idiv %rcx\n");
                },
                _ => {},
            }
        },
    }
}
