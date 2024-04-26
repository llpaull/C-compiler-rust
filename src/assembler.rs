use crate::parser;

pub fn assemble(ast: &parser::Program) -> String {
    let mut res = String::new();
    for func in &ast.funcs {
        res.push_str(&format!(".globl {}\n{}:\n", func.name, func.name));
        for instr in &func.body {
            match instr {
                parser::Statement::Return(exp) => {
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
        parser::Exp::Integer(n) => res.push_str(&format!("movl ${}, %eax\n", n)),
        parser::Exp::UnaryOp(op, exp) => {
            assemble_exp(exp, res);
            match op {
                parser::UnaryOp::Negation => res.push_str("neg %eax\n"),
                parser::UnaryOp::Not => {
                    res.push_str("cmpl $0, %eax\n");
                    res.push_str("movl $0, %eax\n");
                    res.push_str("sete %al\n");
                },
                parser::UnaryOp::BitNot => res.push_str("not %eax\n"),
            }
        },
    }
}
