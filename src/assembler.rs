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

}
