use super::ast::*;
use std::collections::{HashMap, HashSet};
use std::error::Error;

pub struct Generator {
    unique_ids: u32,
    assembly: Vec<String>,
}

// helper functions
impl Generator {
    fn new() -> Self {
        Generator {
            unique_ids: 0,
            assembly: vec![],
        }
    }

    fn add(&mut self, s: &str) {
        self.assembly.push(s.to_string());
    }

    fn next_id(&mut self) -> String {
        let ret = format!("_{}", self.unique_ids);
        self.unique_ids += 1;
        ret
    }

    fn build(&mut self) -> String {
        self.assembly.join("\n")
    }
}

// generator functions
impl Generator {
    pub fn generate(ast: &Program) -> Result<String, Box<dyn Error>> {
        let mut gen = Generator::new();
        gen.generate_program(ast)
    }

    fn generate_program(&mut self, ast: &Program) -> Result<String, Box<dyn Error>> {
        ast.funcs
            .iter()
            .try_for_each(|function| self.generate_function(function))?;
        Ok(self.build())
    }

    fn generate_function(&mut self, function: &Function) -> Result<(), Box<dyn Error>> {
        self.add(&format!(".globl {}", function.name));
        self.add(&format!("{}:", function.name));

        self.add("push %rbp");
        self.add("mov %rsp, %rbp");
        let mut stack = StackFrame::new();

        function
            .body
            .iter()
            .try_for_each(|statement| self.generate_statement(&mut stack, statement))?;

        if !self.check_returns(&function.body) {
            self.add("mov $0, %rax");
            self.add("mov %rbp, %rsp");
            self.add("pop %rbp");
            self.add("ret");
        }

        Ok(())
    }

    fn generate_statement(
        &mut self,
        stack: &mut StackFrame,
        statement: &Statement,
    ) -> Result<(), Box<dyn Error>> {
        match statement {
            Statement::Expression(exp) => self.generate_expression(stack, exp)?,
            Statement::Return(exp) => {
                self.generate_expression(stack, exp)?;
                self.add("mov %rbp, %rsp");
                self.add("pop %rbp");
                self.add("ret");
            }
            Statement::Declaration(name, opt) => {
                stack.add_var(name)?;
                match opt {
                    None => self.add("push $0"),
                    Some(exp) => {
                        self.generate_expression(stack, exp)?;
                        self.add("push %rax");
                    }
                }
            }
            Statement::If(cond, if_body, opt) => {
                self.generate_if_statement(stack, cond, if_body, opt)?
            }
            Statement::Compound(list) => {
                let mut stack_clone = stack.clone();
                list.iter()
                    .try_for_each(|x| self.generate_statement(&mut stack_clone, x))?;
                let mut i = 0;
                let size = stack_clone.scope_size();
                while i < size {
                    self.add("pop %rcx");
                    i += 1;
                }
            }
        }

        Ok(())
    }

    fn generate_if_statement(
        &mut self,
        stack: &mut StackFrame,
        cond: &Expression,
        if_body: &Statement,
        opt: &Option<Box<Statement>>,
    ) -> Result<(), Box<dyn Error>> {
        self.generate_expression(stack, cond)?;
        self.add("cmp $0, %rax");
        let falseid = self.next_id();
        self.add(&format!("je {}", falseid));
        self.generate_statement(stack, if_body)?;
        match opt {
            None => self.add(&format!("{}:", falseid)),
            Some(else_body) => {
                let postid = self.next_id();
                self.add(&format!("jmp {}", postid));
                self.add(&format!("{}:", falseid));
                self.generate_statement(stack, else_body)?;
                self.add(&format!("{}:", postid));
            }
        }
        Ok(())
    }

    fn generate_expression(
        &mut self,
        stack: &mut StackFrame,
        expression: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        match expression {
            Expression::Num(i) => self.add(&format!("mov ${}, %rax", i)),
            Expression::Var(name) => {
                self.add(&format!("mov {}(%rbp), %rax", stack.get_offset(name)?))
            }
            Expression::UnOp(op, exp) => self.generate_unop(stack, op, exp)?,
            Expression::BinOp(op, l, r) => self.generate_binop(stack, op, l, r)?,
            Expression::Assign(name, exp) => {
                self.generate_expression(stack, exp)?;
                self.add(&format!("mov %rax, {}(%rbp)", stack.get_offset(name)?));
            }
            Expression::Ternary(cond, if_body, else_body) => {
                self.generate_ternary(stack, cond, if_body, else_body)?
            }
        }
        Ok(())
    }

    fn generate_unop(
        &mut self,
        stack: &mut StackFrame,
        op: &UnOp,
        exp: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        self.generate_expression(stack, exp)?;
        match op {
            UnOp::Negation => self.add("neg %rax"),
            UnOp::BitNot => self.add("not %rax"),
            UnOp::LogicNot => {
                self.add("cmp $0, %rax");
                self.add("mov $0, %rax");
                self.add("sete %al");
            }
        }
        Ok(())
    }

    fn generate_binop(
        &mut self,
        stack: &mut StackFrame,
        op: &BinOp,
        l: &Expression,
        r: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        match op {
            BinOp::Addition => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("add %rcx, %rax");
            }
            BinOp::Subtraction => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sub %rcx, %rax");
            }
            BinOp::Multiplication => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("imul %rcx, %rax");
            }
            BinOp::Division => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("cqo");
                self.add("idiv %rcx");
            }
            BinOp::Modulus => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("cqo");
                self.add("idiv %rcx");
                self.add("mov %rdx, %rax");
            }
            BinOp::Equal => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("sete %al");
            }
            BinOp::NotEqual => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setne %al");
            }
            BinOp::LessThan => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setl %al");
            }
            BinOp::LessThanOrEqual => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setle %al");
            }
            BinOp::GreaterThan => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setg %al");
            }
            BinOp::GreaterThanOrEqual => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setge %al");
            }
            BinOp::LogicOr => {
                let l_false = self.next_id();
                let l_true = self.next_id();
                self.generate_expression(stack, l)?;
                self.add("cmp $0, %rax");
                self.add(&format!("je {}", l_false));
                self.add("mov $1, %rax");
                self.add(&format!("jmp {}", l_true));
                self.add(&format!("{}:", l_false));
                self.generate_expression(stack, r)?;
                self.add("cmp $0, %rax");
                self.add("mov $0, %rax");
                self.add("setne %al");
                self.add(&format!("{}:", l_true));
            }
            BinOp::LogicAnd => {
                let l_false = self.next_id();
                let l_true = self.next_id();
                self.generate_expression(stack, l)?;
                self.add("cmp $0, %rax");
                self.add(&format!("jne {}", l_true));
                self.add("mov $0, %rax");
                self.add(&format!("jmp {}", l_false));
                self.add(&format!("{}:", l_true));
                self.generate_expression(stack, r)?;
                self.add("cmp $0, %rax");
                self.add("mov $0, %rax");
                self.add("setne %al");
                self.add(&format!("{}:", l_false));
            }
            BinOp::BitwiseOr => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("or %rcx, %rax");
            }
            BinOp::BitwiseAnd => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("and %rcx, %rax");
            }
            BinOp::BitwiseXor => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("pop %rcx");
                self.add("xor %rcx, %rax");
            }
            BinOp::ShiftLeft => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sal %rcx, %rax");
            }
            BinOp::ShiftRight => {
                self.generate_expression(stack, l)?;
                self.add("push %rax");
                self.generate_expression(stack, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sar %rcx, %rax");
            }
            BinOp::Comma => {
                self.generate_expression(stack, l)?;
                self.generate_expression(stack, r)?;
            }
        }

        Ok(())
    }

    fn generate_ternary(
        &mut self,
        stack: &mut StackFrame,
        cond: &Expression,
        if_body: &Expression,
        else_body: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        let falseid = self.next_id();
        let postid = self.next_id();

        self.generate_expression(stack, cond)?;
        self.add("cmp $0, %rax");
        self.add(&format!("je {}", falseid));
        self.generate_expression(stack, if_body)?;
        self.add(&format!("jmp {}", postid));
        self.add(&format!("{}:", falseid));
        self.generate_expression(stack, else_body)?;
        self.add(&format!("{}:", postid));

        Ok(())
    }

    fn check_returns(&mut self, statements: &Vec<Statement>) -> bool {
        match statements.last() {
            Some(x) => self.check_statement_returns(x),
            None => false,
        }
    }

    fn check_statement_returns(&mut self, statement: &Statement) -> bool {
        match statement {
            Statement::Return(_) => true,
            Statement::If(_, if_body, opt) => {
                let if_returns = self.check_statement_returns(if_body);
                match opt {
                    None => if_returns,
                    Some(body) => if_returns && self.check_statement_returns(body),
                }
            }
            Statement::Declaration(_, _) | Statement::Expression(_) => false,
            Statement::Compound(list) => list.iter().any(|x| self.check_statement_returns(x)),
        }
    }
}

struct StackFrame {
    vars: HashMap<String, i32>,
    scope: HashSet<String>,
    index: i32,
}

impl Clone for StackFrame {
    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }

    fn clone(&self) -> Self {
        StackFrame { vars: self.vars.clone(), scope: HashSet::new(), index: self.index }
    }
}

impl StackFrame {
    fn new() -> Self {
        StackFrame {
            vars: HashMap::new(),
            scope: HashSet::new(),
            index: -8,
        }
    }

    fn add_var(&mut self, name: &str) -> Result<(), Box<dyn Error>> {
        let name = name.to_string();
        if let Some(_) = self.scope.get(&name) {
            return Err(format!("Cannot instantiate variable {} more than once", name).into());
        }
        self.vars.insert(name.clone(), self.index);
        self.scope.insert(name);
        self.index -= 8;
        Ok(())
    }

    fn get_offset(&mut self, name: &str) -> Result<i32, Box<dyn Error>> {
        match self.vars.get(name) {
            Some(i) => Ok(*i),
            None => Err(format!("Variable with name {} has not been declared yet", name).into()),
        }
    }

    fn scope_size(&mut self) -> usize {
        self.scope.len()
    }
}
