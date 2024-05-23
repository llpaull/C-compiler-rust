use super::ast::*;
use std::collections::{HashMap, HashSet};
use std::error::Error;

pub struct Generator {
    unique_ids: u32,
    convention: CallingConvention,
    assembly: Vec<String>,
}

pub enum CallingConvention {
    CDECL,
}

// helper functions
impl Generator {
    fn new(convention: CallingConvention) -> Self {
        Generator {
            unique_ids: 0,
            convention,
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
    pub fn generate(
        ast: &Program,
        convention: CallingConvention,
    ) -> Result<String, Box<dyn Error>> {
        let mut gen = Generator::new(convention);
        gen.generate_program(ast)
    }

    fn generate_program(&mut self, ast: &Program) -> Result<String, Box<dyn Error>> {
        ast.funcs
            .iter()
            .try_for_each(|function| self.generate_function(function))?;
        Ok(self.build())
    }

    fn generate_function(&mut self, function: &Function) -> Result<(), Box<dyn Error>> {
        let clone: Vec<Statement>;
        match function.body {
            None => return Ok(()),
            Some(ref body) => clone = body.clone(),
        }

        self.add(&format!(".globl {}", function.name));
        self.add(&format!("{}:", function.name));

        self.add("push %rbp");
        self.add("mov %rsp, %rbp");
        let mut context = Context::new();

        match self.convention {
            CallingConvention::CDECL => self.generate_cdecl(&mut context, &function.params)?,
        }

        clone
            .iter()
            .try_for_each(|statement| self.generate_statement(&mut context, statement))?;

        if !self.check_returns(&clone) {
            self.add("mov $0, %rax");
            self.add("mov %rbp, %rsp");
            self.add("pop %rbp");
            self.add("ret");
        }
        self.add("");

        Ok(())
    }

    fn generate_cdecl(
        &mut self,
        context: &mut Context,
        params: &Vec<String>,
    ) -> Result<(), Box<dyn Error>> {
        let mut offset = 16;
        params
            .iter()
            .try_for_each(|param| -> Result<(), Box<dyn Error>> {
                context.add_arg(param, offset)?;
                offset += 8;
                Ok(())
            })
    }

    fn generate_statement(
        &mut self,
        context: &mut Context,
        statement: &Statement,
    ) -> Result<(), Box<dyn Error>> {
        match statement {
            Statement::Expression(exp) => self.generate_expression(context, exp)?,
            Statement::Return(exp) => {
                self.generate_expression(context, exp)?;
                self.add("mov %rbp, %rsp");
                self.add("pop %rbp");
                self.add("ret");
            }
            Statement::Declaration(name, opt) => {
                context.add_var(name)?;
                match opt {
                    None => self.add("push $0"),
                    Some(exp) => {
                        self.generate_expression(context, exp)?;
                        self.add("push %rax");
                    }
                }
            }
            Statement::If(cond, if_body, opt) => {
                self.generate_if_statement(context, cond, if_body, opt)?
            }
            Statement::Compound(list) => {
                let mut context_clone = context.clone();
                list.iter().try_for_each(|statement| {
                    self.generate_statement(&mut context_clone, statement)
                })?;
                self.add(&format!("add ${}, %rsp", context_clone.scope_size() * 8));
            }
            Statement::For(exp, cond, post, body) => {
                let beginning = self.next_id();
                let post_label = self.next_id();
                let end = self.next_id();
                context.add_loop(&end, &post_label);

                let mut context_clone = context.clone();
                self.generate_expression(&mut context_clone, exp)?;
                self.add(&format!("{}:", beginning));
                self.generate_expression(&mut context_clone, cond)?;
                match cond {
                    Expression::Null => {}
                    _ => {
                        self.add("cmp $0, %rax");
                        self.add(&format!("je {}", end));
                    }
                }
                self.generate_statement(&mut context_clone, body)?;
                self.add(&format!("{}:", post_label));
                self.generate_expression(&mut context_clone, post)?;
                self.add(&format!("jmp {}", beginning));
                self.add(&format!("{}:", end));
                context.remove_loop();
            }
            Statement::ForDecl(decl, cond, post, body) => {
                let beginning = self.next_id();
                let post_label = self.next_id();
                let end = self.next_id();
                context.add_loop(&end, &post_label);

                let mut context_clone = context.clone();
                self.generate_statement(&mut context_clone, decl)?;
                self.add(&format!("{}:", beginning));
                self.generate_expression(&mut context_clone, cond)?;
                match cond {
                    Expression::Null => {}
                    _ => {
                        self.add("cmp $0, %rax");
                        self.add(&format!("je {}", end));
                    }
                }
                self.generate_statement(&mut context_clone, body)?;
                self.add(&format!("{}:", post_label));
                self.generate_expression(&mut context_clone, post)?;
                self.add(&format!("jmp {}", beginning));
                self.add(&format!("{}:", end));
                self.add("add $8, %rsp");
                context.remove_loop();
            }
            Statement::While(exp, body) => {
                let beginning = self.next_id();
                let end = self.next_id();
                context.add_loop(&end, &beginning);

                self.add(&format!("{}:", beginning));
                self.generate_expression(context, exp)?;
                self.add("cmp $0, %rax");
                self.add(&format!("je {}", end));
                self.generate_statement(context, body)?;
                self.add(&format!("jmp {}", beginning));
                self.add(&format!("{}:", end));
                context.remove_loop();
            }
            Statement::Do(body, exp) => {
                let beginning = self.next_id();
                let end = self.next_id();
                context.add_loop(&end, &beginning);

                self.add(&format!("{}:", beginning));
                self.generate_statement(context, body)?;
                self.generate_expression(context, exp)?;
                self.add("cmp $0, %rax");
                self.add(&format!("jne {}", beginning));
                self.add(&format!("{}:", end));
                context.remove_loop();
            }
            Statement::Break => {
                let label = context.get_break();
                match label {
                    Some(label) => self.add(&format!("jmp {}", label)),
                    None => return Err("Break outside of a loop is not possible".into()),
                }
            }
            Statement::Continue => {
                let label = context.get_continue();
                match label {
                    Some(label) => self.add(&format!("jmp {}", label)),
                    None => return Err("Continue outside of a loop is not possible".into()),
                }
            }
        }

        Ok(())
    }

    fn generate_if_statement(
        &mut self,
        context: &mut Context,
        cond: &Expression,
        if_body: &Statement,
        opt: &Option<Box<Statement>>,
    ) -> Result<(), Box<dyn Error>> {
        self.generate_expression(context, cond)?;
        self.add("cmp $0, %rax");
        let falseid = self.next_id();
        self.add(&format!("je {}", falseid));
        self.generate_statement(context, if_body)?;
        match opt {
            None => self.add(&format!("{}:", falseid)),
            Some(else_body) => {
                let postid = self.next_id();
                self.add(&format!("jmp {}", postid));
                self.add(&format!("{}:", falseid));
                self.generate_statement(context, else_body)?;
                self.add(&format!("{}:", postid));
            }
        }
        Ok(())
    }

    fn generate_expression(
        &mut self,
        context: &mut Context,
        expression: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        match expression {
            Expression::Num(i) => self.add(&format!("mov ${}, %rax", i)),
            Expression::Var(name) => {
                self.add(&format!("mov {}(%rbp), %rax", context.get_offset(name)?))
            }
            Expression::UnOp(op, exp) => self.generate_unop(context, op, exp)?,
            Expression::BinOp(op, l, r) => self.generate_binop(context, op, l, r)?,
            Expression::Assign(name, exp) => {
                self.generate_expression(context, exp)?;
                self.add(&format!("mov %rax, {}(%rbp)", context.get_offset(name)?));
            }
            Expression::Ternary(cond, if_body, else_body) => {
                self.generate_ternary(context, cond, if_body, else_body)?
            }
            Expression::FunctionCall(name, args) => {
                let offset = match self.convention {
                    CallingConvention::CDECL => self.generate_args_cdecl(context, args)?,
                };
                self.add(&format!("call {}", name));
                self.add(&format!("add ${}, %rsp", offset * 8));
            }
            Expression::Null => {}
        }
        Ok(())
    }

    fn generate_unop(
        &mut self,
        context: &mut Context,
        op: &UnOp,
        exp: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        self.generate_expression(context, exp)?;
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
        context: &mut Context,
        op: &BinOp,
        l: &Expression,
        r: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        match op {
            BinOp::Addition => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("add %rcx, %rax");
            }
            BinOp::Subtraction => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sub %rcx, %rax");
            }
            BinOp::Multiplication => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("imul %rcx, %rax");
            }
            BinOp::Division => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("cqo");
                self.add("idiv %rcx");
            }
            BinOp::Modulus => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("cqo");
                self.add("idiv %rcx");
                self.add("mov %rdx, %rax");
            }
            BinOp::Equal => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("sete %al");
            }
            BinOp::NotEqual => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setne %al");
            }
            BinOp::LessThan => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setl %al");
            }
            BinOp::LessThanOrEqual => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setle %al");
            }
            BinOp::GreaterThan => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setg %al");
            }
            BinOp::GreaterThanOrEqual => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("cmp %rax, %rcx");
                self.add("mov $0, %rax");
                self.add("setge %al");
            }
            BinOp::LogicOr => {
                let l_false = self.next_id();
                let l_true = self.next_id();
                self.generate_expression(context, l)?;
                self.add("cmp $0, %rax");
                self.add(&format!("je {}", l_false));
                self.add("mov $1, %rax");
                self.add(&format!("jmp {}", l_true));
                self.add(&format!("{}:", l_false));
                self.generate_expression(context, r)?;
                self.add("cmp $0, %rax");
                self.add("mov $0, %rax");
                self.add("setne %al");
                self.add(&format!("{}:", l_true));
            }
            BinOp::LogicAnd => {
                let l_false = self.next_id();
                let l_true = self.next_id();
                self.generate_expression(context, l)?;
                self.add("cmp $0, %rax");
                self.add(&format!("jne {}", l_true));
                self.add("mov $0, %rax");
                self.add(&format!("jmp {}", l_false));
                self.add(&format!("{}:", l_true));
                self.generate_expression(context, r)?;
                self.add("cmp $0, %rax");
                self.add("mov $0, %rax");
                self.add("setne %al");
                self.add(&format!("{}:", l_false));
            }
            BinOp::BitwiseOr => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("or %rcx, %rax");
            }
            BinOp::BitwiseAnd => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("and %rcx, %rax");
            }
            BinOp::BitwiseXor => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("pop %rcx");
                self.add("xor %rcx, %rax");
            }
            BinOp::ShiftLeft => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sal %rcx, %rax");
            }
            BinOp::ShiftRight => {
                self.generate_expression(context, l)?;
                self.add("push %rax");
                self.generate_expression(context, r)?;
                self.add("mov %rax, %rcx");
                self.add("pop %rax");
                self.add("sar %rcx, %rax");
            }
            BinOp::Comma => {
                self.generate_expression(context, l)?;
                self.generate_expression(context, r)?;
            }
        }

        Ok(())
    }

    fn generate_ternary(
        &mut self,
        context: &mut Context,
        cond: &Expression,
        if_body: &Expression,
        else_body: &Expression,
    ) -> Result<(), Box<dyn Error>> {
        let falseid = self.next_id();
        let postid = self.next_id();

        self.generate_expression(context, cond)?;
        self.add("cmp $0, %rax");
        self.add(&format!("je {}", falseid));
        self.generate_expression(context, if_body)?;
        self.add(&format!("jmp {}", postid));
        self.add(&format!("{}:", falseid));
        self.generate_expression(context, else_body)?;
        self.add(&format!("{}:", postid));

        Ok(())
    }

    fn generate_args_cdecl(
        &mut self,
        context: &mut Context,
        arg: &Expression,
    ) -> Result<i32, Box<dyn Error>> {
        match arg {
            Expression::Null => Ok(0),
            Expression::BinOp(BinOp::Comma, left, right) => {
                let right = self.generate_args_cdecl(context, right)?;
                let left = self.generate_args_cdecl(context, left)?;
                Ok(left + right)
            }
            _ => {
                self.generate_expression(context, arg)?;
                self.add("push %rax");
                Ok(1)
            }
        }
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
            Statement::Declaration(_, _)
            | Statement::Expression(_)
            | Statement::Break
            | Statement::Continue => false,
            Statement::If(_, if_body, opt) => {
                let if_returns = self.check_statement_returns(if_body);
                match opt {
                    None => if_returns,
                    Some(body) => if_returns && self.check_statement_returns(body),
                }
            }
            Statement::Compound(list) => list.iter().any(|x| self.check_statement_returns(x)),
            Statement::For(_, _, _, body) => self.check_statement_returns(body),
            Statement::ForDecl(_, _, _, body) => self.check_statement_returns(body),
            Statement::While(_, body) => self.check_statement_returns(body),
            Statement::Do(body, _) => self.check_statement_returns(body),
        }
    }
}

struct Context {
    stack: StackFrame,
    breaks: Vec<String>,
    continues: Vec<String>,
}

impl Clone for Context {
    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }

    fn clone(&self) -> Self {
        Context {
            stack: self.stack.clone(),
            breaks: self.breaks.clone(),
            continues: self.continues.clone(),
        }
    }
}

impl Context {
    fn new() -> Self {
        Context {
            stack: StackFrame::new(),
            breaks: vec![],
            continues: vec![],
        }
    }

    fn add_var(&mut self, name: &str) -> Result<(), Box<dyn Error>> {
        self.stack.add_var(name)
    }

    fn get_offset(&mut self, name: &str) -> Result<i32, Box<dyn Error>> {
        self.stack.get_offset(name)
    }

    fn add_arg(&mut self, name: &str, offset: i32) -> Result<(), Box<dyn Error>> {
        self.stack.add_arg(name, offset)
    }

    fn scope_size(&self) -> usize {
        self.stack.scope_size()
    }

    fn add_loop(&mut self, break_label: &str, continue_label: &str) {
        self.breaks.push(break_label.to_string());
        self.continues.push(continue_label.to_string());
    }

    fn remove_loop(&mut self) {
        self.breaks.pop();
        self.continues.pop();
    }

    fn get_break(&self) -> Option<&String> {
        self.breaks.last()
    }

    fn get_continue(&self) -> Option<&String> {
        self.continues.last()
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
        StackFrame {
            vars: self.vars.clone(),
            scope: HashSet::new(),
            index: self.index,
        }
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

    fn scope_size(&self) -> usize {
        self.scope.len()
    }

    fn add_arg(&mut self, name: &str, offset: i32) -> Result<(), Box<dyn Error>> {
        let name = name.to_string();
        if let Some(_) = self.scope.get(&name) {
            return Err(format!("Cannot instantiate variable {} more than once", name).into());
        }
        self.vars.insert(name.clone(), offset);
        self.scope.insert(name);
        Ok(())
    }
}
