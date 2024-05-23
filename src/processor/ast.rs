#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Declaration(String, Option<Expression>),
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<Statement>),
    For(Expression, Expression, Expression, Box<Statement>),
    ForDecl(Box<Statement>, Expression, Expression, Box<Statement>),
    While(Expression, Box<Statement>),
    Do(Box<Statement>, Expression),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum Expression {
    Num(u32),
    Var(String),
    UnOp(UnOp, Box<Expression>),
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    Assign(String, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Null,
}

#[derive(Debug)]
pub enum UnOp {
    Negation,
    BitNot,
    LogicNot,
}

#[derive(Debug)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    LogicAnd,
    LogicOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    Comma,
}
