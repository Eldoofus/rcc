use crate::compiler::parser::{self, Block};

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    BitwiseNot,
    Negative,
    Negation,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitshiftLeft,
    BitshiftRight,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum Val<'a> {
    Constant(i64),
    Temp(u32),
    Local(&'a str, u32),
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction<'a> {
    Return(Val<'a>),
    Unary {
        op: UnaryOp,
        src: Val<'a>,
        dst: Val<'a>,
    },
    Binary {
        op: BinaryOp,
        src1: Val<'a>,
        src2: Val<'a>,
        dst: Val<'a>,
    },
    Copy {
        src: Val<'a>,
        dst: Val<'a>,
    },
    Jump {
        target: u32,
    },
    JumpIfZero {
        cond: Val<'a>,
        target: u32,
    },
    JumpIfNotZero {
        cond: Val<'a>,
        target: u32,
    },
    Label(u32),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>);

pub struct Tacker {
    tmpc: u32,
    lblc: u32,
}

pub fn convert_binop(op: parser::BinaryOp) -> BinaryOp {
    return match op {
        parser::BinaryOp::Add => BinaryOp::Add,
        parser::BinaryOp::Subtract => BinaryOp::Subtract,
        parser::BinaryOp::Multiply => BinaryOp::Multiply,
        parser::BinaryOp::Divide => BinaryOp::Divide,
        parser::BinaryOp::Modulo => BinaryOp::Modulo,
        parser::BinaryOp::BitwiseAnd => BinaryOp::BitwiseAnd,
        parser::BinaryOp::BitwiseOr => BinaryOp::BitwiseOr,
        parser::BinaryOp::BitwiseXor => BinaryOp::BitwiseXor,
        parser::BinaryOp::BitshiftLeft => BinaryOp::BitshiftLeft,
        parser::BinaryOp::BitshiftRight => BinaryOp::BitshiftRight,
        parser::BinaryOp::Equal => BinaryOp::Equal,
        parser::BinaryOp::NotEqual => BinaryOp::NotEqual,
        parser::BinaryOp::Less => BinaryOp::Less,
        parser::BinaryOp::LessEqual => BinaryOp::LessEqual,
        parser::BinaryOp::Greater => BinaryOp::Greater,
        parser::BinaryOp::GreaterEqual => BinaryOp::GreaterEqual,
        _ => unreachable!(),
    };
}

impl Tacker {
    pub fn new() -> Tacker {
        return Tacker { tmpc: 1, lblc: 1 };
    }

    pub fn convert_expression<'a>(&mut self, e: parser::Expression<'a>, instructions: &mut Vec<Instruction<'a>>) -> Val<'a> {
        match e {
            parser::Expression::Constant(i) => Val::Constant(i),
            parser::Expression::Var(n, id) => Val::Local(n, id),
            parser::Expression::Unary(op, inner) => {
                let src = self.convert_expression(*inner, instructions);
                let dst = if let parser::UnaryOp::PreIncrement | parser::UnaryOp::PreDecrement = op {
                    src
                } else {
                    self.tmpc += 1;
                    Val::Temp(self.tmpc - 1)
                };
                if let parser::UnaryOp::PostIncrement | parser::UnaryOp::PostDecrement = op {
                    instructions.push(Instruction::Copy { src, dst });
                }
                instructions.push(match op {
                    parser::UnaryOp::BitwiseNot | parser::UnaryOp::Negative | parser::UnaryOp::Negation => Instruction::Unary {
                        op: match op {
                            parser::UnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
                            parser::UnaryOp::Negative => UnaryOp::Negative,
                            parser::UnaryOp::Negation => UnaryOp::Negation,
                            _ => unreachable!(),
                        },
                        src,
                        dst,
                    },
                    _ => Instruction::Binary {
                        op: match op {
                            parser::UnaryOp::PreIncrement | parser::UnaryOp::PostIncrement => BinaryOp::Add,
                            parser::UnaryOp::PreDecrement | parser::UnaryOp::PostDecrement => BinaryOp::Subtract,
                            _ => unreachable!(),
                        },
                        src1: src,
                        src2: Val::Constant(1),
                        dst: src,
                    },
                });
                dst
            }
            parser::Expression::Binary(op, e1, e2) => {
                let src1 = self.convert_expression(*e1, instructions);
                let short_circuit = self.lblc;
                if let parser::BinaryOp::Conjunction = op {
                    instructions.push(Instruction::JumpIfZero {
                        cond: src1,
                        target: short_circuit,
                    });
                    self.lblc += 1;
                } else if let parser::BinaryOp::Disjunction = op {
                    instructions.push(Instruction::JumpIfNotZero {
                        cond: src1,
                        target: short_circuit,
                    });
                    self.lblc += 1;
                }
                let src2 = self.convert_expression(*e2, instructions);
                let dst = Val::Temp(self.tmpc);
                self.tmpc += 1;
                if let parser::BinaryOp::Conjunction = op {
                    instructions.push(Instruction::JumpIfZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy { src: Val::Constant(1), dst });
                    instructions.push(Instruction::Jump { target: self.lblc });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy { src: Val::Constant(0), dst });
                    instructions.push(Instruction::Label(self.lblc));
                    self.lblc += 1;
                } else if let parser::BinaryOp::Disjunction = op {
                    instructions.push(Instruction::JumpIfNotZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy { src: Val::Constant(0), dst });
                    instructions.push(Instruction::Jump { target: self.lblc });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy { src: Val::Constant(1), dst });
                    instructions.push(Instruction::Label(self.lblc));
                    self.lblc += 1;
                } else {
                    instructions.push(Instruction::Binary {
                        op: convert_binop(op),
                        src1,
                        src2,
                        dst,
                    });
                }
                dst
            }
            parser::Expression::Assignment(left, right, op) => {
                let dst = match *left {
                    parser::Expression::Var(name, id) => Val::Local(name, id),
                    _ => unreachable!(),
                };
                let src = self.convert_expression(*right, instructions);
                match op {
                    parser::BinaryOp::Assign => instructions.push(Instruction::Copy { src, dst }),
                    _ => instructions.push(Instruction::Binary {
                        op: convert_binop(op),
                        src1: dst,
                        src2: src,
                        dst,
                    }),
                }
                dst
            }
            parser::Expression::Conditional(cond, then_e, else_e) => {
                let cond = self.convert_expression(*cond, instructions);
                let result = Val::Temp(self.tmpc);
                self.tmpc += 1;
                let e_label = self.lblc;
                self.lblc += 2;
                instructions.push(Instruction::JumpIfZero { cond, target: e_label });
                let src = self.convert_expression(*then_e, instructions);
                instructions.push(Instruction::Copy { src, dst: result });
                instructions.push(Instruction::Jump { target: e_label + 1 });
                instructions.push(Instruction::Label(e_label));
                let src = self.convert_expression(*else_e, instructions);
                instructions.push(Instruction::Copy { src, dst: result });
                instructions.push(Instruction::Label(e_label + 1));
                result
            }
        }
    }

    pub fn convert_declaration<'a>(&mut self, d: parser::Declaration<'a>, instructions: &mut Vec<Instruction<'a>>) {
        if let Some(init) = d.init {
            let src = self.convert_expression(init, instructions);
            instructions.push(Instruction::Copy {
                src,
                dst: Val::Local(d.name, d.id),
            });
        }
    }

    pub fn convert_statement<'a>(&mut self, s: parser::Statement<'a>, instructions: &mut Vec<Instruction<'a>>) {
        match s {
            parser::Statement::Return(e) => {
                let r = self.convert_expression(e, instructions);
                instructions.push(Instruction::Return(r));
            }
            parser::Statement::Expression(e) => {
                self.convert_expression(e, instructions);
            }
            parser::Statement::If(cond, then_s, Some(else_s)) => {
                let cond = self.convert_expression(cond, instructions);
                let e_label = self.lblc;
                self.lblc += 2;
                instructions.push(Instruction::JumpIfZero { cond, target: e_label });
                self.convert_statement(*then_s, instructions);
                instructions.push(Instruction::Jump { target: e_label + 1 });
                instructions.push(Instruction::Label(e_label));
                self.convert_statement(*else_s, instructions);
                instructions.push(Instruction::Label(e_label + 1));
            }
            parser::Statement::If(cond, then_s, None) => {
                let cond = self.convert_expression(cond, instructions);
                let e_label = self.lblc;
                self.lblc += 1;
                instructions.push(Instruction::JumpIfZero { cond, target: e_label });
                self.convert_statement(*then_s, instructions);
                instructions.push(Instruction::Label(e_label));
            }
            parser::Statement::Goto(_, id) => instructions.push(Instruction::Jump { target: id }),
            parser::Statement::Label(_, id, s) => {
                instructions.push(Instruction::Label(id));
                self.convert_statement(*s, instructions);
            }
            parser::Statement::Compound(Block(body)) => {
                for b in body {
                    match b {
                        parser::BlockItem::Decl(d) => self.convert_declaration(d, instructions),
                        parser::BlockItem::Stmt(s) => self.convert_statement(s, instructions),
                    }
                }
            }
            parser::Statement::Break(id) => instructions.push(Instruction::Jump { target: id }),
            parser::Statement::Continue(id) => instructions.push(Instruction::Jump { target: id - 1 }),
            parser::Statement::While(cond, stmt, id) => {
                instructions.push(Instruction::Label(id - 1));
                let cond = self.convert_expression(cond, instructions);
                instructions.push(Instruction::JumpIfZero { cond, target: id });
                self.convert_statement(*stmt, instructions);
                instructions.push(Instruction::Jump { target: id - 1 });
                instructions.push(Instruction::Label(id));
            }
            parser::Statement::DoWhile(stmt, cond, id) => {
                instructions.push(Instruction::Label(id - 2));
                self.convert_statement(*stmt, instructions);
                instructions.push(Instruction::Label(id - 1));
                let cond = self.convert_expression(cond, instructions);
                instructions.push(Instruction::JumpIfNotZero { cond, target: id - 2 });
                instructions.push(Instruction::Label(id));
            }
            parser::Statement::For(init, cond, post, stmt, id) => {
                if let parser::ForInit::InitDecl(d) = init {
                    self.convert_declaration(d, instructions);
                } else if let parser::ForInit::InitExp(Some(e)) = init {
                    self.convert_expression(e, instructions);
                }
                instructions.push(Instruction::Label(id - 2));
                if let Some(cond) = cond {
                    let cond = self.convert_expression(cond, instructions);
                    instructions.push(Instruction::JumpIfZero { cond, target: id });
                }
                self.convert_statement(*stmt, instructions);
                instructions.push(Instruction::Label(id - 1));
                if let Some(post) = post {
                    self.convert_expression(post, instructions);
                }
                instructions.push(Instruction::Jump { target: id - 2 });
                instructions.push(Instruction::Label(id));
            }
            parser::Statement::Null => (),
        }
    }

    pub fn convert_function<'a>(&mut self, f: parser::Function<'a>) -> Function<'a> {
        let mut instructions: Vec<Instruction> = Vec::new();
        let parser::Function { name, body } = f;
        for b in body.0 {
            match b {
                parser::BlockItem::Decl(d) => self.convert_declaration(d, &mut instructions),
                parser::BlockItem::Stmt(s) => self.convert_statement(s, &mut instructions),
            }
        }
        if name == "main" {
            match instructions.last() {
                Some(Instruction::Return(_)) => (),
                _ => instructions.push(Instruction::Return(Val::Constant(0))),
            }
        }
        return Function { name, instructions };
    }

    pub fn convert<'a>(&mut self, parser::Program(f, varc, lblc): parser::Program<'a>) -> Program<'a> {
        self.tmpc = varc + 1;
        self.lblc = lblc + 1;
        return Program(self.convert_function(f));
    }
}
