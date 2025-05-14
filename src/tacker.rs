use crate::parser;

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
    Local(&'a str),
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
pub enum FnDef<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instruction<'a>>,
    },
}

#[derive(Debug)]
pub enum PgDef<'a> {
    Program(FnDef<'a>),
}

pub struct Tacker {
    pub tmpc: u32,
    pub lblc: u32,
}

impl Tacker {
    pub fn convert_expression<'a>(
        &mut self,
        e: parser::Expression,
        instructions: &mut Vec<Instruction<'a>>,
    ) -> (&mut Self, Val<'a>) {
        match e {
            parser::Expression::Constant(i) => (self, Val::Constant(i)),
            parser::Expression::Unary(op, inner) => {
                let (tacker, src) = self.convert_expression(*inner, instructions);
                let dst = Val::Temp(tacker.tmpc);
                tacker.tmpc += 1;
                instructions.push(Instruction::Unary {
                    op: match op {
                        parser::UnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
                        parser::UnaryOp::Negative => UnaryOp::Negative,
                        parser::UnaryOp::Negation => UnaryOp::Negation,
                    },
                    src,
                    dst,
                });
                (tacker, dst)
            }
            parser::Expression::Binary(op, e1, e2) => {
                let (tacker, src1) = self.convert_expression(*e1, instructions);
                if op == parser::BinaryOp::Conjunction {
                    instructions.push(Instruction::JumpIfZero {
                        cond: src1,
                        target: tacker.lblc,
                    });
                } else if op == parser::BinaryOp::Disjunction {
                    instructions.push(Instruction::JumpIfNotZero {
                        cond: src1,
                        target: tacker.lblc,
                    });
                }
                let (tacker, src2) = tacker.convert_expression(*e2, instructions);
                let dst = Val::Temp(tacker.tmpc);
                tacker.tmpc += 1;
                if op == parser::BinaryOp::Conjunction {
                    let short_circuit = tacker.lblc;
                    let end = tacker.lblc + 1;
                    tacker.lblc += 2;
                    instructions.push(Instruction::JumpIfZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(1),
                        dst,
                    });
                    instructions.push(Instruction::Jump { target: end });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(0),
                        dst,
                    });
                    instructions.push(Instruction::Label(end));
                } else if op == parser::BinaryOp::Disjunction {
                    let short_circuit = tacker.lblc;
                    let end = tacker.lblc + 1;
                    tacker.lblc += 2;
                    instructions.push(Instruction::JumpIfZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(0),
                        dst,
                    });
                    instructions.push(Instruction::Jump { target: end });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(1),
                        dst,
                    });
                    instructions.push(Instruction::Label(end));
                } else {
                    instructions.push(Instruction::Binary {
                        op: match op {
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
                        },
                        src1,
                        src2,
                        dst,
                    });
                }
                (tacker, dst)
            }
        }
    }

    pub fn convert_statement(&mut self, s: parser::Statement, instructions: &mut Vec<Instruction>) {
        let parser::Statement::Return(e) = s;
        let (_, r) = self.convert_expression(e, instructions);
        instructions.push(Instruction::Return(r));
    }

    pub fn convert_function<'a>(&mut self, f: parser::FnDef<'a>) -> FnDef<'a> {
        let mut instructions: Vec<Instruction> = Vec::new();
        let parser::FnDef::Function { name, body } = f;
        self.convert_statement(body, &mut instructions);
        return FnDef::Function { name, instructions };
    }

    pub fn convert<'a>(&mut self, p: parser::PgDef<'a>) -> PgDef<'a> {
        let parser::PgDef::Program(f) = p;
        return PgDef::Program(self.convert_function(f));
    }
}
