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
    pub tmpc: u32,
    pub lblc: u32,
}

impl Tacker {
    pub fn convert_expression<'a>(
        &mut self,
        e: parser::Expression<'a>,
        instructions: &mut Vec<Instruction<'a>>,
    ) -> Val<'a> {
        match e {
            parser::Expression::Constant(i) => Val::Constant(i),
            parser::Expression::Var(n, id) => Val::Local(n, id),
            parser::Expression::Unary(op, inner) => {
                let src = self.convert_expression(*inner, instructions);
                let dst = Val::Temp(self.tmpc);
                self.tmpc += 1;
                instructions.push(Instruction::Unary {
                    op: match op {
                        parser::UnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
                        parser::UnaryOp::Negative => UnaryOp::Negative,
                        parser::UnaryOp::Negation => UnaryOp::Negation,
                    },
                    src,
                    dst,
                });
                dst
            }
            parser::Expression::Binary(op, e1, e2) => {
                let src1 = self.convert_expression(*e1, instructions);
                let short_circuit = self.lblc;
                if op == parser::BinaryOp::Conjunction {
                    instructions.push(Instruction::JumpIfZero {
                        cond: src1,
                        target: short_circuit,
                    });
                    self.lblc += 1;
                } else if op == parser::BinaryOp::Disjunction {
                    instructions.push(Instruction::JumpIfNotZero {
                        cond: src1,
                        target: short_circuit,
                    });
                    self.lblc += 1;
                }
                let src2 = self.convert_expression(*e2, instructions);
                let dst = Val::Temp(self.tmpc);
                self.tmpc += 1;
                if op == parser::BinaryOp::Conjunction {
                    instructions.push(Instruction::JumpIfZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(1),
                        dst,
                    });
                    instructions.push(Instruction::Jump { target: self.lblc });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(0),
                        dst,
                    });
                    instructions.push(Instruction::Label(self.lblc));
                    self.lblc += 1;
                } else if op == parser::BinaryOp::Disjunction {
                    instructions.push(Instruction::JumpIfNotZero {
                        cond: src2,
                        target: short_circuit,
                    });
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(0),
                        dst,
                    });
                    instructions.push(Instruction::Jump { target: self.lblc });
                    instructions.push(Instruction::Label(short_circuit));
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(1),
                        dst,
                    });
                    instructions.push(Instruction::Label(self.lblc));
                    self.lblc += 1;
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
                dst
            }
            parser::Expression::Assignment(left, right) => {
                let dst = match *left {
                    parser::Expression::Var(name, id) => Val::Local(name, id),
                    _ => unreachable!(),
                };
                let src = self.convert_expression(*right, instructions);
                instructions.push(Instruction::Copy { src, dst });
                dst
            }
        }
    }

    pub fn convert_block_item<'a>(
        &mut self,
        b: parser::BlockItem<'a>,
        instructions: &mut Vec<Instruction<'a>>,
    ) {
        match b {
            parser::BlockItem::Decl(parser::Declaration { name, id, init }) => {
                if let Some(init) = init {
                    let src = self.convert_expression(init, instructions);
                    instructions.push(Instruction::Copy {
                        src,
                        dst: Val::Local(name, id),
                    });
                }
            }
            parser::BlockItem::Stmt(s) => match s {
                parser::Statement::Return(e) => {
                    let r = self.convert_expression(e, instructions);
                    instructions.push(Instruction::Return(r));
                }
                parser::Statement::Expression(e) => {
                    self.convert_expression(e, instructions);
                }
                parser::Statement::Null => (),
            },
        }
    }

    pub fn convert_function<'a>(&mut self, f: parser::Function<'a>) -> Function<'a> {
        let mut instructions: Vec<Instruction> = Vec::new();
        let parser::Function { name, body } = f;
        for b in body {
            self.convert_block_item(b, &mut instructions);
        }
        if name == "main" {
            match instructions.last() {
                Some(Instruction::Return(_)) => (),
                _ => instructions.push(Instruction::Return(Val::Constant(0))),
            }
        }
        return Function { name, instructions };
    }

    pub fn convert<'a>(&mut self, parser::Program(f): parser::Program<'a>) -> Program<'a> {
        return Program(self.convert_function(f));
    }
}
