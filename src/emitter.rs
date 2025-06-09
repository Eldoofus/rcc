use crate::tacker;
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    R10,
    R11,
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(i64),
    Register(Register),
    Pseudo(u32),
    Stack(i32),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum ShiftOp {
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    Cmp,
}

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOp, Operand),
    Shift(ShiftOp, Operand, Operand),
    Binary(BinaryOp, Operand, Operand),
    Div(Operand),
    Cqo,
    Jmp(u32),
    JmpCC(Condition, u32),
    SetCC(Condition, Operand),
    Label(u32),
    AllocateStack(i32),
    Ret,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>);

pub fn convert_val(v: tacker::Val) -> Operand {
    return match v {
        tacker::Val::Constant(i) => Operand::Imm(i),
        tacker::Val::Temp(id) => Operand::Pseudo(id),
        tacker::Val::Local(_, id) => Operand::Pseudo(id),
    };
}

pub fn convert_instructions(is: Vec<tacker::Instruction>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    for i in is {
        match i {
            tacker::Instruction::Return(v) => {
                instructions.push(Instruction::Mov {
                    src: convert_val(v),
                    dst: Operand::Register(Register::RAX),
                });
                instructions.push(Instruction::Ret);
            }
            tacker::Instruction::Unary {
                op: tacker::UnaryOp::Negation,
                src,
                dst,
            } => {
                instructions.push(Instruction::Binary(
                    BinaryOp::Cmp,
                    Operand::Imm(0),
                    convert_val(src),
                ));
                instructions.push(Instruction::Mov {
                    src: Operand::Imm(0),
                    dst: convert_val(dst),
                });
                instructions.push(Instruction::SetCC(Condition::E, convert_val(dst)));
            }
            tacker::Instruction::Unary { op, src, dst } => {
                instructions.push(Instruction::Mov {
                    src: convert_val(src),
                    dst: convert_val(dst),
                });
                instructions.push(Instruction::Unary(
                    match op {
                        tacker::UnaryOp::Negative => UnaryOp::Neg,
                        tacker::UnaryOp::BitwiseNot => UnaryOp::Not,
                        _ => unreachable!(),
                    },
                    convert_val(dst),
                ));
            }
            tacker::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => match op {
                tacker::BinaryOp::Divide | tacker::BinaryOp::Modulo => {
                    instructions.push(Instruction::Mov {
                        src: convert_val(src1),
                        dst: Operand::Register(Register::RAX),
                    });
                    instructions.push(Instruction::Cqo);
                    instructions.push(Instruction::Div(convert_val(src2)));
                    instructions.push(Instruction::Mov {
                        src: Operand::Register(match op {
                            tacker::BinaryOp::Divide => Register::RAX,
                            tacker::BinaryOp::Modulo => Register::RDX,
                            _ => unreachable!(),
                        }),
                        dst: convert_val(dst),
                    })
                }
                tacker::BinaryOp::BitshiftLeft | tacker::BinaryOp::BitshiftRight => {
                    instructions.push(Instruction::Mov {
                        src: convert_val(src1),
                        dst: convert_val(dst),
                    });
                    instructions.push(Instruction::Shift(
                        match op {
                            tacker::BinaryOp::BitshiftLeft => ShiftOp::Shl,
                            tacker::BinaryOp::BitshiftRight => ShiftOp::Shr,
                            _ => unreachable!(),
                        },
                        convert_val(src2),
                        convert_val(dst),
                    ));
                }
                tacker::BinaryOp::Equal
                | tacker::BinaryOp::NotEqual
                | tacker::BinaryOp::Greater
                | tacker::BinaryOp::GreaterEqual
                | tacker::BinaryOp::Less
                | tacker::BinaryOp::LessEqual => {
                    instructions.push(Instruction::Binary(
                        BinaryOp::Cmp,
                        convert_val(src2),
                        convert_val(src1),
                    ));
                    instructions.push(Instruction::Mov {
                        src: Operand::Imm(0),
                        dst: convert_val(dst),
                    });
                    instructions.push(Instruction::SetCC(
                        match op {
                            tacker::BinaryOp::Equal => Condition::E,
                            tacker::BinaryOp::NotEqual => Condition::NE,
                            tacker::BinaryOp::Greater => Condition::G,
                            tacker::BinaryOp::GreaterEqual => Condition::GE,
                            tacker::BinaryOp::Less => Condition::L,
                            tacker::BinaryOp::LessEqual => Condition::LE,
                            _ => unreachable!(),
                        },
                        convert_val(dst),
                    ));
                }
                _ => {
                    instructions.push(Instruction::Mov {
                        src: convert_val(src1),
                        dst: convert_val(dst),
                    });
                    instructions.push(Instruction::Binary(
                        match op {
                            tacker::BinaryOp::Add => BinaryOp::Add,
                            tacker::BinaryOp::Subtract => BinaryOp::Sub,
                            tacker::BinaryOp::Multiply => BinaryOp::Mul,
                            tacker::BinaryOp::BitwiseAnd => BinaryOp::And,
                            tacker::BinaryOp::BitwiseOr => BinaryOp::Or,
                            tacker::BinaryOp::BitwiseXor => BinaryOp::Xor,
                            _ => unreachable!(),
                        },
                        convert_val(src2),
                        convert_val(dst),
                    ))
                }
            },
            tacker::Instruction::Copy { src, dst } => instructions.push(Instruction::Mov {
                src: convert_val(src),
                dst: convert_val(dst),
            }),
            tacker::Instruction::Jump { target } => instructions.push(Instruction::Jmp(target)),
            tacker::Instruction::JumpIfZero { cond, target } => {
                instructions.push(Instruction::Binary(
                    BinaryOp::Cmp,
                    Operand::Imm(0),
                    convert_val(cond),
                ));
                instructions.push(Instruction::JmpCC(Condition::E, target));
            }
            tacker::Instruction::JumpIfNotZero { cond, target } => {
                instructions.push(Instruction::Binary(
                    BinaryOp::Cmp,
                    Operand::Imm(0),
                    convert_val(cond),
                ));
                instructions.push(Instruction::JmpCC(Condition::NE, target));
            }
            tacker::Instruction::Label(l) => instructions.push(Instruction::Label(l)),
        }
    }

    let is = instructions;
    let mut instructions: Vec<Instruction> = Vec::new();
    //let mut offset = 0;
    let mut last_id: u32 = 0;

    let mut replace = |o| match o {
        Operand::Pseudo(id) => {
            // offset -= if id as i32 <= last_id {
            //     0
            // } else {
            //     last_id = id as i32;
            //     8
            // };
            last_id = std::cmp::max(id, last_id);
            Operand::Stack(id as i32 * -8)
        }
        _ => o,
    };

    for i in is {
        instructions.push(match i {
            Instruction::Mov { src, dst } => Instruction::Mov {
                src: replace(src),
                dst: replace(dst),
            },
            Instruction::Unary(op, dst) => Instruction::Unary(op, replace(dst)),
            Instruction::Shift(op, src, dst) => Instruction::Shift(op, replace(src), replace(dst)),
            Instruction::Binary(op, src, dst) => {
                Instruction::Binary(op, replace(src), replace(dst))
            }
            Instruction::Div(div) => Instruction::Div(replace(div)),
            Instruction::SetCC(cond, dst) => Instruction::SetCC(cond, replace(dst)),
            _ => i,
        });
    }

    let is = instructions;
    let mut instructions: Vec<Instruction> = vec![Instruction::AllocateStack((last_id * 8) as i32)];

    for i in is {
        if let Instruction::Mov { src, dst } = i
            && let Operand::Stack(_) = src
            && let Operand::Stack(_) = dst
        {
            instructions.push(Instruction::Mov {
                src,
                dst: Operand::Register(Register::R10),
            });
            instructions.push(Instruction::Mov {
                src: Operand::Register(Register::R10),
                dst,
            });
        } else if let Instruction::Shift(op, Operand::Imm(i), dst) = i {
            instructions.push(Instruction::Shift(op, Operand::Imm(i % 64), dst));
        } else if let Instruction::Shift(op, src, dst) = i {
            instructions.push(Instruction::Mov {
                src,
                dst: Operand::Register(Register::RCX),
            });
            instructions.push(Instruction::Shift(
                op,
                Operand::Register(Register::RCX),
                dst,
            ));
        } else if let Instruction::Binary(BinaryOp::Cmp, src, dst) = i
            && let Operand::Imm(_) = dst
        {
            instructions.push(Instruction::Mov {
                src: dst,
                dst: Operand::Register(Register::R11),
            });
            instructions.push(Instruction::Binary(
                BinaryOp::Cmp,
                src,
                Operand::Register(Register::R11),
            ));
        } else if let Instruction::Binary(BinaryOp::Mul, src, dst) = i
            && let Operand::Stack(_) = dst
        {
            instructions.push(Instruction::Mov {
                src: dst,
                dst: Operand::Register(Register::R11),
            });
            instructions.push(Instruction::Binary(
                BinaryOp::Mul,
                src,
                Operand::Register(Register::R11),
            ));
            instructions.push(Instruction::Mov {
                src: Operand::Register(Register::R11),
                dst: dst,
            });
        } else if let Instruction::Binary(op, src, dst) = i
            && let Operand::Stack(_) = src
            && let Operand::Stack(_) = dst
        {
            instructions.push(Instruction::Mov {
                src,
                dst: Operand::Register(Register::R10),
            });
            instructions.push(Instruction::Binary(
                op,
                Operand::Register(Register::R10),
                dst,
            ));
        } else if let Instruction::Div(div) = i
            && let Operand::Imm(_) = div
        {
            instructions.push(Instruction::Mov {
                src: div,
                dst: Operand::Register(Register::R10),
            });
            instructions.push(Instruction::Div(Operand::Register(Register::R10)));
        } else {
            instructions.push(i);
        }
    }

    return instructions;
}

pub fn convert_function(tacker::Function { name, instructions }: tacker::Function) -> Function {
    return Function {
        name,
        instructions: convert_instructions(instructions),
    };
}

pub fn convert(tacker::Program(f): tacker::Program) -> Program {
    return Program(convert_function(f));
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match f.width() {
                Some(1) => match self {
                    Register::RAX => "%al",
                    Register::RCX => "%cl",
                    Register::RDX => "%dl",
                    Register::R10 => "%r10b",
                    Register::R11 => "%r11b",
                },
                _ => match self {
                    Register::RAX => "%rax",
                    Register::RCX => "%rcx",
                    Register::RDX => "%rdx",
                    Register::R10 => "%r10",
                    Register::R11 => "%r11",
                },
            }
        )
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Condition::E => "e",
                Condition::NE => "ne",
                Condition::G => "g",
                Condition::GE => "ge",
                Condition::L => "l",
                Condition::LE => "le",
            }
        )
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "${}", i),
            Operand::Register(r) => r.fmt(f),
            Operand::Stack(s) => write!(f, "{}(%rbp)", s),
            _ => panic!("Found {:?}", self),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "negq"),
            UnaryOp::Not => write!(f, "notq"),
        }
    }
}

impl fmt::Display for ShiftOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShiftOp::Shl => write!(f, "salq"),
            ShiftOp::Shr => write!(f, "sarq"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "addq"),
            BinaryOp::Sub => write!(f, "subq"),
            BinaryOp::Mul => write!(f, "imulq"),
            BinaryOp::And => write!(f, "andq"),
            BinaryOp::Or => write!(f, "orq"),
            BinaryOp::Xor => write!(f, "xorq"),
            BinaryOp::Cmp => write!(f, "cmpq"),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "movq\t{}, {}", src, dst),
            Instruction::Unary(op, dst) => write!(f, "{}\t{}", op, dst),
            Instruction::Shift(op, src, dst) => write!(f, "{}\t{:1}, {}", op, src, dst),
            Instruction::Binary(op, src, dst) => write!(f, "{}\t{}, {}", op, src, dst),
            Instruction::Div(div) => write!(f, "idivq\t{}", div),
            Instruction::Cqo => write!(f, "cqo"),
            Instruction::AllocateStack(offset) => write!(f, "subq\t${}, %rsp", offset),
            Instruction::Jmp(l) => write!(f, "jmp\t.L{}", l),
            Instruction::JmpCC(cc, l) => write!(f, "j{}\t.L{}", cc, l),
            Instruction::SetCC(cc, l) => write!(f, "set{}\t{:1}", cc, l),
            Instruction::Label(l) => write!(f, "\r.L{}:", l),
            Instruction::Ret => write!(f, "movq\t%rbp, %rsp\n\tpopq\t%rbp\n\tretq"),
        }
    }
}

impl<'a> fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Function { name, instructions } = self;
        writeln!(
            f,
            "\t.globl\t{}\n{0}:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp",
            name
        )?;
        for i in instructions {
            writeln!(f, "\t{}", i)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Program(func) = self;
        writeln!(f, "{}\t.section\t.note.GNU-stack,\"\",@progbits", func)
    }
}
