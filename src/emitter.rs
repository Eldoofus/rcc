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
}

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOp, Operand),
    Binary(BinaryOp, Operand, Operand),
    Div(Operand),
    Cqo,
    AllocateStack(i32),
    Ret,
}

#[derive(Debug)]
pub enum FnDef<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instruction>,
    },
}

#[derive(Debug)]
pub enum PgDef<'a> {
    Program(FnDef<'a>),
}

pub fn convert_val(v: tacker::Val) -> Operand {
    return match v {
        tacker::Val::Constant(i) => Operand::Imm(i),
        tacker::Val::Temp(id) => Operand::Pseudo(id),
        _ => unreachable!(),
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
            tacker::Instruction::Unary { op, src, dst } => {
                instructions.push(Instruction::Mov {
                    src: convert_val(src),
                    dst: convert_val(dst),
                });
                instructions.push(Instruction::Unary(
                    match op {
                        tacker::UnaryOp::Negate => UnaryOp::Neg,
                        tacker::UnaryOp::Complement => UnaryOp::Not,
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
                    instructions.push(Instruction::Mov {
                        src: convert_val(src2),
                        dst: Operand::Register(Register::RCX),
                    });
                    instructions.push(Instruction::Unary(
                        match op {
                            tacker::BinaryOp::BitshiftLeft => UnaryOp::Shl,
                            tacker::BinaryOp::BitshiftRight => UnaryOp::Shr,
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
            Instruction::Binary(op, src, dst) => {
                Instruction::Binary(op, replace(src), replace(dst))
            }
            Instruction::Div(div) => Instruction::Div(replace(div)),
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

pub fn convert_function(f: tacker::FnDef) -> FnDef {
    let tacker::FnDef::Function { name, instructions } = f;
    return FnDef::Function {
        name,
        instructions: convert_instructions(instructions),
    };
}

pub fn convert(p: tacker::PgDef) -> PgDef {
    let tacker::PgDef::Program(f) = p;
    return PgDef::Program(convert_function(f));
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::RAX => write!(f, "%rax"),
            Register::RCX => write!(f, "%rcx"),
            Register::RDX => write!(f, "%rdx"),
            Register::R10 => write!(f, "%r10"),
            Register::R11 => write!(f, "%r11"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "${}", i),
            Operand::Register(r) => write!(f, "{}", r),
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
            UnaryOp::Shl => write!(f, "salq"),
            UnaryOp::Shr => write!(f, "sarq"),
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
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "movq\t{}, {}", src, dst),
            Instruction::Unary(op, dst) => write!(f, "{}\t{}", op, dst),
            Instruction::Binary(op, src, dst) => write!(f, "{}\t{}, {}", op, src, dst),
            Instruction::Div(div) => write!(f, "idivq\t{}", div),
            Instruction::Cqo => write!(f, "cqo"),
            Instruction::AllocateStack(offset) => write!(f, "subq\t${}, %rsp", offset),
            Instruction::Ret => write!(f, "movq\t%rbp, %rsp\n\tpopq\t%rbp\n\tretq"),
        }
    }
}

impl<'a> fmt::Display for FnDef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnDef::Function { name, instructions } = self;
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

impl<'a> fmt::Display for PgDef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PgDef::Program(func) = self;
        writeln!(f, "{}\t.section\t.note.GNU-stack,\"\",@progbits", func)
    }
}
