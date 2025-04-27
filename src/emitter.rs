use crate::parser;
use std::fmt;

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register
}

#[derive(Debug)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand
    },
    Ret
}

#[derive(Debug)]
pub enum FnDef<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instruction>
    }
}

#[derive(Debug)]
pub enum PgDef<'a> {
    Program(FnDef<'a>)
}

pub fn convert_expression(e: parser::Expression) -> Operand {
    let parser::Expression::Constant(i) = e;
    return Operand::Imm(i);
}

pub fn convert_statement(s: parser::Statement) -> Vec<Instruction> {
    let parser::Statement::Return(v) = s;
    return vec![Instruction::Mov { src: convert_expression(v), dst: Operand::Register }, Instruction::Ret];
}

pub fn convert_function(f: parser::FnDef) -> FnDef {
    let parser::FnDef::Function { name, body } = f;
    return FnDef::Function { name, instructions: convert_statement(body) };
}

pub fn convert(p: parser::PgDef) -> PgDef {
    let parser::PgDef::Program(f) = p;
    return PgDef::Program(convert_function(f));
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "${}", i),
            Operand::Register => write!(f, "%rax")
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "movq\t{}, {}", src, dst),
            Instruction::Ret => write!(f, "retq")
        }
    }
}

impl<'a> fmt::Display for FnDef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnDef::Function { name, instructions } = self;
        writeln!(f, "\t.globl\t{}\n{0}:", name)?;
        for i in instructions {
            writeln!(f, "\t{}", i)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for PgDef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PgDef::Program(func) = self;
        write!(f, "{}\t.section\t.note.GNU-stack,\"\",@progbits", func)
    }
}