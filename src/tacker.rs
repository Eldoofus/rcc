use crate::parser;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Complement,
    Negate,
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
                        parser::UnaryOp::Complement => UnaryOp::Complement,
                        parser::UnaryOp::Negate => UnaryOp::Negate,
                    },
                    src,
                    dst,
                });
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
