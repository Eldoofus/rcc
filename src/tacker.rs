use crate::parser;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Complement,
    Negate
}

#[derive(Debug, Clone, Copy)]
pub enum Val<'b> {
    Constant(i32),
    Var(&'b str)
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction<'b> {
    Return(Val<'b>),
    Unary {
        op: UnaryOp,
        src: Val<'b>,
        dst: Val<'b>
    }
}

#[derive(Debug)]
pub enum FnDef<'a, 'b> {
    Function {
        name: &'a str,
        instructions: Vec<Instruction<'b>>
    }
}

#[derive(Debug)]
pub enum PgDef<'a, 'b> {
    Program(FnDef<'a, 'b>)
}

pub struct Tacker {
    pub tmps: Vec<String>,
}

impl Tacker {
    pub fn mk_tmp(&mut self) -> &str {
        self.tmps.push(String::from("tmp.") + self.tmps.len().to_string().as_str());
        return &self.tmps.last().unwrap();
    }
    
    // pub fn convert_expression<'a, F>(&'a mut self, e: parser::Expression, instructions: &mut Vec<Instruction<'a>>, mut f: F) -> Val<'a>
    // where F: FnMut(&'a mut Self, &mut Vec<Instruction<'a>>, Val<'a>) -> Val<'a> {
    //     let t = match e {
    //         parser::Expression::Constant(i) => Val::Constant(i),
    //         parser::Expression::Unary(op, inner) => {
    //             self.convert_expression(*inner, instructions, |tacker, instructions, src| {
    //                 let dst = Val::Var(tacker.mk_tmp());
    //                 instructions.push(Instruction::Unary { op: match op {
    //                     parser::UnaryOp::Complement => UnaryOp::Complement,
    //                     parser::UnaryOp::Negate => UnaryOp::Negate
    //                 }, src, dst });
    //                 dst
    //             })
    //         }
    //     };
    //     return f(self, instructions, t);
    // }
    
    // pub fn convert_expression<'a>(&'a mut self, e: parser::Expression, instructions: &mut Vec<Instruction<'a>>) -> Val<'a> {
    //     match e {
    //         parser::Expression::Constant(i) => (self, Val::Constant(i)),
    //         parser::Expression::Unary(op, inner) => {
    //             let src = self.convert_expression(*inner, instructions);
    //             let dst = Val::Var(tacker.mk_tmp());
    //             instructions.push(Instruction::Unary { op: match op {
    //                 parser::UnaryOp::Complement => UnaryOp::Complement,
    //                 parser::UnaryOp::Negate => UnaryOp::Negate
    //             }, src, dst });
    //             (tacker, dst)
    //         }
    //     }
    // }
}