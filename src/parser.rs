use std::collections::HashMap;

use crate::lexer::Token;

#[derive(Debug)]
pub enum UnaryOp {
    BitwiseNot,
    Negative,
    Negation,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Conjunction,
    Disjunction,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Assign,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(i64),
    Var(&'a str, u32),
    Unary(UnaryOp, Box<Expression<'a>>),
    Binary(BinaryOp, Box<Expression<'a>>, Box<Expression<'a>>),
    Assignment(Box<Expression<'a>>, Box<Expression<'a>>, BinaryOp),
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub id: u32,
    pub init: Option<Expression<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Null,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Stmt(Statement<'a>),
    Decl(Declaration<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: Vec<BlockItem<'a>>,
}

#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>);

pub struct TokenStream<'a> {
    pub tokens: &'a [Token<'a>],
    pub varc: u32,
    pub var_map: HashMap<&'a str, u32>,
}

pub fn parse_unaryop(t: Token) -> Result<UnaryOp, String> {
    return Ok(match t {
        Token::Tilde => UnaryOp::BitwiseNot,
        Token::DoublePlus => UnaryOp::PreIncrement,
        Token::DoubleMinus => UnaryOp::PreDecrement,
        Token::Minus => UnaryOp::Negative,
        Token::Bang => UnaryOp::Negation,
        _ => Err(format!("Expected Unary Operator, but found {:?}", t))?,
    });
}

pub fn parse_binaryop(t: Token) -> Result<BinaryOp, String> {
    return Ok(match t {
        Token::Plus => BinaryOp::Add,
        Token::Minus => BinaryOp::Subtract,
        Token::Star => BinaryOp::Multiply,
        Token::Slash => BinaryOp::Divide,
        Token::Percent => BinaryOp::Modulo,
        Token::Ampersand => BinaryOp::BitwiseAnd,
        Token::Pipe => BinaryOp::BitwiseOr,
        Token::Carat => BinaryOp::BitwiseXor,
        Token::DoubleLeftChevron => BinaryOp::BitshiftLeft,
        Token::DoubleRightChevron => BinaryOp::BitshiftRight,
        Token::DoubleAmpersand => BinaryOp::Conjunction,
        Token::DoublePipe => BinaryOp::Disjunction,
        Token::Equal
        | Token::PlusEqual
        | Token::MinusEqual
        | Token::StarEqual
        | Token::SlashEqual
        | Token::PercentEqual
        | Token::AmpersandEqual
        | Token::PipeEqual
        | Token::CaratEqual
        | Token::DoubleLeftChevronEqual
        | Token::DoubleRightChevronEqual => BinaryOp::Assign,
        Token::DoubleEqual => BinaryOp::Equal,
        Token::BangEqual => BinaryOp::NotEqual,
        Token::LeftChevron => BinaryOp::Less,
        Token::LeftChevronEqual => BinaryOp::LessEqual,
        Token::RightChevron => BinaryOp::Greater,
        Token::RightChevronEqual => BinaryOp::GreaterEqual,
        _ => Err(format!("Expected Binary Operator, but found {:?}", t))?,
    });
}

pub fn parse_assign(t: Token) -> Result<BinaryOp, String> {
    return Ok(match t {
        Token::Equal => BinaryOp::Assign,
        Token::PlusEqual => BinaryOp::Add,
        Token::MinusEqual => BinaryOp::Subtract,
        Token::StarEqual => BinaryOp::Multiply,
        Token::SlashEqual => BinaryOp::Divide,
        Token::PercentEqual => BinaryOp::Modulo,
        Token::AmpersandEqual => BinaryOp::BitwiseAnd,
        Token::PipeEqual => BinaryOp::BitwiseOr,
        Token::CaratEqual => BinaryOp::BitwiseXor,
        Token::DoubleLeftChevronEqual => BinaryOp::BitshiftLeft,
        Token::DoubleRightChevronEqual => BinaryOp::BitshiftRight,
        _ => Err(format!("Expected Assignment Operator, but found {:?}", t))?,
    });
}

pub fn is_binaryop(t: Token) -> bool {
    return match t {
        Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Ampersand
        | Token::Pipe
        | Token::Carat
        | Token::DoubleLeftChevron
        | Token::DoubleRightChevron
        | Token::DoubleAmpersand
        | Token::DoublePipe
        | Token::Equal
        | Token::PlusEqual
        | Token::MinusEqual
        | Token::StarEqual
        | Token::SlashEqual
        | Token::PercentEqual
        | Token::AmpersandEqual
        | Token::PipeEqual
        | Token::CaratEqual
        | Token::DoubleLeftChevronEqual
        | Token::DoubleRightChevronEqual
        | Token::DoubleEqual
        | Token::BangEqual
        | Token::LeftChevron
        | Token::LeftChevronEqual
        | Token::RightChevron
        | Token::RightChevronEqual => true,
        _ => false,
    };
}

pub fn precedence(op: BinaryOp) -> i32 {
    return match op {
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 13,
        BinaryOp::Add | BinaryOp::Subtract => 12,
        BinaryOp::BitshiftLeft | BinaryOp::BitshiftRight => 11,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => 10,
        BinaryOp::Equal | BinaryOp::NotEqual => 9,
        BinaryOp::BitwiseAnd => 8,
        BinaryOp::BitwiseXor => 7,
        BinaryOp::BitwiseOr => 6,
        BinaryOp::Conjunction => 5,
        BinaryOp::Disjunction => 4,
        BinaryOp::Assign => 2,
    };
}

impl<'a> TokenStream<'a> {
    pub fn take(&mut self) -> Token<'a> {
        let t = self.tokens[0];
        self.tokens = &self.tokens[1..];
        return t;
    }

    pub fn expect(&mut self, t: Token) -> Result<&mut Self, String> {
        if self.tokens[0] == t {
            self.tokens = &self.tokens[1..];
            return Ok(self);
        } else {
            return Err(format!("Expected token: {:?}, but found {:?}", t, self.tokens[0]));
        }
    }

    pub fn identifier(&mut self) -> Result<&'a str, String> {
        let t = self.tokens[0];
        self.tokens = &self.tokens[1..];
        return match t {
            Token::Identifier(s) => Ok(s),
            _ => Err(format!("Expected identifier, but found {:?}", t)),
        };
    }

    pub fn parse_factor(&mut self) -> Result<Expression<'a>, String> {
        let t = self.take();
        let f = match t {
            Token::Constant(c) => Expression::Constant(c),
            Token::Identifier(n) => match self.var_map.get(n) {
                Some(&id) => Expression::Var(n, id),
                None => return Err(format!("Use of undeclared identifier '{}'", n)),
            },
            Token::Tilde | Token::DoublePlus | Token::DoubleMinus | Token::Minus | Token::Bang => match (t, self.parse_factor()?) {
                (_, f @ Expression::Var(_, _)) => Expression::Unary(parse_unaryop(t)?, Box::new(f)),
                (Token::DoublePlus | Token::DoubleMinus, _) => return Err(format!("Expected lvalue")),
                (_, f) => Expression::Unary(parse_unaryop(t)?, Box::new(f)),
            },
            Token::OpenParenthesis => {
                let inner = self.parse_expression(0)?;
                self.expect(Token::CloseParenthesis)?;
                inner
            }
            _ => return Err(format!("Expected expression, but found {:?}", t)),
        };

        return Ok(match (self.tokens[0], f) {
            (Token::DoublePlus | Token::DoubleMinus, f @ Expression::Var(_, _)) => Expression::Unary(
                match self.take() {
                    Token::DoublePlus => UnaryOp::PostIncrement,
                    Token::DoubleMinus => UnaryOp::PostDecrement,
                    _ => unreachable!(),
                },
                Box::new(f),
            ),
            (Token::DoublePlus | Token::DoubleMinus, _) => return Err(format!("Expected lvalue")),
            (_, f) => f,
        });
    }

    pub fn parse_expression(&mut self, min_prec: i32) -> Result<Expression<'a>, String> {
        let mut left = self.parse_factor()?;
        let mut t = self.tokens[0];
        while is_binaryop(t) && precedence(parse_binaryop(t)?) >= min_prec {
            t = self.take();
            let op = parse_binaryop(t)?;
            left = match t {
                Token::Equal
                | Token::PlusEqual
                | Token::MinusEqual
                | Token::StarEqual
                | Token::SlashEqual
                | Token::PercentEqual
                | Token::AmpersandEqual
                | Token::PipeEqual
                | Token::CaratEqual
                | Token::DoubleLeftChevronEqual
                | Token::DoubleRightChevronEqual => match left {
                    Expression::Var(_, _) => {
                        Expression::Assignment(Box::new(left), Box::new(self.parse_expression(precedence(op))?), parse_assign(t)?)
                    }
                    _ => return Err(format!("Left hand side of assignment must be an lvalue")),
                },
                _ => Expression::Binary(op, Box::new(left), Box::new(self.parse_expression(precedence(op) + 1)?)),
            };
            t = self.tokens[0];
        }
        return Ok(left);
    }

    pub fn parse_block_item(&mut self) -> Result<BlockItem<'a>, String> {
        let t = self.tokens[0];
        let b = Ok(match t {
            Token::Int => {
                let n = self.expect(Token::Int)?.identifier()?;
                if self.var_map.contains_key(n) {
                    return Err(format!("Variable '{}' already declared", n));
                }
                self.var_map.insert(n, self.varc);
                self.varc += 1;
                BlockItem::Decl(Declaration {
                    name: n,
                    id: self.varc - 1,
                    init: match self.tokens[0] {
                        Token::Equal => Some(self.expect(Token::Equal)?.parse_expression(0)?),
                        Token::Semicolon => None,
                        _ => {
                            return Err(format!("Expected semicolon or initialiser, but found {:?}", t));
                        }
                    },
                })
            }
            Token::Return => BlockItem::Stmt(Statement::Return(self.expect(Token::Return)?.parse_expression(0)?)),
            Token::Semicolon => {
                self.expect(Token::Semicolon)?;
                return Ok(BlockItem::Stmt(Statement::Null));
            }
            _ => BlockItem::Stmt(Statement::Expression(self.parse_expression(0)?)),
        });
        self.expect(Token::Semicolon)?;
        return b;
    }

    pub fn parse_function(&mut self) -> Result<Function<'a>, String> {
        let n = self.expect(Token::Int)?.identifier()?;
        self.expect(Token::OpenParenthesis)?
            .expect(Token::Void)?
            .expect(Token::CloseParenthesis)?
            .expect(Token::OpenBrace)?;
        let mut body: Vec<BlockItem> = Vec::new();
        let mut t = self.tokens[0];
        while t != Token::CloseBrace {
            body.push(self.parse_block_item()?);
            t = self.tokens[0];
        }
        self.expect(Token::CloseBrace)?;
        return Ok(Function { name: n, body });
    }

    pub fn parse(&mut self) -> Result<Program<'a>, String> {
        let func = self.parse_function()?;
        self.expect(Token::EndOfFile)?;
        return Ok(Program(func));
    }
}
