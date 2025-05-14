use crate::lexer::Token;

#[derive(Debug)]
pub enum UnaryOp {
    BitwiseNot,
    Negative,
    Negation,
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
}

#[derive(Debug)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum FnDef<'a> {
    Function { name: &'a str, body: Statement },
}

#[derive(Debug)]
pub enum PgDef<'a> {
    Program(FnDef<'a>),
}

pub struct TokenStream<'a> {
    pub tokens: &'a [Token<'a>],
}

pub fn parse_unaryop(t: Token) -> UnaryOp {
    return match t {
        Token::Tilde => UnaryOp::BitwiseNot,
        Token::Minus => UnaryOp::Negative,
        Token::Bang => UnaryOp::Negation,
        _ => panic!("Expected Unary Operator, but found {:?}", t),
    };
}

pub fn parse_binaryop(t: Token) -> BinaryOp {
    return match t {
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
        Token::DoubleEqual => BinaryOp::Equal,
        Token::BangEqual => BinaryOp::NotEqual,
        Token::LeftChevron => BinaryOp::Less,
        Token::LeftChevronEqual => BinaryOp::LessEqual,
        Token::RightChevron => BinaryOp::Greater,
        Token::RightChevronEqual => BinaryOp::GreaterEqual,
        _ => panic!("Expected Binary Operator, but found {:?}", t),
    };
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
    };
}

impl<'a> TokenStream<'a> {
    pub fn take(&mut self) -> (&mut Self, Token) {
        let t = self.tokens[0];
        self.tokens = &self.tokens[1..];
        return (self, t);
    }

    pub fn expect(&mut self, t: Token) -> &mut Self {
        if self.tokens[0] == t {
            self.tokens = &self.tokens[1..];
        } else {
            panic!("Expected token: {:?}, but found {:?}", t, self.tokens[0]);
        }
        return self;
    }

    pub fn parse_factor(&mut self) -> Expression {
        let (_, t) = self.take();
        return match t {
            Token::Constant(c) => Expression::Constant(c),
            Token::Tilde | Token::Minus => {
                Expression::Unary(parse_unaryop(t), Box::new(self.parse_factor()))
            }
            Token::OpenParenthesis => {
                let inner = self.parse_expression(0);
                self.expect(Token::CloseParenthesis);
                inner
            }
            _ => panic!("Expected expression, but found {:?}", t),
        };
    }

    pub fn parse_expression(&mut self, min_prec: i32) -> Expression {
        let mut left = self.parse_factor();
        let mut t = self.tokens[0];
        let mut tokens = self;
        while is_binaryop(t) && precedence(parse_binaryop(t)) >= min_prec {
            (tokens, t) = tokens.take();
            let op = parse_binaryop(t);
            left = Expression::Binary(
                op,
                Box::new(left),
                Box::new(tokens.parse_expression(precedence(op) + 1)),
            );
            t = tokens.tokens[0];
        }
        return left;
    }

    pub fn parse_statement(&mut self) -> Statement {
        let e = self.expect(Token::Return).parse_expression(0);
        self.expect(Token::Semicolon);
        return Statement::Return(e);
    }

    pub fn parse_fndef(&'a mut self) -> FnDef<'a> {
        let (tokens, t) = self.expect(Token::Int).take();
        let n = match t {
            Token::Identifier(s) => s,
            _ => panic!("Expected identifier, but found {:?}", t),
        };
        let s = tokens
            .expect(Token::OpenParenthesis)
            .expect(Token::Void)
            .expect(Token::CloseParenthesis)
            .expect(Token::OpenBrace)
            .parse_statement();
        tokens.expect(Token::CloseBrace);
        return FnDef::Function { name: n, body: s };
    }

    pub fn parse(&'a mut self) -> PgDef<'a> {
        return PgDef::Program(self.parse_fndef());
    }
}
