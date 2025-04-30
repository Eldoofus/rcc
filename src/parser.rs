use crate::lexer::Token;

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo
}

#[derive(Debug)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>)
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
        Token::Tilde => UnaryOp::Complement,
        Token::Minus => UnaryOp::Negate,
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
        _ => panic!("Expected Binary Operator, but found {:?}", t),
    };
}

pub fn is_binaryop(t: Token) -> bool {
    return match t {
        Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Percent => true,
        _ => false,
    }
}

pub fn precedence(op: BinaryOp) -> i32 {
    return match op {
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 2,
        BinaryOp::Add | BinaryOp::Subtract => 1
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
            Token::Tilde | Token::Minus => Expression::Unary(parse_unaryop(t), Box::new(self.parse_factor())),
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
            left  = Expression::Binary(op, Box::new(left), Box::new(tokens.parse_expression(precedence(op) + 1)));
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
