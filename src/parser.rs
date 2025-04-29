use crate::lexer::Token;

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOp, Box<Expression>),
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

    pub fn parse_unaryop(&mut self) -> UnaryOp {
        let (_, t) = self.take();
        return match t {
            Token::Tilde => UnaryOp::Complement,
            Token::Minus => UnaryOp::Negate,
            _ => panic!("Expected Unary Operator, but found {:?}", t),
        };
    }

    pub fn parse_expression(&mut self) -> Expression {
        let (_, t) = self.take();
        return match t {
            Token::Constant(c) => Expression::Constant(c),
            Token::Tilde => {
                Expression::Unary(UnaryOp::Complement, Box::new(self.parse_expression()))
            }
            Token::Minus => Expression::Unary(UnaryOp::Negate, Box::new(self.parse_expression())),
            Token::OpenParenthesis => {
                let inner = self.parse_expression();
                self.expect(Token::CloseParenthesis);
                inner
            }
            _ => panic!("Expected expression, but found {:?}", t),
        };
    }

    pub fn parse_statement(&mut self) -> Statement {
        let e = self.expect(Token::Return).parse_expression();
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
