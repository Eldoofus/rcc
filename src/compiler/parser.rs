use std::collections::HashMap;

use crate::compiler::lexer::Token;

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
    ConditionalMiddle, // i hate this
}

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(i64),
    Var(&'a str, u32),
    Unary(UnaryOp, Box<Expression<'a>>),
    Binary(BinaryOp, Box<Expression<'a>>, Box<Expression<'a>>),
    Assignment(Box<Expression<'a>>, Box<Expression<'a>>, BinaryOp),
    Conditional(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub id: u32,
    pub init: Option<Expression<'a>>,
}

#[derive(Debug)]
pub enum ForInit<'a> {
    InitDecl(Declaration<'a>),
    InitExp(Option<Expression<'a>>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression<'a>),
    Expression(Expression<'a>),
    If(Expression<'a>, Box<Statement<'a>>, Option<Box<Statement<'a>>>),
    Goto(&'a str, u32),
    Label(&'a str, u32, Box<Statement<'a>>),
    Compound(Block<'a>),
    Break(u32),
    Continue(u32),
    While(Expression<'a>, Box<Statement<'a>>, u32),
    DoWhile(Box<Statement<'a>>, Expression<'a>, u32),
    For(ForInit<'a>, Option<Expression<'a>>, Option<Expression<'a>>, Box<Statement<'a>>, u32),
    Null,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Stmt(Statement<'a>),
    Decl(Declaration<'a>),
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<BlockItem<'a>>);

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Program<'a>(pub Function<'a>, pub u32, pub u32);

#[derive(Debug)]
pub struct SymbolTable<'a> {
    pub tables: Vec<HashMap<&'a str, u32>>,
    pub count: u32,
}

#[derive(Debug)]
pub struct TokenStream<'a> {
    pub tokens: &'a [(Token<'a>, usize, usize)],
    var_map: SymbolTable<'a>,
    lbl_map: HashMap<&'a str, i32>,
    fix_map: HashMap<&'a str, (usize, usize)>,
    loopstk: Vec<u32>,
    lblc: u32,
}

pub fn parse_unaryop(t: Token, line: usize, col: usize) -> Result<UnaryOp, String> {
    return Ok(match t {
        Token::Tilde => UnaryOp::BitwiseNot,
        Token::DoublePlus => UnaryOp::PreIncrement,
        Token::DoubleMinus => UnaryOp::PreDecrement,
        Token::Minus => UnaryOp::Negative,
        Token::Bang => UnaryOp::Negation,
        _ => Err(format!("file.c:{}:{}: Expected Unary Operator, but found {:?}", line, col, t))?,
    });
}

pub fn parse_binaryop(t: Token, line: usize, col: usize) -> Result<BinaryOp, String> {
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
        Token::QuestionMark => BinaryOp::ConditionalMiddle,
        _ => Err(format!("file.c:{}:{}: Expected Binary Operator, but found {:?}", line, col, t))?,
    });
}

pub fn parse_assign(t: Token, line: usize, col: usize) -> Result<BinaryOp, String> {
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
        _ => Err(format!("file.c:{}:{}: Expected Assignment Operator, but found {:?}", line, col, t))?,
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
        | Token::RightChevronEqual
        | Token::QuestionMark => true,
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
        BinaryOp::ConditionalMiddle => 3,
        BinaryOp::Assign => 2,
    };
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        return SymbolTable {
            tables: vec![HashMap::new()],
            count: 0,
        };
    }

    pub fn enter_scope(&mut self) {
        self.tables.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.tables.pop();
        self.tables.last().expect("Cannot exit top-level scope");
    }

    pub fn find(&self, k: &str) -> Option<&u32> {
        return self.tables.iter().rev().find_map(|table| table.get(k));
    }

    pub fn contains(&self, k: &str) -> bool {
        return self.tables.last().unwrap().contains_key(k);
    }

    pub fn insert(&mut self, k: &'a str) {
        self.count += 1;
        self.tables.last_mut().unwrap().insert(k, self.count);
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [(Token<'a>, usize, usize)]) -> TokenStream<'a> {
        return TokenStream {
            tokens,
            var_map: SymbolTable::new(),
            lbl_map: HashMap::new(),
            fix_map: HashMap::new(),
            loopstk: Vec::new(),
            lblc: 0,
        };
    }

    pub fn take(&mut self) -> (Token<'a>, usize, usize) {
        let t = self.tokens[0];
        self.tokens = &self.tokens[1..];
        return t;
    }

    pub fn expect(&mut self, t: Token) -> Result<&mut Self, String> {
        if self.tokens[0].0 == t {
            self.tokens = &self.tokens[1..];
            return Ok(self);
        } else {
            return Err(format!(
                "file.c:{}:{}: Expected token: {:?}, but found {:?}",
                self.tokens[0].1, self.tokens[0].2, t, self.tokens[0].0
            ));
        }
    }

    pub fn identifier(&mut self) -> Result<(&'a str, usize, usize), String> {
        let (t, line, col) = self.tokens[0];
        self.tokens = &self.tokens[1..];
        return match t {
            Token::Identifier(s) => Ok((s, line, col)),
            _ => Err(format!("file.c:{}:{}: Expected identifier, but found {:?}", line, col, t)),
        };
    }

    pub fn parse_factor(&mut self) -> Result<Expression<'a>, String> {
        let (t, line, col) = self.take();
        let f = match t {
            Token::Constant(c) => Expression::Constant(c),
            Token::Identifier(n) => match self.var_map.find(n) {
                Some(&id) => Expression::Var(n, id),
                None => return Err(format!("file.c:{}:{}: Use of undeclared identifier '{}'", line, col, n)),
            },
            Token::Tilde | Token::DoublePlus | Token::DoubleMinus | Token::Minus | Token::Bang => match (t, self.parse_factor()?) {
                (_, f @ Expression::Var(_, _)) => Expression::Unary(parse_unaryop(t, line, col)?, Box::new(f)),
                (Token::DoublePlus | Token::DoubleMinus, _) => return Err(format!("file.c:{}:{}: Expected lvalue", line, col)),
                (_, f) => Expression::Unary(parse_unaryop(t, line, col)?, Box::new(f)),
            },
            Token::OpenParenthesis => {
                let inner = self.parse_expression(0)?;
                self.expect(Token::CloseParenthesis)?;
                inner
            }
            _ => return Err(format!("file.c:{}:{}: Expected expression, but found {:?}", line, col, t)),
        };

        return Ok(match (self.tokens[0].0, f) {
            (Token::DoublePlus | Token::DoubleMinus, f @ Expression::Var(_, _)) => Expression::Unary(
                match self.take().0 {
                    Token::DoublePlus => UnaryOp::PostIncrement,
                    Token::DoubleMinus => UnaryOp::PostDecrement,
                    _ => unreachable!(),
                },
                Box::new(f),
            ),
            (Token::DoublePlus | Token::DoubleMinus, _) => return Err(format!("file.c:{}:{}: Expected lvalue", self.tokens[0].1, self.tokens[0].2)),
            (_, f) => f,
        });
    }

    pub fn parse_expression(&mut self, min_prec: i32) -> Result<Expression<'a>, String> {
        let mut left = self.parse_factor()?;
        let (mut t, mut line, mut col) = self.tokens[0];
        while is_binaryop(t) && precedence(parse_binaryop(t, line, col)?) >= min_prec {
            (t, line, col) = self.take();
            let op = parse_binaryop(t, line, col)?;
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
                    Expression::Var(_, _) => Expression::Assignment(
                        Box::new(left),
                        Box::new(self.parse_expression(precedence(op))?),
                        parse_assign(t, line, col)?,
                    ),
                    _ => return Err(format!("file.c:{}:{}: Left hand side of assignment must be an lvalue", line, col)),
                },
                Token::QuestionMark => Expression::Conditional(
                    Box::new(left),
                    Box::new(self.parse_expression(0)?),
                    Box::new(self.expect(Token::Colon)?.parse_expression(precedence(op))?),
                ),
                _ => Expression::Binary(op, Box::new(left), Box::new(self.parse_expression(precedence(op) + 1)?)),
            };
            (t, line, col) = self.tokens[0];
        }
        return Ok(left);
    }

    pub fn parse_declaration(&mut self) -> Result<Declaration<'a>, String> {
        let (n, line, col) = self.expect(Token::Int)?.identifier()?;
        if self.var_map.contains(n) {
            return Err(format!("file.c:{}:{}: Variable '{}' already declared", line, col, n));
        }
        self.var_map.insert(n);
        return Ok(Declaration {
            name: n,
            id: self.var_map.count,
            init: match self.tokens[0] {
                (Token::Equal, _, _) => Some(self.expect(Token::Equal)?.parse_expression(0)?),
                (Token::Semicolon, _, _) => None,
                (t, line, col) => return Err(format!("file.c:{}:{}: Expected semicolon or initialiser, but found {:?}", line, col, t)),
            },
        });
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'a>, String> {
        let (t, line, col) = self.tokens[0];
        let s = Ok(match t {
            Token::Return => Statement::Return(self.expect(Token::Return)?.parse_expression(0)?),
            Token::Semicolon => Statement::Null,
            Token::If => {
                return Ok(Statement::If(
                    self.expect(Token::If)?.expect(Token::OpenParenthesis)?.parse_expression(0)?,
                    Box::new(self.expect(Token::CloseParenthesis)?.parse_statement()?),
                    match self.tokens[0].0 {
                        Token::Else => Some(Box::new(self.expect(Token::Else)?.parse_statement()?)),
                        _ => None,
                    },
                ));
            }
            Token::Goto => {
                let (l, line, col) = self.expect(Token::Goto)?.identifier()?;
                Statement::Goto(
                    l,
                    self.lbl_map
                        .entry(l)
                        .or_insert_with(|| {
                            self.fix_map.insert(l, (line, col));
                            self.lblc += 1;
                            -(self.lblc as i32)
                        })
                        .unsigned_abs(),
                )
            }
            Token::Identifier(l) if self.tokens[1].0 == Token::Colon => {
                self.tokens = &self.tokens[2..];
                if let Some(1..) = self.lbl_map.get(l) {
                    return Err(format!("file.c:{}:{}: Duplicate label '{}'", self.tokens[0].1, self.tokens[0].2, l));
                }
                self.fix_map.remove(l);
                return Ok(Statement::Label(
                    l,
                    self.lbl_map
                        .entry(l)
                        .and_modify(|id| *id *= -1)
                        .or_insert_with(|| {
                            self.lblc += 1;
                            self.lblc as i32
                        })
                        .unsigned_abs(),
                    Box::new(self.parse_statement()?),
                ));
            }
            Token::OpenBrace => {
                self.var_map.enter_scope();
                let mut body: Vec<BlockItem> = Vec::new();
                let mut t = self.expect(Token::OpenBrace)?.tokens[0].0;
                while t != Token::CloseBrace {
                    body.push(self.parse_block_item()?);
                    t = self.tokens[0].0;
                }
                self.expect(Token::CloseBrace)?;
                self.var_map.exit_scope();
                return Ok(Statement::Compound(Block(body)));
            }
            Token::Break => match self.expect(Token::Break)?.loopstk.last() {
                Some(&id) => Statement::Break(id),
                None => return Err(format!("file.c:{}:{}: Found break statement outside loop", line, col)),
            },
            Token::Continue => match self.expect(Token::Continue)?.loopstk.last() {
                Some(&id) => Statement::Continue(id),
                None => return Err(format!("file.c:{}:{}: Found continue statement outside loop", line, col)),
            },
            Token::While => {
                let cond = self.expect(Token::While)?.expect(Token::OpenParenthesis)?.parse_expression(0)?;
                self.expect(Token::CloseParenthesis)?.lblc += 2;
                self.loopstk.push(self.lblc);
                let stmt = Box::new(self.parse_statement()?);
                return Ok(Statement::While(cond, stmt, self.loopstk.pop().unwrap()));
            }
            Token::Do => {
                self.expect(Token::Do)?.lblc += 3;
                self.loopstk.push(self.lblc);
                let stmt = Box::new(self.parse_statement()?);
                Statement::DoWhile(
                    stmt,
                    self.expect(Token::While)?.expect(Token::OpenParenthesis)?.parse_expression(0)?,
                    self.expect(Token::CloseParenthesis)?.loopstk.pop().unwrap(),
                )
            }
            Token::For => {
                self.expect(Token::For)?.expect(Token::OpenParenthesis)?.var_map.enter_scope();
                let init = match self.tokens[0].0 {
                    Token::Int => ForInit::InitDecl(self.parse_declaration()?),
                    Token::Semicolon => ForInit::InitExp(None),
                    _ => ForInit::InitExp(Some(self.parse_expression(0)?)),
                };
                let cond = match self.expect(Token::Semicolon)?.tokens[0].0 {
                    Token::Semicolon => None,
                    _ => Some(self.parse_expression(0)?),
                };
                let post = match self.expect(Token::Semicolon)?.tokens[0].0 {
                    Token::CloseParenthesis => None,
                    _ => Some(self.parse_expression(0)?),
                };
                self.expect(Token::CloseParenthesis)?.lblc += 3;
                self.loopstk.push(self.lblc);
                let stmt = Box::new(self.parse_statement()?);
                self.var_map.exit_scope();
                return Ok(Statement::For(init, cond, post, stmt, self.loopstk.pop().unwrap()));
            }
            _ => Statement::Expression(self.parse_expression(0)?),
        });
        self.expect(Token::Semicolon)?;
        return s;
    }

    pub fn parse_block_item(&mut self) -> Result<BlockItem<'a>, String> {
        return Ok(match self.tokens[0].0 {
            Token::Int => {
                let decl = BlockItem::Decl(self.parse_declaration()?);
                self.expect(Token::Semicolon)?;
                decl
            }
            _ => BlockItem::Stmt(self.parse_statement()?),
        });
    }

    pub fn parse_function(&mut self) -> Result<Function<'a>, String> {
        let n = self.expect(Token::Int)?.identifier()?.0;
        self.expect(Token::OpenParenthesis)?
            .expect(Token::Void)?
            .expect(Token::CloseParenthesis)?
            .expect(Token::OpenBrace)?;
        let mut body: Vec<BlockItem> = Vec::new();
        let mut t = self.tokens[0].0;
        while t != Token::CloseBrace {
            body.push(self.parse_block_item()?);
            t = self.tokens[0].0;
        }
        self.expect(Token::CloseBrace)?;
        if !self.fix_map.is_empty() {
            let (label, (line, col)) = self.fix_map.iter().next().unwrap();
            return Err(format!("file.c:{}:{}: Could not find label: '{}'", line, col, label));
        }
        self.lbl_map.clear();
        return Ok(Function { name: n, body: Block(body) });
    }

    pub fn parse(&mut self) -> Result<Program<'a>, String> {
        let func = self.parse_function()?;
        self.expect(Token::EndOfFile)?;
        return Ok(Program(func, self.var_map.count, self.lblc));
    }
}
