use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    // Literals
    Constant(i64),       // [0-9]+\b
    Identifier(&'a str), // [a-zA-Z_]\w*\b

    // Keywords
    Int,      // int\b
    Void,     // void\b
    Return,   // return\b
    If,       // if\b
    Else,     // else\b
    Goto,     // goto\b
    Do,       // do\b
    While,    // while\b
    For,      // for\b
    Break,    // break\b
    Continue, // continue\b

    // MonoChar & DuoChar Tokens
    OpenParenthesis,         // \(
    CloseParenthesis,        // \)
    OpenBrace,               // \{
    CloseBrace,              // \}
    Semicolon,               // ;
    Tilde,                   // ~
    DoublePlus,              // \+\+
    Plus,                    // \+
    DoubleMinus,             // --
    Minus,                   // -
    Star,                    // \*
    Slash,                   // /
    Percent,                 // %
    Bang,                    // !
    DoubleAmpersand,         // &&
    Ampersand,               // &
    DoublePipe,              // \|\|
    Pipe,                    // \|
    Carat,                   // \^
    DoubleLeftChevron,       // <<
    LeftChevron,             // <
    DoubleRightChevron,      // >>
    RightChevron,            // >
    Equal,                   // =
    PlusEqual,               // \+=
    MinusEqual,              // -=
    StarEqual,               // \*=
    SlashEqual,              // /=
    PercentEqual,            // %=
    AmpersandEqual,          // &=
    PipeEqual,               // \|=
    CaratEqual,              // \^=
    DoubleLeftChevronEqual,  // <<=
    DoubleRightChevronEqual, // >>=
    DoubleEqual,             // ==
    BangEqual,               // !=
    LeftChevronEqual,        // <=
    RightChevronEqual,       // >=
    QuestionMark,            // ?
    Colon,                   // :

    // EOF
    EndOfFile,

    // Unknown
    Unknown(&'a str),
}

pub fn trim<'a>(mut s: &'a str, line: &mut usize, col: &mut usize) -> &'a str {
    while let Some(c) = s.get(0..1) {
        match c {
            " " => *col += 1,
            "\t" => *col += 8 - *col % 8,
            "\n" => {
                *line += 1;
                *col = 0
            }
            _ => return s,
        }
        s = &s[1..];
    }
    return s;
}

pub fn lex<'a>(input: &'a str) -> Result<Vec<(Token<'a>, usize, usize)>, String> {
    let lts = [
        (Token::Constant(0), Regex::new(r"^[0-9]+\b").unwrap()),
        (Token::Identifier(""), Regex::new(r"^[a-zA-Z_]\w*\b").unwrap()),
    ];
    let kws = [
        (Token::Int, Regex::new(r"^int\b").unwrap()),
        (Token::Void, Regex::new(r"^void\b").unwrap()),
        (Token::Return, Regex::new(r"^return\b").unwrap()),
        (Token::If, Regex::new(r"^if\b").unwrap()),
        (Token::Else, Regex::new(r"^else\b").unwrap()),
        (Token::Goto, Regex::new(r"^goto\b").unwrap()),
        (Token::Do, Regex::new(r"^do\b").unwrap()),
        (Token::While, Regex::new(r"^while\b").unwrap()),
        (Token::For, Regex::new(r"^for\b").unwrap()),
        (Token::Break, Regex::new(r"^break\b").unwrap()),
        (Token::Continue, Regex::new(r"^continue\b").unwrap()),
    ];
    let tks = [
        (Token::OpenParenthesis, Regex::new(r"^\(").unwrap()),
        (Token::CloseParenthesis, Regex::new(r"^\)").unwrap()),
        (Token::OpenBrace, Regex::new(r"^\{").unwrap()),
        (Token::CloseBrace, Regex::new(r"^\}").unwrap()),
        (Token::Semicolon, Regex::new(r"^;").unwrap()),
        (Token::Tilde, Regex::new(r"^~").unwrap()),
        (Token::DoublePlus, Regex::new(r"^\+\+").unwrap()),
        (Token::Plus, Regex::new(r"^\+").unwrap()),
        (Token::DoubleMinus, Regex::new(r"^--").unwrap()),
        (Token::Minus, Regex::new(r"^-").unwrap()),
        (Token::Star, Regex::new(r"^\*").unwrap()),
        (Token::Slash, Regex::new(r"^/").unwrap()),
        (Token::Percent, Regex::new(r"^%").unwrap()),
        (Token::Bang, Regex::new(r"^!").unwrap()),
        (Token::DoubleAmpersand, Regex::new(r"^&&").unwrap()),
        (Token::Ampersand, Regex::new(r"^&").unwrap()),
        (Token::DoublePipe, Regex::new(r"^\|\|").unwrap()),
        (Token::Pipe, Regex::new(r"^\|").unwrap()),
        (Token::Carat, Regex::new(r"^\^").unwrap()),
        (Token::DoubleLeftChevron, Regex::new(r"^<<").unwrap()),
        (Token::LeftChevron, Regex::new(r"^<").unwrap()),
        (Token::DoubleRightChevron, Regex::new(r"^>>").unwrap()),
        (Token::RightChevron, Regex::new(r"^>").unwrap()),
        (Token::Equal, Regex::new(r"^=").unwrap()),
        (Token::PlusEqual, Regex::new(r"^\+=").unwrap()),
        (Token::MinusEqual, Regex::new(r"^-=").unwrap()),
        (Token::StarEqual, Regex::new(r"^\*=").unwrap()),
        (Token::SlashEqual, Regex::new(r"^/=").unwrap()),
        (Token::PercentEqual, Regex::new(r"^%=").unwrap()),
        (Token::AmpersandEqual, Regex::new(r"^&=").unwrap()),
        (Token::PipeEqual, Regex::new(r"^\|=").unwrap()),
        (Token::CaratEqual, Regex::new(r"^\^=").unwrap()),
        (Token::DoubleLeftChevronEqual, Regex::new(r"^<<=").unwrap()),
        (Token::DoubleRightChevronEqual, Regex::new(r"^>>=").unwrap()),
        (Token::DoubleEqual, Regex::new(r"^==").unwrap()),
        (Token::BangEqual, Regex::new(r"^!=").unwrap()),
        (Token::LeftChevronEqual, Regex::new(r"^<=").unwrap()),
        (Token::RightChevronEqual, Regex::new(r"^>=").unwrap()),
        (Token::QuestionMark, Regex::new(r"^\?").unwrap()),
        (Token::Colon, Regex::new(r"^:").unwrap()),
    ];

    let unknown = Regex::new(r"^\S+?\b").unwrap();

    let mut tokens: Vec<(Token, usize, usize)> = Vec::new();
    let mut line = 0;
    let mut col = 0;
    let mut input = trim(input, &mut line, &mut col);

    while !input.is_empty() {
        let mut token: Token = Token::Semicolon;
        let mut maxlen = 0;

        for lt in &lts {
            if let Some(tok) = lt.1.find(input) {
                if tok.len() > maxlen {
                    maxlen = tok.len();
                    token = match lt.0 {
                        Token::Constant(_) => Token::Constant(tok.as_str().parse().expect("Invalid Integer literal encountered")),
                        Token::Identifier(_) => kws
                            .iter()
                            .find_map(|kw| kw.1.is_match(tok.as_str()).then_some(kw.0))
                            .unwrap_or(Token::Identifier(tok.as_str())),
                        _ => token,
                    }
                }
            }
        }

        for tk in &tks {
            if let Some(tok) = tk.1.find(input) {
                if tok.len() > maxlen {
                    maxlen = tok.len();
                    token = tk.0;
                }
            }
        }

        if maxlen == 0 {
            let tok = unknown.find(input).map(|x| x.as_str()).unwrap_or(input);
            return Err(format!("file.c:{}:{}: Syntax error: unrecognised token '{}' encountered", line, col, tok));
        }

        tokens.push((token, line, col));
        col += maxlen;
        input = trim(&input[maxlen..], &mut line, &mut col);
    }

    trim(input, &mut line, &mut col);
    tokens.push((Token::EndOfFile, line, col));
    return Ok(tokens);
}
