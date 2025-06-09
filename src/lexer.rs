use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    // Literals
    Constant(i64),       // [0-9]+\b
    Identifier(&'a str), // [a-zA-Z_]\w*\b

    // Keywords
    Int,    // int\b
    Void,   // void\b
    Return, // return\b

    // MonoChar & DuoChar Tokens
    OpenParenthesis,    // \(
    CloseParenthesis,   // \)
    OpenBrace,          // \{
    CloseBrace,         // \}
    Semicolon,          // ;
    Tilde,              // ~
    DoublePlus,         // \+\+
    Plus,               // \+
    DoubleMinus,        // --
    Minus,              // -
    Star,               // \*
    Slash,              // /
    Percent,            // %
    Bang,               // !
    DoubleAmpersand,    // &&
    Ampersand,          // &
    DoublePipe,         // \|\|
    Pipe,               // \|
    Carat,              // \^
    DoubleLeftChevron,  // <<
    LeftChevron,        // <
    DoubleRightChevron, // >>
    RightChevron,       // >
    Equal,              // =
    DoubleEqual,        // ==
    BangEqual,          // !=
    LeftChevronEqual,   // <=
    RightChevronEqual,  // >=

    // EOF
    EndOfFile,

    // Unknown
    Unknown(&'a str),
}

pub fn lex<'a>(input: &'a str) -> Result<Vec<Token<'a>>, String> {
    let lts = [
        (Token::Constant(0), Regex::new(r"^[0-9]+\b").unwrap()),
        (
            Token::Identifier(""),
            Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
        ),
    ];
    let kws = [
        (Token::Int, Regex::new(r"^int\b").unwrap()),
        (Token::Void, Regex::new(r"^void\b").unwrap()),
        (Token::Return, Regex::new(r"^return\b").unwrap()),
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
        (Token::DoubleEqual, Regex::new(r"^==").unwrap()),
        (Token::BangEqual, Regex::new(r"^!=").unwrap()),
        (Token::LeftChevronEqual, Regex::new(r"^<=").unwrap()),
        (Token::RightChevronEqual, Regex::new(r"^>=").unwrap()),
    ];

    let unknown = Regex::new(r"^\S+?\b").unwrap();

    let mut tokens: Vec<Token> = Vec::new();
    let mut input = input.trim();

    while !input.is_empty() {
        let mut token: Token = Token::Semicolon;
        let mut maxlen = 0;

        for lt in &lts {
            if let Some(tok) = lt.1.find(input) {
                if tok.len() > maxlen {
                    maxlen = tok.len();
                    token = match lt.0 {
                        Token::Constant(_) => Token::Constant(
                            tok.as_str()
                                .parse()
                                .expect("Invalid Integer literal encountered"),
                        ),
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
            return Err(format!(
                "Syntax error: unrecognised token '{}' encountered",
                tok
            ));
        }

        tokens.push(token);
        input = input[maxlen..].trim()
    }

    tokens.push(Token::EndOfFile);
    return Ok(tokens);
}
