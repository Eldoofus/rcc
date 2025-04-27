pub mod lexer;
pub mod parser;

use std::{env, fs, path::Path};

fn main() {
    if cfg!(not(target_os = "linux")) {
        println!("Only GNU/Linux is supported currently");
        return;
    }

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("No input file provided");
        return;
    }

    let path = Path::new(&args[1]);
    if let Some(ext) = path.extension()
        && ext == "c"
    {
        if path.try_exists().unwrap_or(false) {
            let file = fs::read_to_string(path).expect("Cannot read file");
            let tokens: &[lexer::Token] = &lexer::lex(file.as_str());
            let mut tkstream = parser::TokenStream { tokens };

            println!("{}", file.as_str());

            for token in tokens {
                println!("{:?}", &token);
            }

            println!("\n{:?}", tkstream.parse());
        } else {
            println!("Invalid file path");
        }
    } else {
        println!("Invalid file type: expected *.c");
    }
}
