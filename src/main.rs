pub mod lexer;
pub mod parser;

use std::{env, fs, path::Path, process::Command};

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

            dbg!(tkstream.parse());

            //preprocessor
            Command::new("gcc")
                .arg("-E")
                .arg("-P")
                .arg(path)
                .arg("-o")
                .arg(path.with_extension("i"))
                .output()
                .expect("Failed to execute preprocessor");
            //compiler
            Command::new("gcc")
                .arg("-S")
                .arg("-O")
                .arg("-fno-asynchronous-unwind-tables")
                .arg("-fcf-protection=none")
                .arg(path.with_extension("i"))
                .arg("-o")
                .arg(path.with_extension("s"))
                .output()
                .expect("Failed to execute compiler");
            //assembler
            Command::new("gcc")
                .arg("-c")
                .arg(path.with_extension("s"))
                .arg("-o")
                .arg(path.with_extension("o"))
                .output()
                .expect("Failed to execute assembler");
            //linker
            Command::new("gcc")
                .arg(path.with_extension("o"))
                .arg("-o")
                .arg(path.with_extension(""))
                .output()
                .expect("Failed to execute linker");
        } else {
            println!("Invalid file path");
        }
    } else {
        println!("Invalid file type: expected *.c");
    }
}
