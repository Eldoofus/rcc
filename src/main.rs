pub mod emitter;
pub mod lexer;
pub mod parser;
pub mod tacker;

use std::io::Write;
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

            println!("{}", file.as_str());

            // for token in tokens {
            //     println!("{:?}", &token);
            // }

            let ast = tkstream.parse();
            println!("\n{:?}", &ast);

            let tacky = tacker::Tacker { tmpc: 1, lblc: 1 }.convert(ast);
            // println!("\n{:?}", &tacky);

            let asm = emitter::convert(tacky);
            //println!("\n{:?}", &asm);
            println!("\n{}", &asm);

            let mut file =
                fs::File::create(path.with_extension("s")).expect("Could not create output file");
            writeln!(&mut file, "{}", &asm).unwrap();

            Command::new("gcc")
                .arg(path.with_extension("s"))
                .arg("-o")
                .arg(path.with_extension(""))
                .output()
                .expect("Failed to execute gcc");

            println!(
                "Program returned: {}",
                Command::new(path.with_extension("")).status().unwrap()
            );
        } else {
            println!("Invalid file path");
        }
    } else {
        println!("Invalid file type: expected *.c");
    }
}
