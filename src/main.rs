pub mod emitter;
pub mod lexer;
pub mod parser;
pub mod tacker;

use std::io::Write;
use std::{env, fs, path::Path, process::Command};

#[test]
fn test() -> Result<(), String> {
    for i in 1.. {
        let chapter = &format!("./tests/chapter_{}", i);
        let chapter = Path::new(chapter);
        if chapter.try_exists().unwrap_or(false) {
            for set in fs::read_dir(chapter).unwrap() {
                let set = set.unwrap();
                for file in fs::read_dir(set.path()).unwrap() {
                    let set = set.file_name();
                    let set = set.to_str().unwrap();
                    let file = file.unwrap().path();
                    let file = file.as_path();

                    let test = |file: &Path| {
                        println!("{}", file.display());
                        Command::new("gcc")
                            .arg(file)
                            .arg("-E")
                            .arg("-P")
                            .arg("-o")
                            .arg(file.with_extension("i"))
                            .status()
                            .expect("Failed to execute gcc");
                        let program = fs::read_to_string(file.with_extension("i")).unwrap();
                        Command::new("rm")
                            .arg(file.with_extension("i"))
                            .status()
                            .expect("Failed to remove temporary files");
                        let tokens = lexer::lex(program.as_str());
                        if set == "invalid_lex" {
                            assert!(tokens.is_err());
                            return;
                        }
                        let mut tkstream = parser::TokenStream {
                            tokens: &tokens.unwrap(),
                        };
                        let ast = tkstream.parse();
                        if set == "invalid_parse" {
                            assert!(ast.is_err());
                            return;
                        }
                        let tacky = tacker::Tacker { tmpc: 1, lblc: 1 }.convert(ast.unwrap());
                        let asm = emitter::convert(tacky);
                        let mut output = fs::File::create(file.with_extension("s"))
                            .expect("Could not create output file");
                        writeln!(&mut output, "{}", &asm).unwrap();
                        Command::new("gcc")
                            .arg(file.with_extension("s"))
                            .arg("-o")
                            .arg(file.with_extension(""))
                            .status()
                            .expect("Failed to execute gcc");
                        let status = Command::new(file.with_extension("")).status().unwrap();
                        Command::new("gcc")
                            .arg(file)
                            .arg("-o")
                            .arg(file.with_extension(""))
                            .status()
                            .expect("Failed to execute gcc");
                        let expected = Command::new(file.with_extension("")).status().unwrap();
                        Command::new("rm")
                            .arg(file.with_extension("s"))
                            .arg(file.with_extension(""))
                            .status()
                            .expect("Failed to remove temporary files");
                        assert_eq!(status.code().unwrap_or(-1), expected.code().unwrap_or(-1));
                    };

                    if file.is_dir() {
                        for file in fs::read_dir(file).unwrap() {
                            let file = file.unwrap().path();
                            let file = file.as_path();
                            test(file);
                        }
                    } else {
                        test(file);
                    }
                }
            }
        } else {
            break;
        }
    }
    Ok(())
}

fn main() {
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
            Command::new("gcc")
                .arg(path)
                .arg("-E")
                .arg("-P")
                .arg("-o")
                .arg(path.with_extension("i"))
                .status()
                .expect("Failed to execute gcc");
            let file = fs::read_to_string(path.with_extension("i")).expect("Cannot read file");
            let tokens: &[lexer::Token] = &lexer::lex(file.as_str()).unwrap();
            let mut tkstream = parser::TokenStream { tokens };

            println!("{}", file.as_str());

            // for token in tokens {
            //     println!("{:?}", &token);
            // }

            let ast = tkstream.parse().unwrap();
            println!("\n{:?}", &ast);

            let tacky = tacker::Tacker { tmpc: 1, lblc: 1 }.convert(ast);
            println!("\n{:?}", &tacky);

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
                .status()
                .expect("Failed to execute gcc");

            let status = Command::new(path.with_extension("")).status().unwrap();

            println!("Program returned: {}", status.code().unwrap_or(-1));

            Command::new("rm")
                .arg(path.with_extension("i"))
                .arg(path.with_extension("s"))
                .arg(path.with_extension(""))
                .status()
                .expect("Failed to remove temp files");
        } else {
            println!("Invalid file path");
        }
    } else {
        println!("Invalid file type: expected *.c");
    }
}
