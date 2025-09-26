pub mod emitter;
pub mod lexer;
pub mod parser;
pub mod tacker;

use std::collections::HashMap;

#[test]
fn ctest() -> Result<(), String> {
    use std::{fs, path::Path, process::Command, io::Write};
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
                            varc: 1,
                            var_map: HashMap::new(),
                        };
                        let ast = tkstream.parse();
                        if set == "invalid_parse" || set == "invalid_semantics" {
                            assert!(ast.is_err());
                            return;
                        }
                        let tacky = tacker::Tacker {
                            tmpc: tkstream.varc,
                            lblc: 1,
                        }
                        .convert(ast.unwrap());
                        let asm = emitter::convert(tacky);
                        let mut output = fs::File::create(file.with_extension("s")).expect("Could not create output file");
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

pub fn compile(file: &str) -> String {
    let tokens: &[lexer::Token] = &lexer::lex(file).unwrap();
    let mut tkstream = parser::TokenStream {
        tokens,
        varc: 1,
        var_map: HashMap::new(),
    };

    // println!("{}", file);

    // for token in tokens {
    //     println!("{:?}", &token);
    // }

    let ast = tkstream.parse().unwrap();
    // println!("\n{:?}", &ast);

    let tacky = tacker::Tacker {
        tmpc: tkstream.varc,
        lblc: 1,
    }
    .convert(ast);
    // println!("\n{:?}", &tacky);

    let asm = emitter::convert(tacky);
    // println!("\n{:?}", &asm);
    // println!("\n{}", &asm);

    return format!("{}", &asm);
}

pub fn compile_dbg(file: &str) -> String {
    let tokens: &[lexer::Token] = &lexer::lex(file).unwrap();
    let mut tkstream = parser::TokenStream {
        tokens,
        varc: 1,
        var_map: HashMap::new(),
    };

    println!("{}", file);

    for token in tokens {
        println!("{:?}", &token);
    }

    let ast = tkstream.parse().unwrap();
    println!("\n{:?}", &ast);

    let tacky = tacker::Tacker {
        tmpc: tkstream.varc,
        lblc: 1,
    }
    .convert(ast);
    println!("\n{:?}", &tacky);

    let asm = emitter::convert(tacky);
    println!("\n{:?}", &asm);
    println!("\n{}", &asm);

    return format!("{}", &asm);
}