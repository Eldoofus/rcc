#![feature(map_try_insert)]

pub mod assembler;
pub mod compiler; /* TODO Refactor */

use std::io::Write;
use std::{env, fs, path::Path, process::Command};

#[test]
fn astest() -> Result<(), String> {
    use std::{fs, path::Path, process::Command};
    for i in 1.. {
        let chapter = &format!("./tests/chapter_{}", i);
        let chapter = Path::new(chapter);
        if chapter.try_exists().unwrap_or(false) {
            for set in fs::read_dir(chapter).unwrap() {
                let set = set.unwrap();
                if set.file_name() != "valid" {
                    continue;
                }
                for file in fs::read_dir(set.path()).unwrap() {
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
                        let asm = compiler::compile(program.as_str());
                        // let mut output: Vec<u8> = Vec::new();
                        // writeln!(output, "{}", &asm).unwrap();
                        // let output = String::from_utf8(output).unwrap();
                        let obj = assembler::assemble(&format!("{}", &asm));
                        fs::write(file.with_extension("o"), obj).unwrap();

                        Command::new("gcc")
                            .arg(file.with_extension("o"))
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
                            .arg(file.with_extension("o"))
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

            let asm = compiler::compile_dbg(file.as_str());

            let mut file = fs::File::create(path.with_extension("s")).expect("Could not create output file");
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
    } else if let Some(ext) = path.extension()
        && ext == "s"
    {
        if path.try_exists().unwrap_or(false) {
            let file = fs::read_to_string(path).expect("Cannot read file");
            let obj = assembler::assemble(&file);

            fs::File::create(path.with_extension("o"))
                .expect("Could not create output file")
                .write(&obj)
                .expect("Unable to write to output file");

            Command::new("objdump")
                .arg("-d")
                .arg(path.with_extension("o"))
                .status()
                .expect("Failed to dump object file");

            Command::new("gcc")
                .arg(path.with_extension("o"))
                .arg("-o")
                .arg(path.with_extension(""))
                .status()
                .expect("Failed to execute gcc");

            let status = Command::new(path.with_extension("")).status().unwrap();

            println!("Program returned: {}", status.code().unwrap_or(-1));

            Command::new("rm")
                .arg(path.with_extension("o"))
                .arg(path.with_extension(""))
                .status()
                .expect("Failed to remove temp files");
        } else {
            println!("Invalid file path");
        }
    } else {
        println!("Invalid file type: expected *.c or *.s");
    }
}
