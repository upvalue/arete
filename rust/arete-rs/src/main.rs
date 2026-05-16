mod printer;
mod reader;
mod runtime;
mod value;

use std::env;
use std::fs;
use std::process::ExitCode;

use reader::Reader;
use runtime::Runtime;

fn print_help() {
    println!("arete-rs 0.1.0");
    println!();
    println!("  --help: Print this message");
    println!("  --read <file>: Read and print S-expressions from a file without evaluating them");
    println!("  --eval <expr>: Evaluate expressions from the argument string");
    println!("  <file>: Evaluate Scheme source from a file");
}

fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|err| format!("arete-rs: failed to read {path}: {err}"))
}

fn read_and_print(rt: &mut Runtime, path: &str) -> Result<(), String> {
    let input = read_file(path)?;
    let mut reader = Reader::new(&input);
    loop {
        match reader.read(rt) {
            Ok(value) if value.is_eof() => break,
            Ok(value) => println!("{}", printer::write_value(&value)),
            Err(err) => return Err(format!("Reader error: {err}")),
        }
    }
    Ok(())
}

fn eval_source(rt: &mut Runtime, source: &str) -> Result<(), String> {
    let mut reader = Reader::new(source);
    let mut forms = Vec::new();
    loop {
        match reader.read(rt) {
            Ok(value) if value.is_eof() => break,
            Ok(value) => forms.push(value),
            Err(err) => return Err(format!("Reader error: {err}")),
        }
    }
    rt.eval_program(forms).map_err(|err| err.to_string())?;
    Ok(())
}

fn eval_file(rt: &mut Runtime, path: &str) -> Result<(), String> {
    let input = read_file(path)?;
    eval_source(rt, &input)
}

fn run() -> Result<(), String> {
    let mut rt = Runtime::new();
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        return Err("arete-rs: REPL is not implemented yet".to_string());
    }

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--help" => {
                print_help();
                return Ok(());
            }
            "--read" => {
                i += 1;
                let Some(path) = args.get(i) else {
                    return Err("arete-rs: expected file after --read".to_string());
                };
                read_and_print(&mut rt, path)?;
            }
            "--eval" => {
                i += 1;
                let Some(expr) = args.get(i) else {
                    return Err("arete-rs: expected expression after --eval".to_string());
                };
                eval_source(&mut rt, expr)?;
            }
            other if other.starts_with("--") => {
                return Err(format!("arete-rs: unsupported option {other}"));
            }
            path => eval_file(&mut rt, path)?,
        }
        i += 1;
    }

    Ok(())
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::FAILURE
        }
    }
}
