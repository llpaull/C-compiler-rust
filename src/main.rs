use std::fs;
use std::env;
use std::path::Path;
use std::process::Command;
use std::io::Write;
mod lexer;
mod parser;
mod assembler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }
    let filename = &args[1];

    let extension = Path::new(filename).extension().and_then(|s| s.to_str()).unwrap_or("");
    if extension != "c" {
        eprintln!("Incorrect file type: .{}, <filename>.c is required", extension);
        return;
    }

    let file = Path::new(filename).file_stem().and_then(|s| s.to_str()).unwrap_or("");
    let path = Path::new(filename).parent().unwrap_or(Path::new(""));
    let name = path.join(file);

    let name = name.to_str().unwrap_or("");

    if let Ok(contents) = fs::read_to_string(filename) {
        let tokens = lexer::lex(&contents);
        let ast = parser::parse(tokens).expect("error parsing tokens, invalid grammar");
        let assembly = assembler::assemble(&ast);

        let mut file = fs::File::create("assembly.s").expect("Error creating file");
        file.write_all(assembly.as_bytes()).expect("Error writing to file");

        Command::new("gcc").args(["assembly.s", "-o", name]).output().expect("Error compiling assembly");
        Command::new("rm").args(&["assembly.s"]).output().expect("Error removing assembly file");
    }
    else {
        eprintln!("Error reading file");
    }
}
