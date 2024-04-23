use std::fs;
use std::env;
use std::path::Path;
mod lexer;
mod parser;

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

    if let Ok(contents) = fs::read_to_string(filename) {
        let tokens = lexer::Lexer::lex(&contents);
        parser::Parser::parse(tokens);
    }
    else {
        eprintln!("Error reading file");
    }
}
