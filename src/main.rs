use std::env;
use std::error::Error;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process;

mod processor;
use processor::*;

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem processing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let tokens = lexer::Lexer::lex(&fs::read_to_string(&config.filepath)?)?;
    if config.debug {
        tokens.iter().for_each(|token| println!("{:?}", token))
    }

    let ast = parser::Parser::parse(tokens)?;
    if config.debug {
        println!("{:#?}", ast)
    }

    validator::Validator::validate(&ast)?;

    let assembly2 = generator::Generator::generate(&ast, generator::CallingConvention::CDECL)?;

    fs::File::create("assembly.s")?.write_all(&assembly2.as_bytes())?;
    process::Command::new("gcc")
        .args(["assembly.s", "-o", &config.filename])
        .output()?;

    if !config.debug {
        process::Command::new("rm").arg("assembly.s").output()?;
    }

    Ok(())
}

struct Config {
    filename: String,
    filepath: String,
    debug: bool,
}

impl Config {
    fn new(mut args: impl Iterator<Item = String>) -> Result<Self, &'static str> {
        args.next();
        let filepath = match args.next() {
            Some(arg) => arg,
            None => return Err("Expected filename"),
        };

        match Path::new(&filepath)
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("")
        {
            "c" => {}
            _ => return Err("Incorrect or no file extension found, .c file extension is required"),
        }

        let debug: bool;
        #[cfg(debug_assertions)]
        {
            debug = true;
        }

        #[cfg(not(debug_assertions))]
        {
            debug = false;
        }

        // because of how the test script by nora sandler is written,
        // it expects the executable to be inside the same folder as the source file.
        // however, when i am debugging i want the executable to be in the current directory
        // just like how gcc normally does it, that is what this is for

        let filename: String;
        let name = Path::new(&filepath)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap();

        if debug {
            filename = name.to_string();
        } else {
            filename = Path::new(&filepath)
                .parent()
                .unwrap()
                .join(name)
                .display()
                .to_string();
        }

        Ok(Config {
            filename,
            filepath,
            debug,
        })
    }
}
