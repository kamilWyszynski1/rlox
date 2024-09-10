#![feature(let_chains)]
extern crate core;
extern crate lazy_static;

use clap::Parser;
use std::io::{Read, Write};

mod ast;
mod cli;
mod interpreter;
mod representation;

use crate::ast::parser;
use crate::interpreter::interpreter::Interpreter;
use crate::representation::lexer::Lexer;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    file_name: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter::new();
    match args.file_name {
        Some(file_name) => run_from_file(&file_name),
        None => loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;

            if input.trim() == "exit" {
                return Ok(());
            }

            let mut lexer = Lexer::new(&input);
            match lexer.scan_tokens() {
                Ok(tokens) => {
                    let mut pareser = parser::Parser::new(tokens);
                    match pareser.parse() {
                        Ok(expressions) => match interpreter.interpret(expressions) {
                            Ok(_) => {}
                            Err(err) => {
                                eprintln!("{}", err)
                            }
                        },
                        Err(err) => eprintln!("{}", err),
                    }
                }
                Err(err) => eprintln!("{}", err),
            }
        },
    }
}

fn run_from_file(file: &str) -> anyhow::Result<()> {
    let mut file = std::fs::File::open(file)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut interpreter = Interpreter::new();
    let mut lexer = Lexer::new(&contents);
    let tokens = lexer.scan_tokens()?;
    let expressions = parser::Parser::new(tokens).parse()?;
    interpreter.interpret(expressions)
}
