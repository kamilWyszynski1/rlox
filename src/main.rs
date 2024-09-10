#![feature(let_chains)]
extern crate core;
extern crate lazy_static;

use clap::Parser;
use std::io::Write;

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
        Some(file_name) => unimplemented!(),
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
