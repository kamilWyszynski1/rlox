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
            let tokens = lexer.scan_tokens()?;
            let mut pareser = parser::Parser::new(tokens);
            let expressions = pareser.parse()?;
            let mut interpreter = Interpreter::new();
            let result = interpreter.interpret(&expressions)?;
            println!("{:?}", result);
        },
    }
    Ok(())
}
