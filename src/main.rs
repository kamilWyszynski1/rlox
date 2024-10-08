#![allow(warnings)]
#![feature(let_chains)]
extern crate core;
extern crate lazy_static;

use anyhow::anyhow;
use clap::{ArgAction, Parser};
use miette::{miette, LabeledSpan, Severity};
use std::io::{Read, Write};
use std::process::exit;

mod ast;
mod cli;
mod error;
mod interpreter;
mod representation;

use crate::ast::parser;
use crate::ast::resolver::Resolver;
use crate::error::error::{ErrorType, RLoxError};
use crate::interpreter::interpreter::Interpreter;
use crate::representation::lexer::Lexer;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to file to run.
    #[arg(short, long)]
    file_name: Option<String>,

    /// Prettifies errors.
    #[arg(
        long = "verbose",
        action = ArgAction::Set,
        default_value_t = true,
        // somehow clap has this option not properly supported in derive, so it needs to be a string
        default_missing_value = "true",
        num_args = 0..=1,
        require_equals = false,
    )]
    verbose: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter::new();
    match args.file_name {
        Some(ref file_name) => run_from_file(file_name, &args),
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

fn run_from_file(file: &str, args: &Args) -> anyhow::Result<()> {
    let mut file = std::fs::File::open(file)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut lexer = Lexer::new(&contents);
    let tokens = lexer.scan_tokens()?;
    let expressions = parser::Parser::new(tokens).parse()?;

    let resolver: Resolver = Resolver::new(Interpreter::new());
    match resolver.resolve(&expressions) {
        Ok(mut interpreter) => {
            interpreter.interpret(expressions)?;
        }
        Err(err) => match err.downcast::<RLoxError>() {
            Ok(rloxerr) => {
                if !args.verbose {
                    eprintln!("Error: {}", rloxerr.error);
                    exit(1);
                } else {
                    format_rlox_error(&rloxerr, &contents)
                }
            }
            Err(err) => {
                return Err(anyhow!(err));
            }
        },
    }
    Ok(())
}

fn format_rlox_error(err: &RLoxError, content: &str) {
    match &err.error {
        ErrorType::Resolve(msg) => {
            let report = miette!(
                // Those fields are optional
                severity = Severity::Error,
                help = msg,
                labels = vec![LabeledSpan::at(err.start..err.end, "here")],
                // Rest of the arguments are passed to `format!`
                // to form diagnostic message
                "Resolving error"
            )
            .with_source_code(content.to_string());
            println!("{:?}", report);
        }
    }
}
