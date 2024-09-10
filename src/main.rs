#![feature(let_chains)]
extern crate lazy_static;
use clap::Parser;
use std::io::Write;

mod ast;
mod cli;
mod interpreter;
mod representation;

use crate::representation::interpreter::run;
use representation::interpreter::run_file;

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
        Some(file_name) => run_file(file_name),
        None => loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;

            if input.trim() == "exit" {
                return Ok(());
            }
            run(input.trim())?
        },
    }?;
    Ok(())
}
