use std::fs;
use std::path::Path;

pub fn run_file<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    let content = fs::read_to_string(path)?;

    Ok(())
}

pub fn run(content: &str) -> anyhow::Result<()> {
    println!("{}", content);

    Ok(())
}
