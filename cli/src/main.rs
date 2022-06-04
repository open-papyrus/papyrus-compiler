use anyhow::{anyhow, Context};
use clap::Parser;
use papyrus_compiler_lexer::SpannedLexer;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    #[clap(parse(from_os_str))]
    input_path: PathBuf,
}

fn main() {
    let args: Args = Args::parse();
    match run(&args) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}

fn run(args: &Args) -> Result<(), anyhow::Error> {
    if !args.input_path.exists() {
        return Err(anyhow!(
            "Input path {} does not exist!",
            args.input_path.display()
        ));
    }

    let script = fs::read_to_string(args.input_path.clone())
        .with_context(|| format!("Unable to read input file {}", args.input_path.display()))?;

    let lexer = SpannedLexer::new(script.as_str());
    for token in lexer {
        println!("{}", token);
    }

    Ok(())
}
