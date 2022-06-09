use anyhow::{anyhow, Context};
use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;
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

    let tokens = papyrus_compiler_lexer::run_lexer(script.as_str());
    // println!("{:#?}", &tokens);

    let parse_result = papyrus_compiler_parser::parse_script(tokens);
    match parse_result {
        Ok(script) => {
            println!("{:#?}", script)
        }
        Err(errors) => {
            for error in errors {
                Report::build(ReportKind::Error, (), error.span().start)
                    .with_message("You fucked up")
                    .with_label(
                        Label::new(error.span()).with_message("Look here, this is fucking stupid"),
                    )
                    .finish()
                    .print(Source::from(&script))
                    .unwrap();
            }

            // println!("{:#?}", errors)
        }
    }

    Ok(())
}
