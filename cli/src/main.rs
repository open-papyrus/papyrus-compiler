use anyhow::{anyhow, Context};
use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;
use papyrus_compiler_core::cache::SourceCache;
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

    let mut cache = SourceCache::default();
    let script = cache.add_file(&args.input_path)?;

    let tokens = papyrus_compiler_core::run_lexer(
        args.input_path.to_str().unwrap().to_string(),
        script.as_str(),
    );

    let tokens = match tokens {
        Ok(tokens) => Some(tokens),
        Err(reports) => {
            for report in reports {
                report
                    .print(&mut cache)
                    .with_context(|| "Unable to print error")?;
            }

            None
        }
    };

    if tokens.is_none() {
        return Ok(());
    }

    let tokens = tokens.unwrap();

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
