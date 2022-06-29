mod cache;

use crate::cache::SourceCache;
use anyhow::{anyhow, Context};
use clap::Parser;
use papyrus_compiler_diagnostics::ariadne_support::convert_to_report;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    #[clap(value_parser)]
    input_path: PathBuf,

    #[clap(long)]
    no_output: bool,
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

    let files: Vec<PathBuf> = if args.input_path.is_dir() {
        let dir = fs::read_dir(&args.input_path)
            .with_context(|| format!("Unable to read dir {}", args.input_path.display()))?;

        dir.filter_map(|file| match file {
            Ok(file) => match file.path().extension() {
                Some(extension) => match extension.to_ascii_lowercase().to_str() {
                    Some("psc") => Some(file.path()),
                    _ => None,
                },
                None => None,
            },
            Err(_) => None,
        })
        .collect()
    } else {
        vec![args.input_path.clone()]
    };

    if files.is_empty() {
        println!("Found no files in {}", args.input_path.display());
        return Ok(());
    }

    for file in files {
        println!("Compiling file {}", file.display());
        let (id, src) = cache.add_file(&file)?;
        let res = papyrus_compiler_core::compile_string(id, &src);

        match res {
            Ok(script) => {
                if !args.no_output {
                    println!("{:#?}", script)
                }
            }
            Err(diagnostics) => {
                for diagnostic in diagnostics {
                    convert_to_report(&diagnostic)
                        .print(&mut cache)
                        .with_context(|| "Unable to print error")?;
                }
            }
        }
    }

    Ok(())
}
