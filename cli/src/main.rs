use anyhow::{anyhow, Context};
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

    let res = papyrus_compiler_core::compile_string(
        args.input_path.to_str().unwrap().to_string(),
        script.as_str(),
    );

    match res {
        Ok(script) => println!("{:#?}", script),
        Err(reports) => {
            for report in reports {
                report
                    .print(&mut cache)
                    .with_context(|| "Unable to print error")?;
            }
        }
    }

    Ok(())
}
