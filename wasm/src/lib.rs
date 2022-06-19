use papyrus_compiler_diagnostics::disable_paint;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_parser(src: &str) -> String {
    disable_paint();

    let res = papyrus_compiler_core::compile_string(0, src);
    match res {
        Ok(script) => format!("{:#?}", script),
        Err(diagnostics) => {
            let mut res = String::with_capacity(diagnostics.len() * 50);

            for diagnostic in diagnostics {
                let to_append = format!(
                    "[{}{:03}] {:?} {}",
                    diagnostic.prefix(),
                    diagnostic.id(),
                    diagnostic.range(),
                    diagnostic.message()
                );
                res.push_str(to_append.as_str());
            }

            res
        }
    }
}
