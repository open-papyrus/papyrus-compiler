use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_parser(src: &str) -> Result<String, JsValue> {
    let res = papyrus_compiler_core::compile_string(0, src);
    match res {
        Ok(script) => Ok(format!("{:#?}", script)),
        Err(_reports) => Err(JsValue::from("Error compiling the script!")),
    }
}
