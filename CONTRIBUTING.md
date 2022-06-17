# Contributing

## Coding Requirements

- [rustup](https://rustup.rs/)
- IDE: [Visual Studio Code](https://code.visualstudio.com/) + [rust-analyzer](https://github.com/rust-lang/rust-analyzer) extension or [CLion](https://www.jetbrains.com/clion/) + [Rust](https://plugins.jetbrains.com/plugin/8182-rust/docs) extension

With rustup you can easily install toolchains and components. You need the **nightly** version to compile:

```bash
rustup toolchain install nightly
```

You should also install and use [clippy](https://github.com/rust-lang/rust-clippy) and [rustfmt](https://github.com/rust-lang/rustfmt):

```bash
rustup component add clippy rustfmt
```

You can configure your IDE to automatically run these two tools on save. I will not accept un-formatted code and clippy will also run as part of the CI.

The [WASM project](wasm) uses `wasm-bindgen` so you need the following:

```bash
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli
```

From the root directly you can use these two commands to export the WASM bindings:

```bash
cargo build -p "papyrus_compiler_wasm" --target wasm32-unknown-unknown --release
wasm-bindgen ./target/wasm32-unknown-unknown/release/papyrus_compiler_wasm.wasm --target web --out-dir ./compiler-explorer/src/wasm
```

## Documentation

The documentation is written in Markdown and be found [here](./docs). We use [mdBook](https://rust-lang.github.io/mdBook/) to create the documentation site from Markdown.

