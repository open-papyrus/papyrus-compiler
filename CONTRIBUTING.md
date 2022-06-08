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
