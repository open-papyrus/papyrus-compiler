# Contributing

## Prerequisites

Whether you want to contribute to the codebase or just to the documentation, there are a few things you should have before getting started:

- [git](https://git-scm.com/), [GitHub Desktop](https://desktop.github.com/), [GitKraken](https://www.gitkraken.com/git-client) or any other Git client
- [rustup](https://rustup.rs/)
- some editor: [Visual Studio Code](https://code.visualstudio.com/)

## Contributing to the Codebase

This project is almost exclusively written in Rust so here are some editors and plugins you should use to make your life easier:

- Free: [Visual Studio Code](https://code.visualstudio.com/) + [rust-analyzer](https://github.com/rust-lang/rust-analyzer) extension
- Paid (or free for students and if you have an open-source license): [CLion](https://www.jetbrains.com/clion/) + [Rust](https://plugins.jetbrains.com/plugin/8182-rust/docs) extension

If you have CLion, you can use the pre-configured Run Configurations in [.run/](./.run).

To get started, you first need the **nightly** toolchain to compile the code:

```bash
rustup toolchain install nightly
```

You should also install and use [clippy](https://github.com/rust-lang/rust-clippy) and [rustfmt](https://github.com/rust-lang/rustfmt):

```bash
rustup component add clippy rustfmt
```

If you add or change any dependencies you should run [cargo-deny](https://github.com/EmbarkStudios/cargo-deny) and make sure there are no issues:

```bash
cargo install cargo-deny
cargo deny check
```

### Useful commands

- clippy: `cargo clippy`
- rustfmt: `cargo fmt`
- benchmark: `cargo bench`
- cargo-deny: `cargo deny check`