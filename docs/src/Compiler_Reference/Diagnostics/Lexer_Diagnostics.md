# Lexer Diagnostics

Prefix for all diagnostics produces by the Lexer: `L`

## `L001`: Unknown Token

Level: [Error](./index.md#error)

The Lexer encountered an unknown token.

Example:

```papyrus
^
```

## `L001`: ParseIntError

Level: [Error](./index.md#error)

An integer could not be parsed. This error is part of the Rust Standard Library: [std::num::ParseIntError](https://doc.rust-lang.org/std/num/struct.ParseIntError.html).

Example:

```papyrus
x = 2147483648  ; positive integer overflow
x = -2147483649 ; negative integer overflow
```

## `L002`: ParseFloatError

Level: [Error](./index.md#error)

A floating point number could not be parsed. This error is part of the Rust Standard Library: [std::num::ParseFloatError](https://doc.rust-lang.org/std/num/struct.ParseFloatError.html)

## `L003`: Number is not finite

Level: [Error](./index.md#error)

A floating point number could not be parsed because it is either Infinite or Not-a-Number (NaN).

Example:

```papyrus
x = 3402823470000000000000000000000000000000.0  ; positive infinity
x = -3402823470000000000000000000000000000000.0 ; negative infinity
```
