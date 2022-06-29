# Parser Diagnostics

Prefix for all diagnostics produces by the Parser: `P`

## `P001`: Unexpected Token

Level: [Error](./index.md#error)

The Parser encountered a Token where it expected something else.

## `P002`: Unexpected EOI

Level: [Error](./index.md#error)

The Parser encountered the _End of Input_ where it expected something else.

## `P003`: Expected EOI

Level: [Error](./index.md#error)

The Parser expected to find the _End of Input_ but found something else.
