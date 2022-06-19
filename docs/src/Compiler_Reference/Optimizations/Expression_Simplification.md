# Expression Simplification

Implementation Status: **WIP**

Some expressions can be simplified by replacing them with an equivalent expression that is more efficient:

```papyrus
x = i + 0
y = i - i
z = x + (-y * 1)
```

Programmers generally don't write such expressions, but these can be the result of [Function Inlining](./Function_Inlining.md) and need to be optimized:

```papyrus
x = i
y = 0
z = x
```

This optimization is not restricted to arithmetic operations but can also be applied to logical operations such as comparisons and boolean operations:

```papyrus
; before optimization:
is_valid = x == 0 && (y == 1 && x == 0)

; after optimization:
is_valid = x == 0 && y == 1
```
