# Constant Folding

Implementation Status: **WIP**

Expressions with constant operands can be evaluated at compile time:

```papyrus
int Function GetValue()
    Return 10 * 2
EndFunction
```

The expression `10 * 2` can be optimized to just `20`:

```papyrus
int Function GetValue()
    Return 20
EndFunction
```

This optimization goes hand-in-hand with [Constant Propagation](./Constant_Propagation.md).
