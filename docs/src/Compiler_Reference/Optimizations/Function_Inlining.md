# Function Inlining

Implementation Status: **WIP**

The overhead associated with calling and returning from a function can be eliminated by expanding the body of the function inline, and additional opportunities for optimization may be exposed as well. Inlining can be the most significant source of speed-up however excessive inlining can hurt speed.

```papyrus
int Function Add(int x, int y)
    Return x + y
EndFunction

int Function Sub(int x, int y)
    Return Add(x, -y)
EndFunction
```

The `Add` function can be expanded inline at the call site in the function `Sub` due to its low complexity:

```papyrus
int Function Sub(int x, int y)
    Return x + -y
EndFunction
```

This optimization often results in nested expressions which can be further optimized using [Expression Simplification](./Expression_Simplification.md):

```papyrus
int Function Sub(int x, int y)
    Return x - y
EndFunction
```
