# Constant Propagation

Implementation Status: **WIP**

Constants assigned to a variable can be propagated through the flow graph and substituted at the use of the variable:

```papyrus
x = 10
y = x * 2
```

Optimized code with Constant Propagation:

```papyrus
x = 10
y = 10 * 2
```

Further optimization with [Constant Folding](./Constant_Folding.md):

```papyrus
x = 10
y = 20
```
