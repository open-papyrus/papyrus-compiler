# Optimizations

## The importance of optimizations

Optimizations are often the reason why compiled code runs so fast. Modern compilers have a wide range of optimizations available, some have significant performance impacts, reduce code size and make things easier for the CPU. Here is a C++ example ([godbolt](https://godbolt.org/z/ssP6PbEWj)):

```c++
int mulByConstant(int x) {
  return x * 65599;
}
```

The instructions generated are exactly what we imagine it to be, an `imul` (integer multiplication) of our input with `65599`:

```x86asm
mulByConstant(int):
    imul eax, edi, 65599
    ret
```

Now what will happen if we wrote some very bad code that does the same thing ([godbolt](https://godbolt.org/z/c6Kcr78Ex)):

```c++
int mulByConstant(int x) {
  return (x << 16) + (x << 6) - x;
  //         ^           ^
  //     x * 65536       |
  //                  x * 64
  // 65536x + 64x - 1x = 65599x
}
```

In this rather intimidating function we are doing the same thing (multiplying `x` with `65599`) however instead of using a multiplication we are using bit-shifts and additions.

```x86asm
mulByConstant(int):
    imul eax, edi, 65599
    ret
```

The result that the compiler generated is the same as before, it looked at our code and saw what we were trying to do. The compiler saw our arithmetic operations and understood we want to multiply by `65599`. It also knows that a single `imul` instruction is cheaper than whatever we wanted to do.

## Optimizing Papyrus

The CK compiler does not optimizations. It only tries to convert the raw code into a `.pex` file, doing only the bare minimum of work required to validate the code and produce an output. This is the reason why the code produces by a decompiler like [Champollion](https://github.com/Orvid/Champollion) is almost identical to the original code.

The next following pages contain information on every optimization done by the Open Papyrus Compiler.
