# Tiger-0-Compiler-Prolog

Front end of a compiler for Tiger-0, a subset of the Tiger language from *Modern Compiler Implementation in ML* (Appel), written in Prolog (SWI-Prolog). Academic project.

There is also a Haskell version: [Tiger-0-Compiler](https://github.com/pedrocarvalho77/Tiger-0-Compiler).

## Status

Work in progress. Done: lexer and parser (source to tokens to AST). Not done: type checking, intermediate code and code generation (`src/int_code.pl` and `src/teste1.pl` are unfinished drafts, not loaded by the compiler).

## Requirements

[SWI-Prolog](https://www.swi-prolog.org/) 9 or later (tested with 11.0.1).

## Usage

```sh
make run FILE=tests/t2.txt
```

Prints the token list, then the AST. Equivalent to running `tiger0_compiler('file')` from `src/compiler.pl`.

Input (`tests/t2.txt`):

```
let
    function fact(n: int) =
        if n>0 then n*fact(n-1)
        else 1
in
    printi(fact(10))
```

Output (AST):

```
[let_in,[func(id(fact),[id(n):type_int],if_then_else(gt(id(n),num(0)),mult(id(n),func_call(id(fact),[minus(id(n),num(1))])),num(1)))],[print_int(func_call(id(fact),[num(10)]))]]
```

## Tests

```sh
make test
```

Checks that `t1` and `t2` compile, and that the other examples still fail (they are known limitations, see below).

## Limitations

- String literals are not lexed (`tests/t3.txt`, `tests/test5.txt`).
- Array indexing (`a[i]`, `a[i] := x`) is not parsed (`tests/t4.txt`).
- Unary minus, `printi`, and operator precedence/associativity are only partly handled; see `src/notas.md`.
- The parser may return several ASTs on backtracking; only the first is printed.
