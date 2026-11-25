# Tiger-0-Compiler-Prolog

Front end of a compiler for Tiger-0, a subset of the Tiger language from *Modern Compiler Implementation in ML* (Appel), written in Prolog (SWI-Prolog). Academic project.

There is also a Haskell version: [Tiger-0-Compiler](https://github.com/pedrocarvalho77/Tiger-0-Compiler).

## Status

Work in progress. Done: lexer and parser (source to tokens to AST), covering all the examples in `tests/` (strings, arrays, `&`/`|`, comparisons, function calls). Not done: type checking, intermediate code and code generation.

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

Checks that every file in `tests/` lexes and parses.

## Limitations

- No type checking or code generation yet (`src/int_code.pl` and `src/teste1.pl` are unfinished drafts, not loaded by the compiler).
- Only the first AST is returned when the grammar is ambiguous.
