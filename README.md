# Tiger-0-Compiler-Prolog

Compiler for Tiger-0, a subset of the Tiger language from *Modern Compiler Implementation in ML* (Appel), written in Prolog (SWI-Prolog). Academic project.

There is also a Haskell version: [Tiger-0-Compiler](https://github.com/pedrocarvalho77/Tiger-0-Compiler).

## Pipeline

| Step | File | Output |
|------|------|--------|
| Lexer | `src/lexer.pl` | token list |
| Parser | `src/parser.pl` | AST |
| Type checker | `src/typecheck.pl` | accepts the AST or reports an error |
| Code generator | `src/codegen.pl` | intermediate code (flat list: labels, jumps, moves, calls...) |
| Interpreter | `src/interp.pl` | runs the intermediate code, to check the compiler end to end |

`src/compiler.pl` ties them together. There is no native back end (MIPS or similar): the intermediate code is the last stage. `src/int_code.pl` and `src/teste1.pl` are older drafts, not loaded.

## Requirements

[SWI-Prolog](https://www.swi-prolog.org/) 9 or later (tested with 11.0.1).

## Usage

```sh
make run  FILE=tests/t2.txt   # print tokens, AST and intermediate code
make exec FILE=tests/t2.txt   # compile and execute
make test
```

Input (`tests/t2.txt`):

```
let
    function fact(n: int): int =
        if n>0 then n*fact(n-1)
        else 1
in
    printi(fact(10))
```

`make exec` prints `3628800`. `make run` prints the intermediate code:

```
main:
    call(t(8),fact,[num(10)])
    printi(t(8))
    ret(num(0))

function fact([v(n_0)]):
    cjump(v(n_0),gt,num(0),'L2','L3')
L2:
    binop(t(5),minus,v(n_0),num(1))
    ...
```

Intermediate code: `call(Dest,Function,Hops,Args)` carries the number of static links to follow; a variable `v(Up,X)` lives `Up` frames above the current one.

Errors are reported on stderr with exit status 1, for example `error: expected int but found string in string(hello)`.

## Language

Supported: `int`, `string` and `intArray` types; `let ... in ... end`; `var` and `function` declarations (recursion and nested functions allowed); `if`, `while`, `for`, `break` (the variable of a `for` is read-only); `+ - * / %`, comparisons, `&`, `|`; array indexing; `printi`, `print`, `scani`; `/* ... */` comments; string escapes `\n`, `\t`.

## Tests

`make test` compiles and runs every program in `tests/` and compares the output with `tests/*.out` (`t3` reads its input from `t3.in`). The expected outputs of the 8 queens (`test5`) and Fibonacci (`t4`) programs were generated with an independent Python implementation. `nested.txt` covers nested functions. `bad_type.txt` and `bad_for.txt` must be rejected by the type checker.

## Limitations

- Function bodies without a declared return type must be `void` (as in Appel's Tiger).
- Nested functions are supported (they can read and assign the variables and parameters of the enclosing functions), but functions are not values: no function parameters or closures.
- Types: no records, no user-defined types (not part of Tiger-0).
- The interpreter does not free the variables of finished calls, so very long runs use more memory.
- Only the first AST is used when the grammar is ambiguous.
