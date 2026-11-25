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
| MIPS back end | `src/mips.pl` | MIPS32 assembly (MARS/SPIM, syscall I/O) |
| Interpreter | `src/interp.pl` | runs the intermediate code directly (faster check of the front end) |

`src/compiler.pl` ties them together. `old/` keeps two older drafts (`int_code.pl`, `teste1.pl`) that are not loaded; `docs/notas.md` has working notes.

## Requirements

[SWI-Prolog](https://www.swi-prolog.org/) 9 or later (tested with 11.0.1).

## Usage

```sh
make run  FILE=tests/ok/factorial.txt   # print tokens, AST and intermediate code
make exec FILE=tests/ok/factorial.txt   # compile and execute
make mips FILE=tests/ok/factorial.txt   # print MIPS assembly
make test
make test-mips MARS=/path/to/Mars.jar   # run the assembly in MARS
```

Input (`tests/ok/factorial.txt`):

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

## MIPS back end

Plain stack machine: every variable and temporary lives in the frame (no register allocation, so the code is slow but simple). Static links give access to the variables of enclosing functions, arrays are heap blocks with a bounds check, I/O uses `syscall`. Runs in [MARS](https://dpetersanderson.github.io/) 4.5 (`java -jar Mars.jar nc file.s`); not tried in SPIM.

## Language

Supported: `int`, `string` and `intArray` types; `let ... in ... end`; `var` and `function` declarations (recursion and nested functions allowed); `if`, `while`, `for`, `break` (the variable of a `for` is read-only); `+ - * / %`, comparisons, `&`, `|`; array indexing; `printi`, `print`, `scani`; `/* ... */` comments; string escapes `\n`, `\t`.

## Tests

`make test` compiles and runs (in the interpreter) every program in `tests/ok/` and compares the output with the `.out` next to it (`primes` reads its input from `primes.in`). The expected outputs of the 8 queens (`eight_queens`) and Fibonacci (`fibonacci`) programs were generated with an independent Python implementation. `make test-mips` does the same on the generated MIPS code with MARS. `nested_functions.txt` covers nested functions. The programs in `tests/err/` must be rejected by the type checker.

## Limitations

- Function bodies without a declared return type must be `void` (as in Appel's Tiger).
- Nested functions are supported (they can read and assign the variables and parameters of the enclosing functions), but functions are not values: no function parameters or closures.
- Types: no records, no user-defined types (not part of Tiger-0).
- Only the first AST is used when the grammar is ambiguous.
