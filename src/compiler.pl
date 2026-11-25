:- module(compiler, [tiger0_compiler/1, tiger0_run/1, main_compile/1, main_run/1, main_mips/1]).

:- use_module(lexer).
:- use_module(parser).
:- use_module(typecheck).
:- use_module(codegen).
:- use_module(interp).
:- use_module(mips).

% compile(+File, -Tokens, -Ast, -IR): lex, parse, type check, generate code
compile(File, Tokens, Ast, IR) :-
    tokenize(File, Tokens),
    ( parse(Tokens, Ast) -> true ; throw(error(tiger('syntax error'), _)) ),
    typecheck(Ast),
    codegen(Ast, IR).

% prints tokens, AST and intermediate code
tiger0_compiler(File) :-
    compile(File, Tokens, Ast, IR),
    format("~w~n~n~w~n~n", [Tokens, Ast]),
    print_ir(IR).

% compiles and executes the program
tiger0_run(File) :-
    compile(File, _, _, IR),
    run_ir(IR).

% prints MIPS assembly
tiger0_mips(File) :-
    compile(File, _, _, IR),
    mips_gen(IR).

% command line entry points: report errors and exit with status 1
main_compile(File) :- guarded(tiger0_compiler(File)).
main_run(File) :- guarded(tiger0_run(File)).
main_mips(File) :- guarded(tiger0_mips(File)).

guarded(Goal) :-
    catch(( Goal -> halt(0) ; format(user_error, "failed~n", []), halt(1) ),
          error(tiger(Msg), _),
          ( format(user_error, "error: ~w~n", [Msg]), halt(1) )).
