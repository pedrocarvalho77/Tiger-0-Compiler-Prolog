:- module(compiler, [tiger0_compiler/1]).

:- use_module(lexer).
:- use_module(parser).

tiger0_compiler(File) :-
    tokenize(File, Tokens),
    write(Tokens),
    write("\n\n"),
    
    parse(Tokens,Ast),
    write(Ast),
    write("\n\n").
