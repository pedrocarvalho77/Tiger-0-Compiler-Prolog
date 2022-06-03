%:- set_prolog_flag(double_quotes, chars).

lexem(int) --> [int].
lexem(string) --> [string]. 
lexem(+) --> [+].
lexem(-) --> [-].
lexem(*) --> [*].
lexem(/) --> [/].
%lexem(%) --> [%].
lexem(var) --> [var].
lexem(:=) --> [:=].
lexem(=) --> [=].
lexem(<>) --> [<>].
lexem(<) --> [<].
lexem(<=) --> [<=].
lexem(>) --> [>].
lexem(>=) --> [>=].
lexem(if) --> [if].
lexem(then) --> [then].
lexem(else) --> [else].
%lexem(tok_left) --> [(].
lexem(")") --> ")".
lexem(;) --> [;].
lexem(while) --> [while].
lexem(do) --> [do].
lexem(function) --> [function].
lexem(:) --> [:].
lexem("let") --> ["let"].
lexem(in) --> [in].
lexem(end) --> [end].
lexem(scani) --> [scani].
lexem(printi) --> [printi].
lexem(&) --> [&].
%lexem(|) --> [|].
lexem(for) --> [for].
lexem(to) --> [to].
lexem(break) --> [break].
lexem(print) --> [print].
%lexem([) --> [[].
%lexem(]) --> []].
lexem(of) --> [of].
lexem(tok_comma) --> [,].

lexem(int(NA)) -->  
    number(A),
    !,                     % longest input match
    {number_chars(NA,A)}.

lexem(id(IA)) -->
    identifier(I),
    !,                     % longest input match
    {atom_chars(IA,I)}.
    
whitespace -->
    [W],
    {char_type(W,space)}. % space is whitespace

identifier([L|Ls]) -->
    first_letter(L),
    ident(Ls).
    
ident([L|Ls]) -->
    first_letter(L),
    ident(Ls).
ident([]) -->
        [].
    
first_letter(L) -->
    uppercase(L).
first_letter(L) -->
    lowercase(L).
%first_letter("_") -->
%    [_]. 
    
uppercase(L) -->
    [L],                   % uppercase letters
    {char_type(L,upper)}.  
     
lowercase(L) -->
    [L],                   % lowercase letters
    {char_type(L,lower)}.  
    
number([D|Ds]) -->        % numbers are
    digit(D),              % a digit followed
    digits(Ds).            % by other digits
    
digits([D|Ds]) -->
    digit(D),
    digits(Ds).
digits([]) -->
    [].
    
digit(D) -->              % a single digit
    [D],
    {char_type(D,digit)}.
    
% entry point!!!!!!!!!!!!!
lexer(Input) -->             
    whitespace,           % whitespace is ignored
    lexer(Input).
lexer([Token | Tokens]) -->
    lexem(Token),   
    lexer(Tokens).
lexer([]) -->
    [].

tokenize(Input) :-
    phrase(lexer(L), Input),
    write(L),
    !.