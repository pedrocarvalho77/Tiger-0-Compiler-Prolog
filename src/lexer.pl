:- module(lexer, [tokenize/2]).

% entry point!!!!!!!!!!!!!
tokenize(Input, Result) :-
    phrase_from_file(lexer(Result), Input),
    !.

lexer(Input) -->             
    mtlexem,
    whitespace,           % whitespace is ignored
    lexer(Input).
lexer([Token | Tokens]) -->
    mtlexem,
    lexem(Token),   
    lexer(Tokens).
lexer([]) -->
    [].

whitespace -->
    [W],
    {char_type(W,space)}. % space is whitespace

anything([]) --> [].
anything([A|T]) --> [A], anything(T).

mtlexem --> "/*", anything(A), "*/", !.
mtlexem --> [].

lexem(tok_type_int) --> "int".
lexem(tok_type_string) --> "string".
lexem(tok_type_intArray) --> "intArray".
lexem(tok_plus) --> "+".
lexem(tok_minus) --> "-".
lexem(tok_mult) --> "*".
lexem(tok_div) --> "/".
lexem(tok_mod) --> "%".
lexem(tok_var) --> "var".
lexem(tok_assign) --> ":=".
lexem(tok_equal) --> "=".
lexem(tok_diff) --> "<>".
lexem(tok_less) --> "<".
lexem(tok_less_or_equal) --> "<=".
lexem(tok_greater) --> ">".
lexem(tok_greater_or_equal) --> ">=".
lexem(tok_if) --> "if".
lexem(tok_then) --> "then".
lexem(tok_else) --> "else".
lexem(tok_lparen) --> "(".
lexem(tok_rparen) --> ")".
lexem(tok_semicolon) --> ";".
lexem(tok_while) --> "while".
lexem(tok_do) --> "do".
lexem(tok_func) --> "function".
lexem(tok_colon) --> ":".
lexem(tok_let) --> "let".
lexem(tok_in) --> "in".
lexem(tok_end) --> "end".
lexem(tok_scani) --> "scani".
lexem(tok_printi) --> "printi".
lexem(tok_and) --> "&".
lexem(tok_or) --> "|".
lexem(tok_for) --> "for".
lexem(tok_to) --> "to".
lexem(tok_break) --> "break".
lexem(tok_print) --> "print".
lexem(tok_l_square_bracket) --> "[".
lexem(tok_r_square_bracket) --> "]".
lexem(tok_of) --> "of".
lexem(tok_comma) --> ",".

lexem(tok_num(NA)) --> number(A), !, {number_chars(NA,A)}.

lexem(tok_id(IA)) --> identifier(I), !, {atom_chars(IA,I)}.
    
identifier([L|Ls]) --> first_symbol(L), ident(Ls).
    
ident([L|Ls]) --> symbol(L), ident(Ls).
ident([]) --> [].
    
%csymf : Char is a letter (upper- or lowercase) or the underscore (_). These are valid first characters for C and Prolog symbols.
first_symbol(L) --> [L], {char_type(L,csymf)}. 

% csym : Char is a letter (upper- or lowercase), digit or the underscore (_). These are valid C and Prolog symbol characters.
symbol(L) --> [L], {char_type(L,csym)}.

number([D|Ds]) --> digit(D), digits(Ds).           
    
digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].
    
digit(D) --> [D],{char_type(D,digit)}.