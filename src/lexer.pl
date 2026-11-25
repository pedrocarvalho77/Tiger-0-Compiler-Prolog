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

mtlexem --> "/*", anything(_), "*/", !.
mtlexem --> [].

lexem(tok_diff) --> "<>".
lexem(tok_less_or_equal) --> "<=".
lexem(tok_greater_or_equal) --> ">=".
lexem(tok_plus) --> "+".
lexem(tok_minus) --> "-".
lexem(tok_mult) --> "*".
lexem(tok_div) --> "/".
lexem(tok_mod) --> "%".
lexem(tok_assign) --> ":=".
lexem(tok_equal) --> "=".
lexem(tok_less) --> "<".
lexem(tok_greater) --> ">".
lexem(tok_lparen) --> "(".
lexem(tok_rparen) --> ")".
lexem(tok_semicolon) --> ";".
lexem(tok_colon) --> ":".
lexem(tok_and) --> "&".
lexem(tok_or) --> "|".
lexem(tok_l_square_bracket) --> "[".
lexem(tok_r_square_bracket) --> "]".
lexem(tok_comma) --> ",".

lexem(tok_num(NA)) --> number(A), !, {number_chars(NA,A)}.

% an identifier is a keyword only if it matches a whole word ("index" is not "in" + "dex")
lexem(Tok) --> identifier(I), !, {atom_chars(A,I), ( keyword(A,K) -> Tok = K ; Tok = tok_id(A) )}.

keyword(int, tok_type_int).
keyword(string, tok_type_string).
keyword(intArray, tok_type_intArray).
keyword(var, tok_var).
keyword(if, tok_if).
keyword(then, tok_then).
keyword(else, tok_else).
keyword(while, tok_while).
keyword(do, tok_do).
keyword(function, tok_func).
keyword(let, tok_let).
keyword(in, tok_in).
keyword(end, tok_end).
keyword(scani, tok_scani).
keyword(printi, tok_printi).
keyword(for, tok_for).
keyword(to, tok_to).
keyword(break, tok_break).
keyword(print, tok_print).
keyword(of, tok_of).
    
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