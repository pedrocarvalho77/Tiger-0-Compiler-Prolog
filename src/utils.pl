:- module(utils, [convert/2]).

convert([],[]).
convert([X|Xs],[Y|Ys]) :- trans(X,Y), convert(Xs,Ys).

trans(tok_num(N),Result) :- concate1(N,Result).
trans(tok_id(Id),Result) :- concate2(Id,Result).

trans(tok_type_int, "int"). 
trans(tok_type_string, "string"). 
trans(tok_plus, "+").
trans(tok_minus, "-").
trans(tok_mult, "*").
trans(tok_div, "/").
trans(tok_mod, "%").
trans(tok_var, "var").
trans(tok_assign, ":=").
trans(tok_equal, "=").
trans(tok_diff, "<>").
trans(tok_less, "<").
trans(tok_less_or_equal, "<=").
trans(tok_greater, ">").
trans(tok_greater_or_equal, ">=").
trans(tok_if, "if").
trans(tok_then, "then").
trans(tok_else, "else").
trans(tok_lparen, "(").
trans(tok_rparen, ")").
trans(tok_semicolon, ";").
trans(tok_while, "while").
trans(tok_do, "do").
trans(tok_func, "function").
trans(tok_colon, ":").
trans(tok_let, "let").
trans(tok_in, "in").
trans(tok_end, "end").
trans(tok_scani, "scani").
trans(tok_printi, "printi").
trans(tok_and, "&").
trans(tok_or, "|").
trans(tok_for, "for").
trans(tok_to, "to").
trans(tok_break, "break").
trans(tok_print, "print").
trans(tok_l_square_bracket, "[").
trans(tok_r_square_bracket, "]").
trans(tok_of, "of").
trans(tok_comma, ",").


concate1(N,Result) :-
    number_string(N,String),
    string_concat("num(", String, Aux),
    string_concat(Aux, ")", Result).

concate2(Id,Result) :-
    atom_string(Id,String),
    string_concat("id(", String, Aux),
    string_concat(Aux, ")", Result).