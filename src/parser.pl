:- set_prolog_flag(double_quotes, chars).

parse(Tokens, Ast):-
    phrase(program(Ast), Tokens).

program([let_in, D, E]) --> ["let"], decl_list(D), ["in"], expr_seq(E).

decl_list([D])      --> decl(D).
decl_list([D|Ds])   --> decl(D), decl_list(Ds).

expr_seq([E])       --> expr(E).
expr_seq([E|Es])    --> expr(E), [","], expr_seq(Es). 

decl(D) --> var_decl(D).
decl(D) --> fun_decl(D).

var_decl(Var:=Num) --> ["var"], var(Var), [":="], expr(Num).

var(Var) --> identifier(Var).
id(Id) --> identifier(Id). 

expr(Num) --> integer(Num).
expr(Var) --> identifier(Var).
expr(E1 + E2) --> ["+"], expr(E1), expr(E2).
 

fun_decl(function(id(T,E))) --> ["function"], id(Id), ["("], type_fields(T), [")"], ["="], expr(E).
fun_decl(function(id(T,Tid,E))) --> ["function"], id(Id), ["("], type_fields(T), [")"], [":"], type_id(Tid), ["="], expr(E).

type_fields([T]) --> type_field(T).
type_fields([T|Ts]) --> type_field(T), [","], type_fields(Ts). 

type_field(Id:Type) --> id(Id), [":"], type_id(Type).

type_id(Int) --> ["int"].              
type_id(String) --> ["string"].

integer(X)      --> [Y], { number_string(X, Y) }.
identifier(X)   --> [Y], { atom_string(X, Y) }.