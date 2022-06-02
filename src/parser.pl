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

%var_decl([varDecl, decl, var, X, num]) --> ["var"], id(X), [":="], ["expr"].
var_decl(Var=Num) --> ["var"], var(Var), [":="], expr(Num).

var(Var) --> identifier(Var).
id(Id) --> identifier(Id). 
%expr(Num) --> [Num].
expr(Num) --> integer(Num).

integer(X)      --> [Y], { number_string(X, Y) }.
identifier(X)   --> [Y], { atom_string(X, Y) }.

fun_decl(func, X, E) --> ["function"], id(Id), ["("], ["type_fields"], [")"], ["="], expr(E).
/*fun_decl([func, var, X, type_field, Y, E]) --> ["function"], id(X), ["("], type_fields(Y), [")"], ["="], expr(E).
fun_decl() --> ["function"], id, ["("], type_fields(Y), [")"], [":"], type_id, ["="], expr(E).

type_fields() --> type_field().
type_fields() --> type_field(), [","], type_fields(). 
*/
type_field() --> id, [":"], type_id().

type_id(int) --> ["int"].              
type_id(string) --> ["string"].

%expr(E) --> num(E).



















