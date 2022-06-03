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

var_decl(Var:=E) --> ["var"], var(Var), [":="], expr(E). %problem here evaluating the expr(E). infite search i think

var(Var) --> identifier(Var).
id(Id) --> identifier(Id). 

expr(neg(E)) --> ["-"], expr(E).
expr(Num) --> integer(Num).
expr(Var) --> identifier(Var).

/*
expr(plus(E,T)) --> expr(E), ["+"], term(T).
expr(minus(E,T)) --> expr(E), ["-"], term(T).
expr(T) --> term(T).
*/
expr(eq(F1,F2)) --> factor(F1), ["="], factor(F2).
expr(diff(F1,F2)) --> factor(F1), ["<>"], factor(F2).
expr(lt(F1,F2)) --> factor(F1), ["<"], factor(F2).
expr(lteq(F1,F2)) --> factor(F1), ["<="], factor(F2).
expr(gt(F1,F2)) --> factor(F1), [">"], factor(F2).
expr(gteq(F1,F2)) --> factor(F1), [">="], factor(F2).

%expr(T) --> term(T). % here for testing 

%expr(and(E1,E2)) --> ["&"], expr(E1), expr(E2).
%expr(or(E1,E2)) --> ["|"], expr(E1), expr(E2).


expr(assign(X,E)) --> id(X), [":="], expr(E).
/*
expr(func_call(X,E)) --> id(X), ["("], expr_list(E), [")"].
expr(expr_seq(E)) --> ["("], expr_seq(E), [")"].
expr(if_then_else(E1,E2,E3)) --> ["if"], expr(E1), ["then"], expr(E2), ["else"], expr(E3).
expr(if_then(E1,E2)) --> ["if"], expr(E1), ["then"], expr(E2).
expr(while_do(E1,E2)) --> ["while"], expr(E1), ["do"], expr(E2).
expr(for_to_do(X,E1,E2,E3)) --> ["for"], id(X), [":="], expr(E1), ["to"], expr(E2), ["do"], expr(E3).
expr(break) --> ["break"].
expr(let_in_end(D,E)) --> ["let"], var_dec_list(D), ["in"], expr_seq(E), ["end"].
expr(print_int(E)) --> ["printi"], ["("], expr(E), [")"].
expr(scan_int(E)) --> ["scani"], ["("], expr(E), [")"].
%expr(F) --> factor(F).
*/

/*
term(mult(T,F)) --> term(T), ["*"], factor(F).
term(div(T,F)) --> term(T), ["/"], factor(F).
term(mod(T,F)) --> term(T), ["%"], factor(F).
term(T) --> factor(F).
*/
factor(F) --> integer(F).
factor(F) --> identifier(F).


fun_decl(function(id(T,E)))     --> ["function"], id(Id), ["("], type_fields(T), [")"], ["="], expr(E).
fun_decl(function(id(T,Tid,E))) --> ["function"], id(Id), ["("], type_fields(T), [")"], [":"], type_id(Tid), ["="], expr(E).

type_fields([T])    --> type_field(T).
type_fields([T|Ts]) --> type_field(T), [","], type_fields(Ts). 

type_field(Id:Type) --> id(Id), [":"], type_id(Type).

expr_list([E])       --> expr(E).                                    
expr_list([E|Es])    --> expr(E), [","], expr_list(Es).                     

var_dec_list([D])       --> var_decl(D).                                    
var_decl_list([D|Ds])   --> var_decl(D), [","], var_decl_list(Ds). 

type_id(int) --> ["int"].              
type_id(string) --> ["string"].

integer(X)      --> [Y], { number_string(X, Y) }.
identifier(X)   --> [Y], { atom_string(X, Y) }.