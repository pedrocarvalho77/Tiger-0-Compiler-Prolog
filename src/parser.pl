:- module(parser, [parse/2]).
:- use_module(utils).


parse(AtomTokens, Ast):-
    convert(AtomTokens, StringTokens),
    phrase(program(Ast), StringTokens).

program([let_in, D, E]) --> ["let"], decl_list(D), ["in"], expr_seq(E), !.

decl_list([D])      --> decl(D).
decl_list([D|Ds])   --> decl(D), decl_list(Ds).

decl(D) --> var_decl(D).
decl(D) --> fun_decl(D).

var_decl(var(Var:=E)) --> ["var"], var(Var), [":="], expr(E).

fun_decl(func(Id,Ts,E))     --> ["function"], id(Id), ["("], type_fields(Ts), [")"], ["="], expr(E).
fun_decl(func(Id,Ts,T,E))   --> ["function"], id(Id), ["("], type_fields(Ts), [")"], [":"], type_id(T), ["="], expr(E).

type_fields([T])    --> type_field(T).
type_fields([T|Ts]) --> type_field(T), [","], type_fields(Ts). 

type_field(Id:Type) --> id(Id), [":"], type_id(Type).

expr(neg(E)) --> ["-"], expr(E).

expr(plus(T,E)) --> term(T), ["+"], expr(E).
expr(minus(T,E)) --> term(T), ["-"], expr(E).

expr(eq(F,E)) --> factor(F), ["="], expr(E).
expr(diff(F,E)) --> factor(F), ["<>"], expr(E).
expr(lt(F,E)) --> factor(F), ["<"], expr(E).
expr(lteq(F,E)) --> factor(F), ["<="], expr(E).
expr(gt(F,E)) --> factor(F), [">"], expr(E).
expr(gteq(F,E)) --> factor(F), [">="], expr(E).

expr(and(T,E)) --> term(T), ["&"], expr(E).
expr(or(T,E)) --> term(T), ["|"], expr(E).

expr(print_int(E)) --> ["printi"], ["("], expr(E), [")"].
expr(assign(X,E)) --> id(X), [":="], expr(E).
expr(func_call(X,E)) --> id(X), ["("], expr_list(E), [")"].
expr(expr_seq(E)) --> ["("], expr_seq(E), [")"].
expr(if_then_else(E1,E2,E3)) --> ["if"], expr(E1), ["then"], expr(E2), ["else"], expr(E3).
expr(if_then(E1,E2)) --> ["if"], expr(E1), ["then"], expr(E2).
expr(while_do(E1,E2)) --> ["while"], expr(E1), ["do"], expr(E2).
expr(for_to_do(X,E1,E2,E3)) --> ["for"], id(X), [":="], expr(E1), ["to"], expr(E2), ["do"], expr(E3).
expr(break) --> ["break"].
expr(let_in_end(D,E)) --> ["let"], var_dec_list(D), ["in"], expr_seq(E), ["end"].
expr(scan_int) --> ["scani"], ["("], [")"].
expr(T) --> term(T).

term(mult(F,E)) --> factor(F), ["*"], expr(E).
term(div(F,E)) --> factor(F), ["/"], expr(E).
term(mod(F,E)) --> factor(F), ["%"], expr(E).
term(F) --> factor(F).

factor(F) --> integer(F).

var(Var) --> identifier(Var).
id(Id) --> identifier(Id). 

expr_seq([E])       --> expr(E).
expr_seq([E|Es])    --> expr(E), [";"], expr_seq(Es). 

expr_list([E])       --> expr(E).                                    
expr_list([E|Es])    --> expr(E), [","], expr_list(Es).                     

var_dec_list([D])       --> var_decl(D).                                    
var_decl_list([D|Ds])   --> var_decl(D), var_decl_list(Ds). 

type_id(type_int) --> ["int"].              
type_id(type_string) --> ["string"].
type_id(tok_type_intArray) --> ["intArray"].

integer(num(X))     --> [Y], { atom_string(X, Y) }.
identifier(id(X))   --> [Y], { atom_string(X, Y) }.