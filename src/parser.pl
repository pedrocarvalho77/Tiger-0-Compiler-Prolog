:- module(parser, [parse/2]).
:- use_module(utils).

% Grammar (lowest to highest precedence):
%   | ; & ; comparison (= <> < <= > >=) ; + - ; * / % ; unary - ; primary
% Binary operators are left-associative. Tokens come from utils:convert/2:
% keywords/symbols are strings, numbers are numbers, identifiers are atoms,
% string literals are str(Atom).

parse(AtomTokens, Ast):-
    convert(AtomTokens, StringTokens),
    once(phrase(program(Ast), StringTokens)).

program([let_in, D, E]) --> ["let"], decl_list(D), ["in"], expr_seq(E).

decl_list([D])      --> decl(D).
decl_list([D|Ds])   --> decl(D), decl_list(Ds).

decl(D) --> var_decl(D).
decl(D) --> fun_decl(D).

var_decl(var(Var:=E)) --> ["var"], var(Var), [":="], expr(E).

fun_decl(func(Id,Ts,T,E))   --> ["function"], id(Id), ["("], type_fields(Ts), [")"], [":"], type_id(T), ["="], expr(E).
fun_decl(func(Id,Ts,E))     --> ["function"], id(Id), ["("], type_fields(Ts), [")"], ["="], expr(E).

type_fields([])     --> [].
type_fields([T])    --> type_field(T).
type_fields([T|Ts]) --> type_field(T), [","], type_fields(Ts).

type_field(Id:Type) --> id(Id), [":"], type_id(Type).

% expr: assignments and control structures extend as far right as possible
expr(assign(L,E)) --> lvalue(L), [":="], expr(E).
expr(if_then_else(E1,E2,E3)) --> ["if"], expr(E1), ["then"], expr(E2), ["else"], expr(E3).
expr(if_then(E1,E2)) --> ["if"], expr(E1), ["then"], expr(E2).
expr(while_do(E1,E2)) --> ["while"], expr(E1), ["do"], expr(E2).
expr(for_to_do(X,E1,E2,E3)) --> ["for"], id(X), [":="], expr(E1), ["to"], expr(E2), ["do"], expr(E3).
expr(E) --> or_expr(E).

or_expr(E)  --> and_expr(L), or_rest(L, E).
or_rest(Acc, E) --> ["|"], and_expr(R), or_rest(or(Acc,R), E).
or_rest(E, E) --> [].

and_expr(E) --> cmp_expr(L), and_rest(L, E).
and_rest(Acc, E) --> ["&"], cmp_expr(R), and_rest(and(Acc,R), E).
and_rest(E, E) --> [].

cmp_expr(E) --> add_expr(L), cmp_rest(L, E).
cmp_rest(L, eq(L,R))   --> ["="],  add_expr(R).
cmp_rest(L, diff(L,R)) --> ["<>"], add_expr(R).
cmp_rest(L, lteq(L,R)) --> ["<="], add_expr(R).
cmp_rest(L, gteq(L,R)) --> [">="], add_expr(R).
cmp_rest(L, lt(L,R))   --> ["<"],  add_expr(R).
cmp_rest(L, gt(L,R))   --> [">"],  add_expr(R).
cmp_rest(E, E) --> [].

add_expr(E) --> term(L), add_rest(L, E).
add_rest(Acc, E) --> ["+"], term(R), add_rest(plus(Acc,R), E).
add_rest(Acc, E) --> ["-"], term(R), add_rest(minus(Acc,R), E).
add_rest(E, E) --> [].

term(E) --> unary(L), term_rest(L, E).
term_rest(Acc, E) --> ["*"], unary(R), term_rest(mult(Acc,R), E).
term_rest(Acc, E) --> ["/"], unary(R), term_rest(div(Acc,R), E).
term_rest(Acc, E) --> ["%"], unary(R), term_rest(mod(Acc,R), E).
term_rest(E, E) --> [].

unary(neg(E)) --> ["-"], unary(E).
unary(E) --> primary(E).

primary(num(N)) --> [N], { number(N) }.
primary(string(S)) --> [str(S)].
primary(print_int(E)) --> ["printi"], ["("], expr(E), [")"].
primary(print_str(E)) --> ["print"], ["("], expr(E), [")"].
primary(scan_int) --> ["scani"], ["("], [")"].
primary(break) --> ["break"].
primary(expr_seq(E)) --> ["("], expr_seq(E), [")"].
primary(let_in_end(D,E)) --> ["let"], decl_list(D), ["in"], expr_seq(E), ["end"].
primary(int_array(E1,E2)) --> ["intArray"], ["["], expr(E1), ["]"], ["of"], primary(E2).
primary(func_call(X,E)) --> id(X), ["("], expr_list(E), [")"].
primary(L) --> lvalue(L).

lvalue(L) --> id(X), lvalue_rest(X, L).
lvalue_rest(Acc, L) --> ["["], expr(E), ["]"], lvalue_rest(index(Acc,E), L).
lvalue_rest(L, L) --> [].

var(Var) --> identifier(Var).
id(Id) --> identifier(Id).

expr_seq([E])       --> expr(E).
expr_seq([E|Es])    --> expr(E), [";"], expr_seq(Es).

expr_list([])        --> [].
expr_list([E])       --> expr(E).
expr_list([E|Es])    --> expr(E), [","], expr_list(Es).

type_id(type_int) --> ["int"].
type_id(type_string) --> ["string"].
type_id(type_intArray) --> ["intArray"].

identifier(id(Y))   --> [Y], { atom(Y) }.
