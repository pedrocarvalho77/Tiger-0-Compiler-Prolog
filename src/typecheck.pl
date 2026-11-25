:- module(typecheck, [typecheck/1]).

% typecheck(+Ast): succeeds if the program is well typed, else throws
% error(tiger(Message), _).
% Types: int, string, intArray, void.
% A function sees the variables declared before it (including those of the
% enclosing functions) and all functions of its declaration list, so
% recursion and mutual recursion work. The variable of a for loop is read-only.

typecheck([let_in, Ds, Es]) :-
    Env0 = env([], [], false),
    decls(Ds, Env0, Env),
    seq_type(Es, Env, _).

terr(Fmt, Args) :-
    format(atom(Msg), Fmt, Args),
    throw(error(tiger(Msg), _)).

% ---- declarations

decls(Ds, Env0, Env) :-
    foldl(add_sig, Ds, Env0, Env1),
    foldl(decl, Ds, Env1, Env).

add_sig(var(_), E, E) :- !.
add_sig(func(id(F), Ps, T, _), env(V,Fs,L), env(V,[F-sig(PTs,RT)|Fs],L)) :- !,
    params_types(Ps, PTs), ret_type(T, RT).
add_sig(func(id(F), Ps, _), env(V,Fs,L), env(V,[F-sig(PTs,void)|Fs],L)) :-
    params_types(Ps, PTs).

params_types(Ps, Ts) :- findall(T, (member(_:T0, Ps), ret_type(T0, T)), Ts).

ret_type(type_int, int).
ret_type(type_string, string).
ret_type(type_intArray, intArray).

decl(var(id(X):=E), env(V,Fs,L), env([X-T|V],Fs,L)) :-
    ty(E, env(V,Fs,L), T),
    ( T == void -> terr("variable ~w has no value (void expression)", [X]) ; true ).
decl(func(id(F), Ps, E), Env, Env) :- !, func_decl(F, Ps, void, E, Env).
decl(func(id(F), Ps, T, E), Env, Env) :-
    ret_type(T, RT), func_decl(F, Ps, RT, E, Env).

func_decl(F, Ps, RT, E, env(V,Fs,_)) :-
    findall(N-T, (member(id(N):T0, Ps), ret_type(T0, T)), PVars),
    append(PVars, V, V1),
    ty(E, env(V1,Fs,false), BT),
    ( BT == RT -> true
    ; terr("function ~w: body has type ~w, expected ~w", [F, BT, RT]) ).

% ---- expressions

seq_type([E], Env, T) :- !, ty(E, Env, T).
seq_type([E|Es], Env, T) :- ty(E, Env, _), seq_type(Es, Env, T).

expect(E, Env, T) :-
    ty(E, Env, T0),
    ( T0 == T -> true ; terr("expected ~w but found ~w in ~q", [T, T0, E]) ).

ty(num(_), _, int).
ty(string(_), _, string).
ty(neg(E), Env, int) :- expect(E, Env, int).
ty(E, Env, int) :- E =.. [Op,A,B], memberchk(Op, [plus,minus,mult,div,mod,and,or]), !,
    expect(A, Env, int), expect(B, Env, int).
ty(E, Env, int) :- E =.. [Op,A,B], memberchk(Op, [lt,lteq,gt,gteq]), !,
    expect(A, Env, int), expect(B, Env, int).
ty(E, Env, int) :- E =.. [Op,A,B], memberchk(Op, [eq,diff]), !,
    ty(A, Env, T),
    ( memberchk(T, [int,string]) -> true ; terr("cannot compare values of type ~w", [T]) ),
    expect(B, Env, T).
ty(id(X), env(V,_,_), T) :-
    ( memberchk(X-T0, V) -> T = T0 ; terr("undefined variable ~w", [X]) ).
ty(index(A,I), Env, int) :- expect(A, Env, intArray), expect(I, Env, int).
ty(assign(L,E), Env, void) :-
    ( L = id(X) -> check_writable(X, Env) ; true ),
    ty(L, Env, T), expect(E, Env, T).
ty(if_then(C,T), Env, void) :- expect(C, Env, int), expect(T, Env, void).
ty(if_then_else(C,A,B), Env, T) :-
    expect(C, Env, int), ty(A, Env, T), expect(B, Env, T).
ty(while_do(C,B), env(V,F,_), void) :-
    expect(C, env(V,F,true), int), expect(B, env(V,F,true), void).
ty(for_to_do(id(X),A,B,Body), env(V,F,L), void) :-
    expect(A, env(V,F,L), int), expect(B, env(V,F,L), int),
    expect(Body, env([ro(X),X-int|V],F,true), void).
ty(break, env(_,_,L), void) :-
    ( L == true -> true ; terr("break outside a loop", []) ).
ty(expr_seq(Es), Env, T) :- seq_type(Es, Env, T).
ty(let_in_end(Ds,Es), Env0, T) :-
    decls(Ds, Env0, Env), seq_type(Es, Env, T).
ty(print_int(E), Env, void) :- expect(E, Env, int).
ty(print_str(E), Env, void) :- expect(E, Env, string).
ty(scan_int, _, int).
ty(int_array(N,I), Env, intArray) :- expect(N, Env, int), expect(I, Env, int).
ty(func_call(id(F),Args), env(V,Fs,L), RT) :-
    ( memberchk(F-sig(PTs,RT0), Fs) -> RT = RT0 ; terr("undefined function ~w", [F]) ),
    length(Args, N), length(PTs, M),
    ( N == M -> true ; terr("function ~w expects ~w arguments, got ~w", [F, M, N]) ),
    maplist([A,T]>>expect(A, env(V,Fs,L), T), Args, PTs).

% the innermost declaration of X decides: ro(X) marks a for-loop variable
check_writable(X, env(V,_,_)) :-
    (   member(Entry, V), ( Entry = ro(X) ; Entry = X-_ ) -> true ; Entry = none ),
    (   Entry = ro(_) -> terr("cannot assign to the loop variable ~w", [X]) ; true ).
