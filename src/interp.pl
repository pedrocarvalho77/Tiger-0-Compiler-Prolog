:- module(interp, [run_ir/1]).

% run_ir(+IR): executes the intermediate code produced by codegen/2.
% Used to check the compiler end to end (there is no native back end yet).
% Arrays live on a heap (arr(Id)), globals in gvar/2, locals in an assoc frame.

:- dynamic fun_def/3, gvar/2, cell/3, arrsize/2.

run_ir(ir(Main, Funs)) :-
    retractall(fun_def(_,_,_)), retractall(gvar(_,_)),
    retractall(cell(_,_,_)), retractall(arrsize(_,_)),
    forall(member(fun(F,Ps,C), Funs), assertz(fun_def(F,Ps,C))),
    run(Main, _).

run(Code, Ret) :-
    labels(Code, Code, t, Labels),
    empty_assoc(F0),
    exec(Code, Labels, F0, Ret).

labels([], _, L, L).
labels([label(N)|Rest], _, L0, L) :- !,
    ( L0 == t -> empty_assoc(L1) ; L1 = L0 ),
    put_assoc(N, L1, Rest, L2), labels(Rest, [], L2, L).
labels([_|Rest], _, L0, L) :- labels(Rest, [], L0, L).

% exec(+Code, +Labels, +Frame, -Return)
exec([], _, _, 0).
exec([I|Is], Ls, F0, Ret) :-
    step(I, Ls, F0, F, Next, Ret0),
    (   Next == fall -> exec(Is, Ls, F, Ret)
    ;   Next == return -> Ret = Ret0
    ;   exec(Next, Ls, F, Ret) ).

% step(+Instr, +Labels, +F0, -F, -Next, -Ret): Next = fall | return | Code
step(label(_), _, F, F, fall, _).
step(jump(L), Ls, F, F, Code, _) :- get_assoc(L, Ls, Code).
step(cjump(A,Rel,B,Lt,Lf), Ls, F, F, Code, _) :-
    val(A, F, X), val(B, F, Y),
    ( rel(Rel, X, Y) -> L = Lt ; L = Lf ),
    get_assoc(L, Ls, Code).
step(move(D,A), _, F0, F, fall, _) :- val(A, F0, X), set(D, X, F0, F).
step(unop(D,neg,A), _, F0, F, fall, _) :- val(A, F0, X), Y is -X, set(D, Y, F0, F).
step(binop(D,Op,A,B), _, F0, F, fall, _) :-
    val(A, F0, X), val(B, F0, Y), arith(Op, X, Y, Z), set(D, Z, F0, F).
step(load(D,A,I), _, F0, F, fall, _) :-
    val(A, F0, arr(Id)), val(I, F0, K), check_index(Id, K),
    cell(Id, K, V), set(D, V, F0, F).
step(store(A,I,V), _, F, F, fall, _) :-
    val(A, F, arr(Id)), val(I, F, K), val(V, F, X), check_index(Id, K),
    retract(cell(Id, K, _)), assertz(cell(Id, K, X)).
step(newarray(D,N,I), _, F0, F, fall, _) :-
    val(N, F0, Size), val(I, F0, Init),
    flag(tiger_arr, Id, Id+1), assertz(arrsize(Id, Size)),
    Max is Size-1, forall(between(0, Max, K), assertz(cell(Id, K, Init))),
    set(D, arr(Id), F0, F).
step(call(D,Fn,Args), _, F0, F, fall, _) :-
    maplist([A,V]>>val(A, F0, V), Args, Vs),
    fun_def(Fn, Ps, Code),
    empty_assoc(E0),
    foldl([P,V,E1,E2]>>put_assoc(P, E1, V, E2), Ps, Vs, E0, Frame),
    labels(Code, Code, t, Labels),
    exec(Code, Labels, Frame, R),
    set(D, R, F0, F).
step(ret(A), _, F, F, return, R) :- val(A, F, R).
step(printi(A), _, F, F, fall, _) :- val(A, F, X), write(X).
step(print(A), _, F, F, fall, _) :- val(A, F, str(S)), atom_chars(S, Cs), emit(Cs).
step(scani(D), _, F0, F, fall, _) :-
    read_line_to_string(user_input, L), number_string(N, L), set(D, N, F0, F).

rel(eq, X, Y) :- X == Y.
rel(diff, X, Y) :- X \== Y.
rel(lt, X, Y) :- X < Y.
rel(lteq, X, Y) :- X =< Y.
rel(gt, X, Y) :- X > Y.
rel(gteq, X, Y) :- X >= Y.

arith(plus, X, Y, Z) :- Z is X+Y.
arith(minus, X, Y, Z) :- Z is X-Y.
arith(mult, X, Y, Z) :- Z is X*Y.
arith(div, X, Y, Z) :- Z is X // Y.
arith(mod, X, Y, Z) :- Z is X rem Y.

check_index(Id, K) :-
    arrsize(Id, N),
    ( K >= 0, K < N -> true
    ; throw(error(tiger(runtime(index_out_of_bounds(K, N))), _)) ).

val(num(N), _, N).
val(str(S), _, str(S)).
val(g(X), _, V) :- gvar(g(X), V).
val(v(X), F, V) :- get_assoc(v(X), F, V).
val(t(X), F, V) :- get_assoc(t(X), F, V).

set(g(X), V, F, F) :- retractall(gvar(g(X), _)), assertz(gvar(g(X), V)).
set(v(X), V, F0, F) :- put_assoc(v(X), F0, V, F).
set(t(X), V, F0, F) :- put_assoc(t(X), F0, V, F).

% string escapes: \n \t \" \\ ; anything else is printed as written
emit([]).
emit(['\\', n|T]) :- !, nl, emit(T).
emit(['\\', t|T]) :- !, put_char('\t'), emit(T).
emit(['\\', C|T]) :- !, put_char(C), emit(T).
emit([C|T]) :- put_char(C), emit(T).
