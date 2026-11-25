:- module(interp, [run_ir/1]).

% run_ir(+IR): executes the intermediate code produced by codegen/2.
% Used to check the compiler end to end (there is no native back end yet).
% Each call gets a frame (a global hashtable holds its variables, flink/2 its
% static link, the frame of the enclosing function); arrays live on a heap (arr(Id)).

:- use_module(library(hashtable)).

:- dynamic fun_def/4, flink/2, cell/3, arrsize/2.

run_ir(ir(Main, Funs)) :-
    retractall(fun_def(_,_,_,_)), retractall(flink(_,_)),
    retractall(cell(_,_,_)), retractall(arrsize(_,_)),
    forall(member(fun(F,Ps,C), Funs),
           ( labels(C, t, Ls), assertz(fun_def(F,Ps,C,Ls)) )),
    ht_new(HT), nb_setval(tiger_vars, HT),
    new_frame(none, Fid),
    labels(Main, t, MainLs), exec(Main, MainLs, Fid, _).

new_frame(Link, Fid) :- flag(tiger_frame, Fid, Fid+1), assertz(flink(Fid, Link)).

labels([], L, L).
labels([label(N)|Rest], L0, L) :- !,
    ( L0 == t -> empty_assoc(L1) ; L1 = L0 ),
    put_assoc(N, L1, Rest, L2), labels(Rest, L2, L).
labels([_|Rest], L0, L) :- labels(Rest, L0, L).

% exec(+Code, +Labels, +Frame, -Return)
exec([], _, _, 0).
exec([I|Is], Ls, F, Ret) :-
    step(I, Ls, F, Next, Ret0),
    (   Next == fall -> exec(Is, Ls, F, Ret)
    ;   Next == return -> Ret = Ret0
    ;   exec(Next, Ls, F, Ret) ).

% step(+Instr, +Labels, +Frame, -Next, -Ret): Next = fall | return | Code
step(label(_), _, _, fall, _).
step(jump(L), Ls, _, Code, _) :- get_assoc(L, Ls, Code).
step(cjump(A,Rel,B,Lt,Lf), Ls, F, Code, _) :-
    val(A, F, X), val(B, F, Y),
    ( rel(Rel, X, Y) -> L = Lt ; L = Lf ),
    get_assoc(L, Ls, Code).
step(move(D,A), _, F, fall, _) :- val(A, F, X), set(D, X, F).
step(unop(D,neg,A), _, F, fall, _) :- val(A, F, X), Y is -X, set(D, Y, F).
step(binop(D,Op,A,B), _, F, fall, _) :-
    val(A, F, X), val(B, F, Y), arith(Op, X, Y, Z), set(D, Z, F).
step(load(D,A,I), _, F, fall, _) :-
    val(A, F, arr(Id)), val(I, F, K), check_index(Id, K),
    cell(Id, K, V), set(D, V, F).
step(store(A,I,V), _, F, fall, _) :-
    val(A, F, arr(Id)), val(I, F, K), val(V, F, X), check_index(Id, K),
    retract(cell(Id, K, _)), assertz(cell(Id, K, X)).
step(newarray(D,N,I), _, F, fall, _) :-
    val(N, F, Size), val(I, F, Init),
    flag(tiger_arr, Id, Id+1), assertz(arrsize(Id, Size)),
    Max is Size-1, forall(between(0, Max, K), assertz(cell(Id, K, Init))),
    set(D, arr(Id), F).
step(call(D,Fn,Hops,Args), _, F, fall, _) :-
    maplist([A,V]>>val(A, F, V), Args, Vs),
    fun_def(Fn, Ps, Code, Labels),
    hop(Hops, F, Link),
    new_frame(Link, Nf),
    maplist([P,V]>>store_var(Nf, P, V), Ps, Vs),
    exec(Code, Labels, Nf, R),
    retractall(flink(Nf,_)),
    set(D, R, F).
step(ret(A), _, F, return, R) :- val(A, F, R).
step(printi(A), _, F, fall, _) :- val(A, F, X), write(X).
step(print(A), _, F, fall, _) :- val(A, F, str(S)), atom_chars(S, Cs), emit(Cs).
step(scani(D), _, F, fall, _) :-
    read_line_to_string(user_input, L), number_string(N, L), set(D, N, F).

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

% hop(+N, +Frame, -Frame2): follow N static links
hop(0, F, F) :- !.
hop(N, F, G) :- flink(F, L), N1 is N-1, hop(N1, L, G).

val(num(N), _, N).
val(str(S), _, str(S)).
val(t(N), F, V) :- load_var(F, t(N), V).
val(v(Up,X), F, V) :- hop(Up, F, G), load_var(G, X, V).

set(t(N), V, F) :- store_var(F, t(N), V).
set(v(Up,X), V, F) :- hop(Up, F, G), store_var(G, X, V).

store_var(F, K, V) :- nb_getval(tiger_vars, HT), ht_put(HT, F-K, V).
load_var(F, K, V) :- nb_getval(tiger_vars, HT), ht_get(HT, F-K, V).

% string escapes: \n \t \" \\ ; anything else is printed as written
emit([]).
emit(['\\', n|T]) :- !, nl, emit(T).
emit(['\\', t|T]) :- !, put_char('\t'), emit(T).
emit(['\\', C|T]) :- !, put_char(C), emit(T).
emit([C|T]) :- put_char(C), emit(T).

