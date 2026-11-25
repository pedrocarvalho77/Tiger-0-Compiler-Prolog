:- module(codegen, [codegen/2, print_ir/1]).

% codegen(+Ast, -IR): IR = ir(MainCode, Functions), Functions = [fun(Name,Params,Code)].
% Intermediate code is a flat list of:
%   label(L)  jump(L)  cjump(A,Rel,B,LTrue,LFalse)
%   move(D,A)  unop(D,neg,A)  binop(D,Op,A,B)
%   load(D,Arr,Idx)  store(Arr,Idx,Val)  newarray(D,Size,Init)
%   call(D,F,Hops,Args)  ret(A)  printi(A)  print(A)  scani(D)
% Atoms: num(N), str(S), t(N) (temp), v(Up,X) (variable X of the frame Up
% static links above the current one; Up = 0 is the current frame).
% Main runs at level 0; a function declared in a body of level K has level K+1.
% Typecheck first: this code assumes a well-typed program.

:- dynamic fun_out/1.

codegen([let_in, Ds, Es], ir(Main, Funs)) :-
    flag(tiger_id, _, 0), retractall(fun_out(_)),
    decls(Ds, env([],[],none,0), Env, Main, Rest),
    seq(Es, Env, _, Rest, [ret(num(0))]),
    findall(F, fun_out(F), Funs).

fresh(Prefix, N) :- flag(tiger_id, N0, N0+1), format(atom(N), '~w~w', [Prefix, N0]).
temp(t(N)) :- flag(tiger_id, N, N+1).
label(L) :- fresh('L', L).

% ---- declarations; env(Vars, Funs, BreakLabel, Level)
% Vars: X-var(Level,Unique). Funs: F-fn(Label,Level).

decls(Ds, Env0, Env, C0, C) :-
    foldl(add_fun, Ds, Env0, Env1),
    decls_(Ds, Env1, Env, C0, C).

add_fun(var(_), E, E).
add_fun(func(id(F),_,_), E0, E) :- add_fun_(F, E0, E).
add_fun(func(id(F),_,_,_), E0, E) :- add_fun_(F, E0, E).
add_fun_(F, env(V,Fs,B,L), env(V,[F-fn(Label,L1)|Fs],B,L)) :-
    fresh('', N), format(atom(Label), '~w_~w', [F,N]), L1 is L+1.

decls_([], Env, Env, C, C).
decls_([var(id(X):=E)|Ds], env(V,Fs,B,L), EnvOut, C0, C) :-
    cg(E, env(V,Fs,B,L), A, C0, [move(v(0,U),A)|C1]),
    fresh('', N), format(atom(U), '~w_~w', [X,N]),
    decls_(Ds, env([X-var(L,U)|V],Fs,B,L), EnvOut, C1, C).
decls_([func(id(F),Ps,E)|Ds], Env, EnvOut, C0, C) :-
    fun(F, Ps, E, Env), decls_(Ds, Env, EnvOut, C0, C).
decls_([func(id(F),Ps,_,E)|Ds], Env, EnvOut, C0, C) :-
    fun(F, Ps, E, Env), decls_(Ds, Env, EnvOut, C0, C).

fun(F, Ps, E, env(V,Fs,_,_)) :-
    memberchk(F-fn(Label,L1), Fs),
    findall(N-var(L1,U), (member(id(N):_, Ps), fresh('', I), format(atom(U), '~w_~w', [N,I])), Bind),
    findall(U, member(_-var(_,U), Bind), Params),
    append(Bind, V, V1),
    seq([E], env(V1,Fs,none,L1), A, Code, [ret(A)]),
    assertz(fun_out(fun(Label, Params, Code))).

% ---- expressions: cg(+Expr, +Env, -ResultAtom, -Code, ?Tail)
% Void expressions yield num(0).

seq([E], Env, A) --> !, cg(E, Env, A).
seq([E|Es], Env, A) --> cg(E, Env, _), seq(Es, Env, A).

var_atom(X, env(V,_,_,L), v(Up,U)) :- memberchk(X-var(Ld,U), V), Up is L-Ld.

cg(num(N), _, num(N)) --> [].
cg(string(S), _, str(S)) --> [].
cg(id(X), Env, A) -->
    { var_atom(X, Env, Var) },
    % copy variables of outer frames: a later call could change them
    ( { Var = v(Up,_), Up > 0 } -> { temp(A) }, [move(A,Var)] ; { A = Var } ).
cg(neg(E), Env, T) --> cg(E, Env, A), { temp(T) }, [unop(T,neg,A)].
cg(E, Env, T) --> { E =.. [Op,X,Y], memberchk(Op, [plus,minus,mult,div,mod]) }, !,
    cg(X, Env, A), cg(Y, Env, B), { temp(T) }, [binop(T,Op,A,B)].
cg(E, Env, T) --> { is_bool(E) }, !,
    { temp(T), label(Lt), label(Lf), label(Le) },
    cond(E, Env, Lt, Lf),
    [label(Lt), move(T,num(1)), jump(Le), label(Lf), move(T,num(0)), label(Le)].
cg(index(Arr,I), Env, T) --> cg(Arr, Env, A), cg(I, Env, B), { temp(T) }, [load(T,A,B)].
cg(assign(id(X),E), Env, num(0)) -->
    { var_atom(X, Env, Var) }, cg(E, Env, A), [move(Var,A)].
cg(assign(index(Arr,I),E), Env, num(0)) -->
    cg(Arr, Env, A), cg(I, Env, B), cg(E, Env, C), [store(A,B,C)].
cg(if_then(C,Th), Env, num(0)) -->
    { label(Lt), label(Lf) },
    cond(C, Env, Lt, Lf), [label(Lt)], cg(Th, Env, _), [label(Lf)].
cg(if_then_else(C,Th,El), Env, T) -->
    { temp(T), label(Lt), label(Lf), label(Le) },
    cond(C, Env, Lt, Lf),
    [label(Lt)], cg(Th, Env, A), [move(T,A), jump(Le), label(Lf)],
    cg(El, Env, B), [move(T,B), label(Le)].
cg(while_do(C,Body), env(V,Fs,_,L), num(0)) -->
    { label(Ls), label(Lb), label(Le), Env = env(V,Fs,Le,L) },
    [label(Ls)], cond(C, Env, Lb, Le),
    [label(Lb)], cg(Body, Env, _), [jump(Ls), label(Le)].
cg(for_to_do(id(X),E1,E2,Body), env(V,Fs,B,L), num(0)) -->
    { fresh('', I), format(atom(U), '~w_~w', [X,I]), Var = v(0,U),
      temp(Hi), label(Ls), label(Lb), label(Le) },
    cg(E1, env(V,Fs,B,L), A), [move(Var,A)],
    cg(E2, env(V,Fs,B,L), H), [move(Hi,H)],
    [label(Ls), cjump(Var,lteq,Hi,Lb,Le), label(Lb)],
    cg(Body, env([X-var(L,U)|V],Fs,Le,L), _),
    [binop(Var,plus,Var,num(1)), jump(Ls), label(Le)].
cg(break, env(_,_,B,_), num(0)) --> [jump(B)].
cg(expr_seq(Es), Env, A) --> seq(Es, Env, A).
cg(let_in_end(Ds,Es), Env0, A, C0, C) :-
    decls(Ds, Env0, Env1, C0, C1),
    seq(Es, Env1, A, C1, C).
cg(print_int(E), Env, num(0)) --> cg(E, Env, A), [printi(A)].
cg(print_str(E), Env, num(0)) --> cg(E, Env, A), [print(A)].
cg(scan_int, _, T) --> { temp(T) }, [scani(T)].
cg(int_array(N,I), Env, T) --> cg(N, Env, A), cg(I, Env, B), { temp(T) }, [newarray(T,A,B)].
cg(func_call(id(F),Args), Env, T) -->
    { Env = env(_,Fs,_,L), memberchk(F-fn(Label,Lf), Fs), Hops is L-Lf+1 },
    args(Args, Env, As), { temp(T) }, [call(T,Label,Hops,As)].

args([], _, []) --> [].
args([E|Es], Env, [A|As]) --> cg(E, Env, A), args(Es, Env, As).

% ---- conditions: cond(+Expr, +Env, +LTrue, +LFalse) jumps to one of the labels

is_bool(E) :- E =.. [Op,_,_], memberchk(Op, [and,or,eq,diff,lt,lteq,gt,gteq]).

cond(and(X,Y), Env, Lt, Lf) --> !,
    { label(L1) }, cond(X, Env, L1, Lf), [label(L1)], cond(Y, Env, Lt, Lf).
cond(or(X,Y), Env, Lt, Lf) --> !,
    { label(L1) }, cond(X, Env, Lt, L1), [label(L1)], cond(Y, Env, Lt, Lf).
cond(E, Env, Lt, Lf) --> { E =.. [Op,X,Y], rel(Op) }, !,
    cg(X, Env, A), cg(Y, Env, B), [cjump(A,Op,B,Lt,Lf)].
cond(E, Env, Lt, Lf) --> cg(E, Env, A), [cjump(A,diff,num(0),Lt,Lf)].

rel(eq). rel(diff). rel(lt). rel(lteq). rel(gt). rel(gteq).

% ---- printing

print_ir(ir(Main, Funs)) :-
    format("main:~n"), maplist(print_instr, Main),
    forall(member(fun(F,Ps,Code), Funs),
           ( format("~nfunction ~w(~w):~n", [F, Ps]), maplist(print_instr, Code) )).

print_instr(label(L)) :- !, format("~w:~n", [L]).
print_instr(I) :- format("    ~q~n", [I]).
