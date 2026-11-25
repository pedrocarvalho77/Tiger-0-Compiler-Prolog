:- module(codegen, [codegen/2, print_ir/1]).

% codegen(+Ast, -IR): IR = ir(MainCode, Functions), Functions = [fun(Name,Params,Code)].
% Intermediate code is a flat list of:
%   label(L)  jump(L)  cjump(A,Rel,B,LTrue,LFalse)
%   move(D,A)  unop(D,neg,A)  binop(D,Op,A,B)
%   load(D,Arr,Idx)  store(Arr,Idx,Val)  newarray(D,Size,Init)
%   call(D,F,Args)  ret(A)  printi(A)  print(A)  scani(D)
% Atoms: num(N), str(S), v(X) (local), t(N) (temp), g(X) (global).
% Typecheck first: this code assumes a well-typed program.

codegen([let_in, Ds, Es], ir(Main, Funs)) :-
    flag(tiger_id, _, 0),
    decls(Ds, top, env([], none), env(V,_), Main0, Main1, Funs),
    seq(Es, env(V,none), _, Main1, [ret(num(0))]),
    Main = Main0.

fresh(Prefix, N) :- flag(tiger_id, N0, N0+1), format(atom(N), '~w~w', [Prefix, N0]).
temp(t(N)) :- flag(tiger_id, N, N+1).
label(L) :- fresh('L', L).

% ---- declarations (Main code goes into the difference list C0-C)

decls([], _, Env, Env, C, C, []).
decls([var(id(X):=E)|Ds], Where, env(V,B), EnvOut, C0, C, Funs) :-
    cg(E, env(V,B), A, C0, [move(Var,A)|C1]),
    fresh('', N), format(atom(U), '~w_~w', [X,N]),
    ( Where == top -> Var = g(U) ; Var = v(U) ),
    decls(Ds, Where, env([X-Var|V],B), EnvOut, C1, C, Funs).
decls([func(id(F),Ps,E)|Ds], Where, Env, EnvOut, C0, C, [Fun|Funs]) :- !,
    fun(F, Ps, E, Env, Fun),
    decls(Ds, Where, Env, EnvOut, C0, C, Funs).
decls([func(id(F),Ps,_,E)|Ds], Where, Env, EnvOut, C0, C, [Fun|Funs]) :-
    fun(F, Ps, E, Env, Fun),
    decls(Ds, Where, Env, EnvOut, C0, C, Funs).

fun(F, Ps, E, env(V,_), fun(F, Params, Code)) :-
    findall(N-v(U), (member(id(N):_, Ps), fresh('', I), format(atom(U), '~w_~w', [N,I])), Bind),
    findall(P, member(_-P, Bind), Params),
    append(Bind, V, V1),
    seq([E], env(V1,none), A, Code, [ret(A)]).

% ---- expressions: cg(+Expr, +Env, -ResultAtom, -Code, ?Tail)
% Env = env(Vars, BreakLabel). Void expressions yield num(0).

seq([E], Env, A) --> !, cg(E, Env, A).
seq([E|Es], Env, A) --> cg(E, Env, _), seq(Es, Env, A).

cg(num(N), _, num(N)) --> [].
cg(string(S), _, str(S)) --> [].
cg(id(X), env(V,_), A) -->
    { memberchk(X-Var, V) },
    % copy globals: a later call could change them before the value is used
    ( { Var = g(_) } -> { temp(A) }, [move(A,Var)] ; { A = Var } ).
cg(neg(E), Env, T) --> cg(E, Env, A), { temp(T) }, [unop(T,neg,A)].
cg(E, Env, T) --> { E =.. [Op,X,Y], memberchk(Op, [plus,minus,mult,div,mod]) }, !,
    cg(X, Env, A), cg(Y, Env, B), { temp(T) }, [binop(T,Op,A,B)].
cg(E, Env, T) --> { is_bool(E) }, !,
    { temp(T), label(Lt), label(Lf), label(Le) },
    cond(E, Env, Lt, Lf),
    [label(Lt), move(T,num(1)), jump(Le), label(Lf), move(T,num(0)), label(Le)].
cg(index(Arr,I), Env, T) --> cg(Arr, Env, A), cg(I, Env, B), { temp(T) }, [load(T,A,B)].
cg(assign(id(X),E), env(V,B), num(0)) -->
    { memberchk(X-Var, V) }, cg(E, env(V,B), A), [move(Var,A)].
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
cg(while_do(C,Body), env(V,_), num(0)) -->
    { label(Ls), label(Lb), label(Le) },
    [label(Ls)], cond(C, env(V,Le), Lb, Le),
    [label(Lb)], cg(Body, env(V,Le), _), [jump(Ls), label(Le)].
cg(for_to_do(id(X),E1,E2,Body), env(V,B), num(0)) -->
    { fresh('', I), format(atom(U), '~w_~w', [X,I]), Var = v(U),
      temp(Hi), label(Ls), label(Lb), label(Le) },
    cg(E1, env(V,B), A), [move(Var,A)],
    cg(E2, env(V,B), H), [move(Hi,H)],
    [label(Ls), cjump(Var,lteq,Hi,Lb,Le), label(Lb)],
    cg(Body, env([X-Var|V],Le), _),
    [binop(Var,plus,Var,num(1)), jump(Ls), label(Le)].
cg(break, env(_,B), num(0)) --> [jump(B)].
cg(expr_seq(Es), Env, A) --> seq(Es, Env, A).
cg(let_in_end(Ds,Es), env(V,B), A, C0, C) :-
    decls(Ds, local, env(V,B), Env1, C0, C1, _),
    seq(Es, Env1, A, C1, C).
cg(print_int(E), Env, num(0)) --> cg(E, Env, A), [printi(A)].
cg(print_str(E), Env, num(0)) --> cg(E, Env, A), [print(A)].
cg(scan_int, _, T) --> { temp(T) }, [scani(T)].
cg(int_array(N,I), Env, T) --> cg(N, Env, A), cg(I, Env, B), { temp(T) }, [newarray(T,A,B)].
cg(func_call(id(F),Args), Env, T) -->
    args(Args, Env, As), { temp(T) }, [call(T,F,As)].

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
