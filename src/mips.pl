:- module(mips, [mips_gen/1]).

% mips_gen(+IR): prints MIPS32 assembly (MARS/SPIM syntax, syscall-based I/O)
% for the intermediate code of codegen/2.
%
% Frame layout, $fp = lowest address of the frame:
%    0($fp) saved $ra   4($fp) static link   8($fp) caller's $fp
%   12+4*i($fp) variable and temporary slots (every v(0,X) and t(N) is a slot)
% Calls: the caller reserves 4*N bytes, stores the N arguments there, puts the
% static link in $v1 and does jal; the callee copies the arguments into its
% parameter slots. Result in $v0. Arrays are heap blocks: [size][elements...]
% (arr value = address) with a bounds check on every access.

:- dynamic slot/2, fsize/2, strlabel/2.

mips_gen(ir(Main, Funs)) :-
    retractall(slot(_,_)), retractall(fsize(_,_)), retractall(strlabel(_,_)),
    forall(sub_term(str(S), ir(Main,Funs)), intern(S)),
    layout(main, [], Main),
    forall(member(fun(F,Ps,C), Funs), layout(F, Ps, C)),
    format(".text~n.globl main~n"),
    emit_fun(main, [], Main, true),
    forall(member(fun(F,Ps,C), Funs), emit_fun(F, Ps, C, false)),
    runtime,
    format("~n.data~n"),
    format("__oob_msg: .asciiz \"error: runtime(index_out_of_bounds)\\n\"~n"),
    forall(strlabel(S, L), format("~w: .asciiz \"~w\"~n", [L, S])).

intern(S) :- ( strlabel(S, _) -> true ; aggregate_all(count, strlabel(_,_), N), format(atom(L), 'str_~w', [N]), assertz(strlabel(S, L)) ).

% ---- frame layout: assign a slot to each variable and temporary of a function

layout(F, Params, Code) :-
    findall(U, sub_term(v(0,U), Code), Us0),
    append(Params, Us0, Us1),
    findall(t(N), sub_term(t(N), Code), Ts),
    append(Us1, Ts, Keys0), list_to_set(Keys0, Keys),
    forall(nth0(I, Keys, K), ( Off is 12+4*I, assertz(slot(K, Off)) )),
    length(Keys, N), S0 is 12+4*N, S is ((S0+7)//8)*8,
    assertz(fsize(F, S)).

% ---- functions

emit_fun(F, Params, Code, IsMain) :-
    fsize(F, S),
    format("~n~w:~n", [F]),
    ( IsMain == true ->
        e("addiu $sp, $sp, -~w", [S]), e("move $fp, $sp"), e("sw $zero, 4($fp)")
    ;   e("addiu $sp, $sp, -~w", [S]),
        e("sw $ra, 0($sp)"), e("sw $v1, 4($sp)"), e("sw $fp, 8($sp)"), e("move $fp, $sp") ),
    forall(nth0(I, Params, P),
           ( slot(P, O), A is S+4*I, e("lw $t0, ~w($fp)", [A]), e("sw $t0, ~w($fp)", [O]) )),
    forall(member(I, Code), instr(I, F, S, IsMain)).

% ---- operands

% address register for the frame Up static links above the current one
frame_base(0, '$fp') :- !.
frame_base(Up, '$t8') :-
    e("move $t8, $fp"),
    forall(between(1, Up, _), e("lw $t8, 4($t8)")).

load(num(N), R) :- e("li ~w, ~w", [R, N]).
load(str(S), R) :- strlabel(S, L), e("la ~w, ~w", [R, L]).
load(t(N), R) :- slot(t(N), O), e("lw ~w, ~w($fp)", [R, O]).
load(v(Up,U), R) :- slot(U, O), frame_base(Up, B), e("lw ~w, ~w(~w)", [R, O, B]).

store(t(N), R) :- slot(t(N), O), e("sw ~w, ~w($fp)", [R, O]).
store(v(Up,U), R) :- slot(U, O), frame_base(Up, B), e("sw ~w, ~w(~w)", [R, O, B]).

e(Fmt, Args) :- format("    "), format(Fmt, Args), nl.
e(Fmt) :- e(Fmt, []).

% ---- instructions

instr(label(L), _, _, _) :- format("~w:~n", [L]).
instr(jump(L), _, _, _) :- e("j ~w", [L]).
instr(cjump(A,Rel,B,Lt,Lf), _, _, _) :-
    load(A, '$t0'), load(B, '$t1'), branch(Rel, Op),
    e("~w $t0, $t1, ~w", [Op, Lt]), e("j ~w", [Lf]).
instr(move(D,A), _, _, _) :- load(A, '$t0'), store(D, '$t0').
instr(unop(D,neg,A), _, _, _) :- load(A, '$t0'), e("subu $t0, $zero, $t0"), store(D, '$t0').
instr(binop(D,Op,A,B), _, _, _) :-
    load(A, '$t0'), load(B, '$t1'), arith(Op),
    store(D, '$t0').
instr(load(D,A,I), _, _, _) :-
    load(A, '$t0'), load(I, '$t1'), bounds, e("lw $t0, 4($t0)"), store(D, '$t0').
instr(store(A,I,V), _, _, _) :-
    load(A, '$t0'), load(I, '$t1'), bounds, load(V, '$t2'), e("sw $t2, 4($t0)").
instr(newarray(D,N,I), _, _, _) :-
    load(N, '$a0'), load(I, '$a1'), e("jal __newarray"), store(D, '$v0').
instr(call(D,Fn,Hops,Args), _, _, _) :-
    length(Args, N), Bytes is 4*N,
    ( N > 0 -> e("addiu $sp, $sp, -~w", [Bytes]) ; true ),
    forall(nth0(I, Args, A), ( load(A, '$t0'), O is 4*I, e("sw $t0, ~w($sp)", [O]) )),
    ( Hops =:= 0 -> e("move $v1, $fp")
    ; e("move $v1, $fp"), forall(between(1, Hops, _), e("lw $v1, 4($v1)")) ),
    e("jal ~w", [Fn]),
    ( N > 0 -> e("addiu $sp, $sp, ~w", [Bytes]) ; true ),
    store(D, '$v0').
instr(ret(A), _, S, IsMain) :-
    load(A, '$v0'),
    (   IsMain == true -> e("li $v0, 10"), e("syscall")
    ;   e("lw $ra, 0($fp)"), e("lw $t0, 8($fp)"),
        e("addiu $sp, $fp, ~w", [S]), e("move $fp, $t0"), e("jr $ra") ).
instr(printi(A), _, _, _) :- load(A, '$a0'), e("li $v0, 1"), e("syscall").
instr(print(A), _, _, _) :- load(A, '$a0'), e("li $v0, 4"), e("syscall").
instr(scani(D), _, _, _) :- e("li $v0, 5"), e("syscall"), store(D, '$v0').

branch(eq, beq). branch(diff, bne). branch(lt, blt).
branch(lteq, ble). branch(gt, bgt). branch(gteq, bge).

arith(plus)  :- e("addu $t0, $t0, $t1").
arith(minus) :- e("subu $t0, $t0, $t1").
arith(mult)  :- e("mul $t0, $t0, $t1").
arith(div)   :- e("div $t0, $t1"), e("mflo $t0").
arith(mod)   :- e("div $t0, $t1"), e("mfhi $t0").

% $t0 = array address, $t1 = index -> $t0 = address of the element minus 4
bounds :-
    e("lw $t2, 0($t0)"), e("bgeu $t1, $t2, __oob"),
    e("sll $t1, $t1, 2"), e("addu $t0, $t0, $t1").

runtime :-
    format("~n__newarray:~n"),
    e("move $t0, $a0"), e("addiu $t1, $a0, 1"), e("sll $a0, $t1, 2"),
    e("li $v0, 9"), e("syscall"), e("sw $t0, 0($v0)"),
    e("addiu $t2, $v0, 4"),
    format("__fill:~n"),
    e("blez $t0, __filled"), e("sw $a1, 0($t2)"), e("addiu $t2, $t2, 4"),
    e("addiu $t0, $t0, -1"), e("j __fill"),
    format("__filled:~n"), e("jr $ra"),
    format("~n__oob:~n"),
    e("la $a0, __oob_msg"), e("li $v0, 4"), e("syscall"),
    e("li $a0, 1"), e("li $v0, 17"), e("syscall").
