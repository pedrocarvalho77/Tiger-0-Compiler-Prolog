

lookup(Key, [(Key, Value)|Dict], Value).
lookup(Key, [(Key1, Value1)|Dict], Value) :-
    Key =\= Key1 , lookup(Key, Dict, Value).


% Code Generator

%% encode(+Structure,-Dictionary,-RelocatableCode)
% RelocatableCode is generated from the parsed Structure (Ast)
% building a Dictionary associating variables with addresses.
% An incomplete ordered binary tree is used to implement it, as described in Section 15.3. The predicate
% lookup(Name,D,Value) (Program 15.9) is used for accessing the incomplete binary tree.
% The functor ; is used to denote sequencing.

encode((X;Xs), D,(Y;Ys)) :-
  encode(X, D, Y),
  encode(Xs, D, Ys).

encode(void, _D, no_op).

encode(assign(Name, E), D, (Code; instr(store, Address))) :-
  lookup(Name, D, Address),
  encode_expression(E, D, Code).
/*
encode(if(Test, Then, Else), D, (TestCode; ThenCode; instr(jump, L2); label(L1); ElseCode; label(L2))) :-
  encode_test(Test, L1, D, TestCode),
  encode(Then, D, ThenCode),
  encode(Else, D, ElseCode).

encode(while(Test, Do), D, (label(L1); TestCode; DoCode; instr(jump, L1); label(L2))) :-
  encode_test(Test, L2, D, TestCode),
  encode(Do, D, DoCode).

encode(pl_read(X), D, instr(read, Address)) :-
  lookup(X, D, Address).

encode(pl_write(E), D, (Code; instr(write, 0))) :-
  encode_expression(E, D, Code).
*/
%% encode_expression(Expression, Dictionary, Code)
%    Code corresponts to an arithmetic Expression.

encode_expression(number(C), _D, instr(loadc, C)).

encode_expression(name(X), D, instr(load, Address)) :-
  lookup(X, D, Address).

encode_expression(expr(Op, E1, E2), D, (Load; Instruction)) :-
  single_instruction(Op, E2, D, Instruction),
  encode_expression(E1, D, Load).
  
encode_expression(expr(Op, E1, E2), D, Code) :-
  \+single_instruction(Op, E2, D, _Instruction),
  single_operation(Op, E1, D, E2Code, Code),
  encode_expression(E2, D, E2Code).

single_instruction(Op, number(C), _D, instr(OpCode, C)) :-
  literal_operation(Op, OpCode).

single_instruction(Op, name(X), D, instr(OpCode, A)) :-
  memory_operation(Op, OpCode), lookup(X, D, A).

single_operation(Op, E, D, Code, (Code; Instruction)) :-
  commutative(Op),
  single_instruction(Op, E, D, Instruction).

single_operation(Op, E, D, Code, (Code; instr(store, Address); Load; instr(OpCode, Address))) :-
  \+commutative(Op),
  lookup('$temp', D, Address),
  encode_expression(E, D, Load),
  op_code(E, Op, OpCode).

op_code(number(_C), Op, OpCode) :-
  literal_operation(Op, OpCode).
  
op_code(name(_C), Op, OpCode) :-
  memory_operation(Op, OpCode).

literal_operation("+", addc).
literal_operation("-", subc).
literal_operation("*", mulc).
literal_operation("/", divc).

memory_operation("+", add).
memory_operation("-", sub).
memory_operation("*", mul).
memory_operation("/", div).

commutative("+").
commutative("*").

encode_test(compare(Op, E1, E2), Label, D, (Code; instr(OpCode, Label))) :-
  comparison_opcode(Op, OpCode),
  encode_expression(expr("-", E1, E2), D, Code).

comparison_opcode("=", jumpeq).
comparison_opcode("!=", jumpne).
comparison_opcode(">", jumpgt).
comparison_opcode("<", jumplt).
comparison_opcode(">=", jumpge).
comparison_opcode("<=", jumple).