/*Program → [ Instructions ]
Instructions → Instruction
Instructions → Instruction , Instructions
Instruction → LABEL labelid
Instruction → id := Atom
Instruction → id := unop Atom
Instruction → id := id binop Atom
Instruction → id := M[Atom]
Instruction → M[Atom] := id
Instruction → GOTO labelid
Instruction → IF id relop Atom THEN labelid ELSE labelid
Instruction → id := CALL functionid(Args)
Atom → id
Atom → num
Args → id
Args → id , Args
*/


