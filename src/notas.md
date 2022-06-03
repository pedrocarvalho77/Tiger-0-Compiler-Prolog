# Questions

1. how to use the undersocre for ID
2. how change this operator(+) to only display + on the tokenizer

# Things to add to the tokenizer

1. Let
2. in

# Inputs

## Lexer
1. tokenize([let,function,x,(,y,:,string,),=,10,in,10]).
## Parser to Test
1. parse(["let","function","x","(","y",":","string",")","=","10","in","10"],Ast).
2. parse(["let","var","x",":=","5","var","y",":=","7","in","10"],Ast).
3. parse(["let","var","x",":=","5","var","y",":=","7","in","10","+","5"],Ast).
4. parse(["let","var","s",":=","0","var","n",":=","1","in","for","n",":=","10","to","n",":=","n","+","1","do","x",":=","10"],Ast).
5. parse(["let","var","s",":=","0","var","n",":=","1","in","while","n","<=","10","do","(","s",":=","s","+","n","*","n",";","n",":=","n","+","1",")"], Ast).
6. parse(["let","function","fact","(","n",":","int",")",":","int","=","if","n",">","0","then","n","*","fact","(","n","-","1",")","else","1","in","printi","(","fact","(","10",")",")"], Ast).
7. parse(["let","function","fact","(","n",":","int",")",":","int","=","if","n",">","0","then","n","*","5","else","1","in","printi","(","fact","(","10",")",")"], Ast).
8. parse(["let","function","fact","(","n",":","int",")",":","int","=","if","n",">","0","then","n","*","fact","(","n","-","1",")","else","1","in","printi","(","fact","(","10",")",")"], Ast).

## Parser real examples

1. parse(["let","var","s",":=","0","var","n",":=","1","in","while","n","<=","10","do","(","s",":=","s","+","n","*","n",";","n",":=","n","+","1",")",";","printi","(","n",")"], Ast).
2. parse(["let","function","fact","(","n",":","int",")",":","int","=","if","n",">","0","then","n","*","fact","(","n","-","1",")","else","1","in","printi","(","fact","(","10",")",")"], Ast).
3.
4.
5.
## Problems

1. logical expressions still missing (& | operators)
2. fix lexer
3. combine all the calls into the compiler.pl file
4. fix backtraing (add cut or look for the integer and identifier definitions)
5. printi not working right, calling as a function not as the printi funcition
6. define what should be on the nodes or not in the ABS
7. Learning prolog, especially CDG