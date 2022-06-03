# Questions

1. how to use the undersocre for ID
2. how change this operator(+) to only display + on the tokenizer

# Things to add to the tokenizer

1. Let
2. in

# Inputs

## Lexer
1. tokenize([let,function,x,(,y,:,string,),=,10,in,10]).
## Parser
1. parse(["let","function","x","(","y",":","string",")","=","10","in","10"],Ast).
2. parse(["let","var","x",":=","5","var","y",":=","7","in","10"],Ast).
3. parse(["let","var","x",":=","5","var","y",":=","7","in","10","+","5"],Ast).
4. parse(["let","var","s",":=","0","var","n",":=","1","in","while","n","<=","10","do","(","s",":=","s","+","n","*","n",";","n",":=","n","+","1",")",";", "printi","(","n",")"], Ast).

## Problems

1.
2.
3. Removing left recursion
4. id(X) not working correctly on for_to_do
4. printi not working right, calling as a function not as the printi funcition
