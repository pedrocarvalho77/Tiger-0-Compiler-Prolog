tokens(Z) --> "while", tokens(Y), {Z = [while | Y]}.
tokens(Z) --> "do", tokens(Y), {Z = [do | Y]}.
tokens(Z) --> "endwhile", tokens(Y), {Z = [endwhile | Y]}.
tokens(Z) --> "repeat", tokens(Y), {Z = [repeat | Y]}.
tokens(Z) --> "until", tokens(Y), {Z = [until | Y]}.
tokens(Z) --> "endrepeat", tokens(Y), {Z = [endrepeat | Y]}.
tokens(Z) --> "if", tokens(Y), {Z = [if | Y]}.
tokens(Z) --> "then", tokens(Y), {Z = [then | Y]}.
tokens(Z) --> "else", tokens(Y), {Z = [else | Y]}.
tokens(Z) --> "endif", tokens(Y), {Z = [endif | Y]}.
tokens(Z) --> "exit", tokens(Y), {Z = [exit | Y]}.
tokens(Z) --> "other", tokens(Y), {Z = [other | Y]}.

/*
program(S)              --> ["program"], identifier(_Name), [";"], statement(S).

statement((S;Ss))          --> ["begin"], statement(S), rest_statements(Ss).
statement(assign(X,E))     --> identifier(X), [":", "="], expression(E).
statement(if(T,S1,S2))     --> ["if"], test(T), ["then"], statement(S1), ["else"], statement(S2).
statement(while(T,S))      --> ["while"], test(T), ["do"], statement(S).
statement(pl_read(X))      --> ["read"], identifier(X).
statement(pl_write(X))     --> ["write"], expression(X).

rest_statements((S;Ss))    --> [";"], statement(S), rest_statements(Ss).
rest_statements(void)      --> ["end"].

expression(X)              --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).

arithmetic_op("+")         --> ["+"].
arithmetic_op("-")         --> ["-"].
arithmetic_op("*")         --> ["*"].
arithmetic_op("/")         --> ["/"].

pl_constant(number(X))     --> pl_integer(X), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(name(X))       --> identifier(X).

pl_integer(X)              --> [Y], { number_string(X, Y) }.
identifier(X)              --> [Y], { atom_string(X, Y) }.

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].
*/


/*
expr : num                                         { Num $1 }
     | id                                          { Var $1 }
     | expr '+' expr                               { OpBin Plus $1 $3 }
     | expr '-' expr                               { OpBin Minus $1 $3 }
     | expr '*' expr                               { OpBin Times $1 $3 }
     | expr '/' expr                               { OpBin Div $1 $3 }
     | expr '%' expr                               { OpBin Mod $1 $3 }
     | expr '=' expr                               { OpRel Eq $1 $3 }
     | expr '<>' expr                              { OpRel Diff $1 $3 }
     | expr '<' expr                               { OpRel Lt $1 $3 }
     | expr '<=' expr                              { OpRel Lteq $1 $3 }
     | expr '>'  expr                              { OpRel Gt $1 $3 }
     | expr '>=' expr                              { OpRel Gteq $1 $3 }
     | expr '&' expr                               { OpRel And $1 $3 }
     | expr '|' expr                               { OpRel Or $1 $3 }
     | '-' expr %prec NEG                          { NegExp $2 }
     | id ':=' expr                                { Assign $1 $3 }
     | id '(' exprList ')'                         { FunCall $1 $3 }
     | '(' exprSeq ')'                             { ExprSeq $2 }
     | if expr then expr else expr                 { IfThenElse $2 $4 $6 }
     | if expr then expr %prec OUTERTHEN           { IfThen $2 $4 }
     | while expr do expr                          { WhileDo $2 $4 }
     | for id ':=' expr to expr do expr            { ForToDo $2 $4 $6 $8 }
     | break                                       { Break }
     | let varDeclList in exprSeq end              { LetInEnd $2 $4 }
     | 'printi' '(' expr ')'                       { PrintInt $3 }
     | 'scani' '(' ')'                             { ScanInt }
     -- | 'print' '(' string ')'                      { Print $3 }\  
     */