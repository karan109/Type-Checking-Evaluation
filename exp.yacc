(* User  declarations *)
%%
(* required declarations *)
%name Calc

%term
  ID of string | CONST of string
| NOT | AND | OR | XOR  | EQUALS | IMPLIES | IF | THEN | ELSE | EOF | LPAREN | RPAREN | TERM | FI

%nonterm exp of AST.exp | START of AST.program option | statement of AST.statement | program of AST.program

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

(* %left *)
%left THEN ELSE IF FI
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left TERM

(* %right *)


(*%nonassoc program statement*)
%start START

%verbose

%%
START: program (print("["); print_program(program);print("]\n\n"); SOME program)
      |    (NONE)

program: program statement(AST.Exp4(program, statement))
 | statement (AST.Exp5(statement))
statement : exp TERM (AST.Exp3(exp, AST.TERM))

  exp: CONST (AST.CONST(CONST))
  | ID (AST.ID(ID))
  | LPAREN exp RPAREN (AST.BinExp2(AST.LPAREN, exp1, AST.RPAREN))
  | NOT exp (AST.UnExp(AST.NOT, exp1))
  | exp AND exp (AST.BinExp(exp1, AST.AND, exp2))
  | exp OR  exp (AST.BinExp(exp1, AST.OR, exp2))
  | exp XOR  exp (AST.BinExp(exp1, AST.XOR, exp2))
  | exp EQUALS  exp (AST.BinExp(exp1, AST.EQUALS, exp2))
  | exp IMPLIES exp (AST.BinExp(exp1, AST.IMPLIES, exp2))
  | IF exp THEN exp ELSE exp FI (AST.TerExp(AST.IF, exp1, AST.THEN, exp2, AST.ELSE, exp3))
