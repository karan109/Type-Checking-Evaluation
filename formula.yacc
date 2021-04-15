(* User  declarations *)
fun lookup "special" = true
  | lookup s = false 
val axa = ref "567"
%%
(* required declarations *)
%name Calc

%term
  ID of string | CONST of string
| NOT | AND | OR | XOR  | EQUALS | IMPLIES | IF | THEN | ELSE | EOF | LPAREN | RPAREN | TERM

%nonterm formula of AST.exp | START of AST.program option | statement of AST.statement | program of AST.program

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left THEN ELSE IF
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
statement : formula TERM (AST.Exp3(formula, AST.TERM))

  formula: CONST (AST.CONST(CONST))
  | ID (AST.ID(ID))
  | LPAREN formula RPAREN (AST.BinExp2(AST.LPAREN, formula1, AST.RPAREN))
  | NOT formula (AST.UnExp(AST.NOT, formula1))
  | formula AND formula (AST.BinExp(formula1, AST.AND, formula2))
  | formula OR  formula (AST.BinExp(formula1, AST.OR, formula2))
  | formula XOR  formula (AST.BinExp(formula1, AST.XOR, formula2))
  | formula EQUALS  formula (AST.BinExp(formula1, AST.EQUALS, formula2))
  | formula IMPLIES formula (AST.BinExp(formula1, AST.IMPLIES, formula2))
  | IF formula THEN formula ELSE formula (AST.TerExp(AST.IF, formula1, AST.THEN, formula2, AST.ELSE, formula3))
