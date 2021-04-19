(* User  declarations *)
%%
(* required declarations *)
%name Calc

%term
  ID of string | BOOL of bool | NUM of int
| NOT | AND | OR | XOR  | EQUALS | IMPLIES | PLUS | MINUS | TIMES | NEGATE | LESSTHAN
| GREATERTHAN| IF | THEN | ELSE | EOF | LPAREN | RPAREN | TERM | FI | LET | IN | END | ASSIGN
| FUN | COLON | ARROW | FN | INTTYPE | BOOLTYPE

%nonterm exp of AST.exp | START of AST.program option | program of AST.program | decl of AST.decl
| function of AST.function | primitive of AST.primitive | statement of AST.statement
| typ of AST.typ

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

(* %left *)
%left THEN ELSE IF FI
%right IMPLIES
%left AND OR XOR EQUALS
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%right TIMES
%right NEGATE NOT
%left TERM

(* %right *)


(*%nonassoc program statement*)
%start START

%verbose

%%
START: program ( (*print("["); print_program(program); print("]\n\n");*) SOME program)
      |    (NONE)

program: statement TERM program (AST.Program(statement, program))
  | statement (AST.Single(statement))
  | statement TERM (AST.Single(statement))

  function: FUN ID LPAREN ID COLON typ RPAREN COLON typ ARROW exp (AST.Fun(AST.VarExp(ID1), AST.VarExp(ID2), AST.Type(typ1), AST.Type(typ2), AST.Exp(exp)))
    | FUN ID LPAREN ID COLON typ RPAREN COLON primitive ARROW exp (AST.Fun(AST.VarExp(ID1), AST.VarExp(ID2), AST.Type(typ), AST.Type0(primitive), AST.Exp(exp)))
    | FUN ID LPAREN ID COLON primitive RPAREN COLON typ ARROW exp (AST.Fun(AST.VarExp(ID1), AST.VarExp(ID2), AST.Type0(primitive), AST.Type(typ), AST.Exp(exp)))
    | FUN ID LPAREN ID COLON primitive RPAREN COLON primitive ARROW exp (AST.Fun(AST.VarExp(ID1), AST.VarExp(ID2), AST.Type0(primitive1), AST.Type0(primitive2), AST.Exp(exp)))

  statement: function (AST.Function(function))
    | exp (AST.Expression(exp))

  primitive: INTTYPE (AST.Int)
    | BOOLTYPE (AST.Bool)
    | LPAREN primitive RPAREN (AST.Primitive(primitive))
    | LPAREN primitive RPAREN (AST.Primitive(primitive))

  typ: primitive (AST.Type0(primitive))
    |  primitive ARROW primitive (AST.Type1(primitive1, primitive2))
    | LPAREN typ RPAREN ARROW primitive (AST.Type2(typ, primitive))
    | primitive ARROW LPAREN typ RPAREN (AST.Type3(primitive, typ))
    | LPAREN typ RPAREN ARROW LPAREN typ RPAREN (AST.Type4(typ1, typ2))
    | LPAREN typ RPAREN (AST.Type(typ))

  decl: ID ASSIGN exp (AST.ValDecl(ID, exp))

  exp: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | BOOL (AST.BoolExp(BOOL))
  | LPAREN exp RPAREN (AST.Exp(exp))
  | NOT exp (AST.UnExp(AST.Not, exp))
  | NEGATE exp (AST.UnExp(AST.Negate, exp))
  | exp AND exp (AST.BinExp(AST.And, exp1, exp2))
  | exp OR  exp (AST.BinExp(AST.Or, exp1, exp2))
  | exp XOR  exp (AST.BinExp(AST.Xor, exp1, exp2))
  | exp EQUALS  exp (AST.BinExp(AST.Equals, exp1, exp2))
  | exp IMPLIES exp (AST.BinExp(AST.Implies, exp1, exp2))
  | exp PLUS exp (AST.BinExp(AST.Plus, exp1, exp2))
  | exp MINUS exp (AST.BinExp(AST.Minus, exp1, exp2))
  | exp TIMES exp (AST.BinExp(AST.Times, exp1, exp2))
  | exp GREATERTHAN exp (AST.BinExp(AST.Greaterthan, exp1, exp2))
  | exp LESSTHAN exp (AST.BinExp(AST.Lessthan, exp1, exp2))
  | IF exp THEN exp ELSE exp FI (AST.IfThenElseExp(exp1, exp2, exp3))
  | LET decl IN exp END (AST.LetExp(decl, exp))
  | ID exp (AST.AppExp(AST.VarExp(ID), exp))
  | FN LPAREN ID COLON typ RPAREN COLON typ ARROW exp (AST.FnExp(AST.VarExp(ID), AST.Type(typ1), AST.Type(typ2), AST.Exp(exp)))
  | FN LPAREN ID COLON typ RPAREN COLON primitive ARROW exp (AST.FnExp(AST.VarExp(ID), AST.Type(typ), AST.Type0(primitive), AST.Exp(exp)))
  | FN LPAREN ID COLON primitive RPAREN COLON typ ARROW exp (AST.FnExp(AST.VarExp(ID), AST.Type0(primitive), AST.Type(typ), AST.Exp(exp)))
  | FN LPAREN ID COLON primitive RPAREN COLON primitive ARROW exp (AST.FnExp(AST.VarExp(ID), AST.Type0(primitive1), AST.Type0(primitive2), AST.Exp(exp)))
