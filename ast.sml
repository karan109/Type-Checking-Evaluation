structure AST =
struct

type id = string
exception EnvironmentLookupError;

datatype binop = Plus | Minus | Times | And | Or | Xor | Equals | Implies | Greaterthan | Lessthan
and
	unop = Not | Negate

datatype typ = Bool
		| Int
		| Type of typ
		| BinType of typ * typ

datatype decl = ValDecl of id * exp

and 
	exp = NumExp of int
		| BoolExp of bool
    	| StringExp of string
    	| Exp of exp
    	| VarExp of id
		| BinExp of binop * exp * exp
		| UnExp of unop * exp
		| LetExp of decl * exp
		| IfThenElseExp of exp * exp * exp
		| AppExp of exp * exp
		| FnExp of exp * typ * typ * exp * (id * value) list

and
	function = Fun of exp * exp * typ * typ * exp * (id * value) list
and			       
	value = IntVal of int
           | StringVal of string
           | BoolVal of bool
           | FunVal of exp * typ * typ * exp * (id * value) list
	
type environment = (id * value) list

fun envAdd (var, v, []) = [(var,v)]
	| envAdd (var, v, (x, y)::ls) = 
		if x = var then (var, v)::ls
		else (x, y)::envAdd (var, v, ls)

fun envLookup (var, env) =
    case List.find(fn (x, _) => x = var) env of
	       SOME (x, v) => v 
	    |   NONE => raise EnvironmentLookupError

fun isInEnv(var, env) = 
	case List.find(fn (x, _) => x = var) env of
	       SOME (x, v) => true
	    |   NONE => false

datatype statement = Function of function
	| Expression of exp
and
program = Program of statement * program
	| Single of statement

fun programToString(a, ct) =
	(indent(ct);
	case a of
		Program(s, p) => (print("Program(\n"); StatementToString(s, ct+1); print(",\n"); programToString(p, ct+1); print(")"))
		| Single(s) => (print("Program(\n"); StatementToString(s, ct+1); print(")")) )
and
StatementToString(s, ct) = 
	(indent(ct);
	case s of
		Function f => (print("Function(\n"); FunctionToString(f, ct+1); print(")"))
		| Expression e => (print("Expression(\n"); ExpressionToString(e, ct+1); print(")")) )
and
FunctionToString(f, ct) = 
	( indent(ct);
	case f of Fun(name, arg, typ1, typ2, expression, env) =>
		(print("Fun(\n"); ExpressionToString(name, ct+1); print(",\n"); ExpressionToString(arg, ct+1); print(",\n"); TypeToString(typ1, ct+1); print(",\n"); TypeToString(typ2, ct+1); print(",\n"); ExpressionToString(expression, ct+1); print(")")) )
and
ExpressionToString(e, ct) =
	( indent(ct);
	case e of
		VarExp x => ( print("VarExp \""); print(x); print("\"") )
		| NumExp n => ( print("NumExp "); print(Int.toString(n)) )
		| BoolExp b => ( print("BoolExp "); print(Bool.toString(b)) )
		| StringExp s => ( print("StringExp \""); print(s); print("\"") )
		| Exp ex => ( print("Exp(\n"); ExpressionToString(ex, ct+1); print(")") )
		| BinExp(oper, a1, a2) => ( print("BinExp(\n"); indent(ct+1); BinToString(oper); print(",\n"); ExpressionToString(a1, ct+1); print(",\n"); ExpressionToString(a2, ct+1); print(")") )
		| UnExp(oper, a1) => ( print("UnExp(\n"); indent(ct+1); UnToString(oper); print(",\n"); ExpressionToString(a1, ct+1); print(")") )
		| LetExp(dec, ex) => ( print("LetExp(\n"); DeclToString(dec, ct+1); print(",\n"); ExpressionToString(ex, ct+1); print(")") )
		| IfThenElseExp(e1, e2, e3) => ( print("IfThenElseExp(\n"); ExpressionToString(e1, ct+1); print(",\n"); ExpressionToString(e2, ct+1); print(",\n"); ExpressionToString(e3, ct+1); print(")") )
		| AppExp(e1, e2) => ( print("AppExp(\n"); ExpressionToString(e1, ct+1); print(",\n"); ExpressionToString(e2, ct+1); print(")") )
		| FnExp(arg, typ1, typ2, expression, env) => (print("FnExp(\n"); ExpressionToString(arg, ct+1); print(",\n"); TypeToString(typ1, ct+1); print(",\n"); TypeToString(typ2, ct+1); print(",\n"); ExpressionToString(expression, ct+1); print(")")) )
and
DeclToString(dec, ct) = 
	( indent(ct); 
	case dec of
		ValDecl(s, e) => ( print("ValDecl(\n"); indent(ct+1); print("\""); print(s); print("\""); print(",\n"); ExpressionToString(e, ct+1); print(")") )
	)
and
TypeToString(t, ct) = 
	( indent(ct);
	case t of
		Int => print("Int")
		| Bool => print("Bool")
		| Type(tt) => ( print("Type(\n"); TypeToString(tt, ct+1); print(")") )
		| BinType(t1, t2) => ( print("BinType(\n"); TypeToString(t1, ct+1); print(",\n"); TypeToString(t2, ct+1); print(")") )
		)
and
BinToString(oper) = 
	case oper of
		Plus => print("Plus")
		| Minus => print("Minus")
		| Times => print("Times")
		| And => print("And")
		| Or => print("Or")
		| Xor => print("Xor")
		| Equals => print("Equals")
		| Implies => print("Implies")
		| Lessthan => print("Lessthan")
		| Greaterthan => print("Greaterthan")
and
UnToString(oper) = 
	case oper of
		Not => print("Not")
		| Negate => print("Negate")
and
indent(ct) = 
	if ct = 0 then ()
	else (print("    "); indent(ct-1) )
end