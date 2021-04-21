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
	       SOME (x, v)   => v
	    |   NONE => raise EnvironmentLookupError
fun isInEnv(var, env) = 
	case List.find(fn (x, _) => x = var) env of
	       SOME (x, v)   => true
	    |   NONE => false

datatype statement = Function of function
	| Expression of exp
and
program = Program of statement * program
	| Single of statement
end