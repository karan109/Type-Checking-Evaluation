structure AST =
struct

type id = string

datatype binop = Plus | Minus | Times | And | Or | Xor | Equals | Implies | Greaterthan | Lessthan
and
	unop = Not | Negate

datatype primitive = Bool | Int
					| Primitive of primitive
and
	typ = Type of typ
		| Type0 of primitive 
		| Type1 of primitive * primitive
		| Type2 of typ * primitive
		| Type3 of primitive * typ
		| Type4 of typ * typ

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
		| FnExp of exp * typ * typ * exp

and
	function = Fun of exp * exp * typ * typ * exp
				       
datatype value = IntVal of int
               | StringVal of string
	       | BoolVal of bool
				
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"

datatype statement = Function of function
					| Expression of exp
and
	program = Program of statement * program
				| Single of statement
end