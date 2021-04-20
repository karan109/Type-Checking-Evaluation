structure EVALUATOR  =
struct
open AST

(*val brokenTypes = Fail "Error in evaluation!"*)
exception brokenTypes;

fun checkTypes(t1 : typ, t2 : typ) = 
	case(t1, t2) of
		(Int, Int) => true
		| (Bool, Bool) => true
		| (Type t, Int) => checkTypes(t, Int)
		| (Type t, Bool) => checkTypes(t, Bool)
		| (Type t, BinType(t3, t4)) => checkTypes(t, t2)
		| (Int, Type t) => checkTypes(t, Int)
		| (Bool, Type t) => checkTypes(t, Bool)
		| (BinType(t3, t4), Type t) => checkTypes(t, t1)
		| (Type t3, Type t4) => checkTypes(t3, t4)
		| (BinType (t3, t4), BinType(t5, t6)) =>
			if checkTypes(t3, t5) andalso checkTypes(t4, t6) then true
			else false
		| _ => false

fun evalExp(e : exp, env : environment) = 
	case e of
		NumExp n => IntVal n
		| StringExp s => StringVal s
		| BoolExp b => BoolVal b
		| Exp expression => evalExp(expression, env)
		| VarExp x => envLookup (x, env)
		| BinExp (b, e1, e2) => evalBinExp(b, e1, e2, env)
		| UnExp (u, e) => evalUnExp(u, e, env)
		| LetExp(ValDecl(x, e1), e2) => evalExp(e2, envAdd (x, evalExp (e1, env), env))
		| IfThenElseExp(a1, a2, a3) => evalIfThenElseExp(a1, a2, a3, env)
		| AppExp(var, expression) => evalAppExp(var, expression, env)
		| FnExp(arg, typ1, typ2, expression, env_temp) => FunVal(arg, typ1, typ2, expression, env)
and
evalBinExp(b : binop, e1 : exp, e2 : exp, env : environment) = 
	case (b, evalExp(e1, env), evalExp(e2, env)) of
		(Plus, IntVal n1, IntVal n2) => IntVal (n1 + n2)
		| (Minus, IntVal n1, IntVal n2) => IntVal (n1 - n2)
		| (Times, IntVal n1, IntVal n2) => IntVal (n1 * n2)
		| (And, BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
		| (Or, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
		| (Xor, BoolVal b1, BoolVal b2) => 
			if (b1 = true andalso b2 = false) orelse (b1 = false andalso b2 = true)
				then BoolVal (true)
			else BoolVal (false)
		| (Implies, BoolVal b1, BoolVal b2) => 
			if (b1 = false andalso b2 = true) then BoolVal (false)
			else BoolVal (true)
		| (Equals, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
		| (Equals, IntVal n1, IntVal n2) => BoolVal (n1 = n2)
		| (Greaterthan, IntVal n1, IntVal n2) => BoolVal (n1 > n2)
		| (Lessthan, IntVal n1, IntVal n2) => BoolVal (n1 < n2)
		| _ => raise brokenTypes
and
evalUnExp(u : unop, e : exp, env : environment) = 
	case (u, evalExp(e, env)) of
		(Negate, IntVal n) => IntVal (0-n)
		| (Not, BoolVal b) => 
			if b = true then BoolVal (false) else BoolVal (true)
		| _ => raise brokenTypes
and
evalIfThenElseExp(a1 : exp, a2: exp, a3 : exp, env : environment) = 
	case (evalExp(a1, env), evalExp(a2, env), evalExp(a3, env)) of
		(BoolVal b, IntVal n1, IntVal n2) => if b then IntVal n1 else IntVal n2
		| (BoolVal b, BoolVal b1, BoolVal b2) => if b then BoolVal b1 else BoolVal b2
		| (BoolVal b, FunVal f1, FunVal f2) => if b then FunVal f1 else FunVal f2
		| _ => raise brokenTypes
and
evalAppExp(var : exp, a: exp, env : environment) = 
	let 
		val f = evalExp(var, env)
		fun check() = 
			case f of 
				FunVal(arg, typ1, typ2, expression, env_fun) =>
					(case evalExp(a, env) of
						IntVal n1 => checkTypes(Int, typ1)
						| BoolVal b1 => checkTypes(Bool, typ1)
						| FunVal (arg_temp, typ1_temp, typ2_temp, expression_temp, env_temp) => checkTypes(BinType(typ1_temp, typ2_temp), typ1)
						| _ => raise brokenTypes )
				| _ => raise brokenTypes
		fun eval() = 
			case f of 
				FunVal(VarExp(arg), typ1, typ2, expression, env_fun) =>
					let 
						val res = evalExp(expression, envAdd(arg, evalExp(a, env), env_fun))
					in
						(case res of
							BoolVal b => if checkTypes(Bool, typ2) = false then raise brokenTypes else res
							| IntVal n => if checkTypes(Int, typ2) = false then raise brokenTypes else res
							| FunVal (arg_temp, typ1_temp, typ2_temp, expression_temp, env_temp) => 
								if checkTypes(BinType(typ1_temp, typ2_temp), typ2) = false then raise brokenTypes else res
							| _ => raise brokenTypes )
					end
				| _ => raise brokenTypes
		in
			if check() then eval()
			else raise brokenTypes
		end
and
evalFunc(f : function, env : environment) = StringVal("Function Definition")

and
evalProgram(arg, env) = 
	case arg of
		Program(p, b) =>
			(case p of Function(f) =>
				(case f of Fun(VarExp(var) , bound , typ1 , typ2 , expression, env_fun) =>
					evalFunc(f, env) :: evalProgram(b, envAdd(var, FunVal(bound , typ1 , typ2 , expression, env), env))
				| _ => raise brokenTypes)
			| Expression(e) =>
				evalExp(e, env) :: evalProgram(b, env))
		| Single(p) =>
			(case p of Function(f) =>
				(case f of Fun(VarExp(var) , bound , typ1 , typ2 , expression, env_fun) =>
					[evalFunc(f, env)]
				| _ => raise brokenTypes)
			| Expression(e) =>
				[evalExp(e, env)] )

(*fun evalResult([]) = []
	| evalResult(x::l) = 
		case x of
			BoolVal b1 => ( Bool.toString b1 ) :: (evalResult l)
			| IntVal n1 => ( Int.toString n1 ) :: (evalResult l)
			| FunVal (VarExp(bound) , typ1 , typ2 , expression, env) => "Fn ("^bound^")" :: (evalResult l)
			| StringVal s1 => s1 :: (evalResult l)
			| _ => raise brokenTypes*)

fun evalResult([]) = ()
	| evalResult(x::l) = 
		case x of
			BoolVal b1 => ( ( print ((Bool.toString b1)^", ") ) ; (evalResult l) )
			| IntVal n1 => ( ( print ((Int.toString n1)^", ") ) ; (evalResult l) )
			| FunVal (VarExp(bound) , typ1 , typ2 , expression, env) =>  ((print ("Fn ("^bound^"), ")) ; (evalResult l))
			| StringVal s1 => ((print (s1^", ") ) ; (evalResult l))
			| _ => raise brokenTypes
end
