structure EVALUATOR  =
struct
open AST

exception brokenTypes;
val funcs = ref ([]:environment)
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

fun getTypeFun(e : exp, env : environment, bound: (string * typ) list):typ = 
	case e of
		NumExp n => Int
		| StringExp s => raise brokenTypes
		| BoolExp b => Bool
		| Exp expression => getTypeFun(expression, env, bound)
		| VarExp x => 
			if isInEnv(x, bound) then envLookup(x, bound)
				
			else
				(case envLookup (x, env) of
					IntVal n => Int
					| BoolVal b => Bool
					| FunVal(VarExp arg, typ1, typ2, expression, env_temp) => 
						if checkTypes(getTypeFun(expression, env_temp, envAdd(arg, typ1, bound)), typ2) then BinType(typ1, typ2)
						else raise brokenTypes
					| _ => 
						(case envLookup (x, !funcs) of 
							FunVal(VarExp arg, typ1, typ2, expression, env_temp) => BinType(typ1, typ2)
							| _ => raise brokenTypes )
				)
		| BinExp (b, e1, e2) => 
			let val t1 = getTypeFun(e1, env, bound)
				val t2 = getTypeFun(e2, env, bound)
			in
				if checkTypes(t1, t2) then
					(case (b, getRealType(t1)) of
						(Plus, Int) => t1
						| (Minus, Int) => t1
						| (Times, Int) => t1
						| (Greaterthan, Int) => Bool
						| (Lessthan, Int) => Bool
						| (And, Bool) => t1
						| (Or, Bool) => t1
						| (Implies, Bool) => t1
						| (Equals, Int) => Bool
						| (Equals, Bool) => Bool
						| (Xor, Bool) => t1
						| _ => raise brokenTypes)
				else raise brokenTypes
			end

		| UnExp (u, e) => 
			(case (u, getRealType(getTypeFun(e, env, bound))) of
				(Not, Bool) => Bool
				| (Negate, Int) => Int
				| _ => raise brokenTypes)
		| LetExp(ValDecl(x, e1), e2) => 
			getTypeFun(e2, env, envAdd(x, getTypeFun(e1, env, bound), bound))
		| IfThenElseExp(a1, a2, a3) => 
			let val t1 = getTypeFun(a2, env, bound)
			in
				if checkTypes(getTypeFun(a1, env, bound), Bool) = true andalso checkTypes(t1, getTypeFun(a3, env, bound)) = true then t1
				else raise brokenTypes
			end
		| AppExp(var, expression) => 
			(case getRealType(getTypeFun(var, env, bound)) of
				BinType(t1, t2) => if checkTypes(getTypeFun(expression, env, bound), t1) = true then t2 else raise brokenTypes
				| _ => raise brokenTypes)
		| FnExp(VarExp arg, typ1, typ2, expression, env_temp) => 
			if checkTypes(getTypeFun(expression, env_temp, envAdd(arg, typ1, bound)), typ2) = true then BinType(typ1, typ2)
			else raise brokenTypes
		| _ => raise brokenTypes
and
getType(e : exp, env : environment) = 
	case e of
		NumExp n => Int
		| StringExp s => raise brokenTypes
		| BoolExp b => Bool
		| Exp expression => getType(expression, env)
		| VarExp x => (case envLookup (x, env) of
			IntVal n => Int
			| BoolVal b => Bool
			| FunVal(arg, typ1, typ2, expression, env_temp) => typ2
			| _ => 
				(case envLookup (x, !funcs) of 
					FunVal(arg, typ1, typ2, expression, env_temp) => typ2
					| _ => raise brokenTypes) )
		| BinExp (b, e1, e2) => getType(e1, env)
		| UnExp (u, e) => getType(e, env)
		| LetExp(ValDecl(x, e1), e2) => getType(e2, envAdd (x, evalExp (e1, env), env))
		| IfThenElseExp(a1, a2, a3) => getType(a2, env)
		| AppExp(var, expression) => getType(var, env)
		| FnExp(arg, typ1, typ2, expression, env_temp) => typ2
and
evalExp(e : exp, env : environment) = 
	case e of
		NumExp n => IntVal n
		| StringExp s => StringVal s
		| BoolExp b => BoolVal b
		| Exp expression => evalExp(expression, env)
		| VarExp x => (envLookup (x, env))
		| BinExp (b, e1, e2) => evalBinExp(b, e1, e2, env)
		| UnExp (u, e) => evalUnExp(u, e, env)
		| LetExp(ValDecl(x, e1), e2) => evalExp(e2, envAdd (x, evalExp (e1, env), env))
		| IfThenElseExp(a1, a2, a3) => evalIfThenElseExp(a1, a2, a3, env)
		| AppExp(var, expression) => evalAppExp(var, expression, env)
		| FnExp(arg, typ1, typ2, expression, env_temp) => 
			if checkTypes(getTypeFun(FnExp(arg, typ1, typ2, expression, env), env, []), BinType(typ1, typ2)) = true then 
				FunVal(arg, typ1, typ2, expression, env)
		else raise brokenTypes 
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
	case (evalExp(a1, env)) of
		(BoolVal b) => 
			if b = true andalso checkTypes(getType(a2, env), getType(a3, env)) = true 
				then evalExp(a2, env) else evalExp(a3, env)
		| _ => raise brokenTypes
and
evalAppExp(var : exp, a: exp, env : environment) = 
	let 
		val temp = evalExp(var, env)
		val f = 
			case temp of StringVal w => envLookup(w, !funcs) 
				| _ => temp
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
						val res = ( evalExp(expression, envAdd(arg, evalExp(a, env), env_fun)) )
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
evalFunc(f : exp, env : environment) = 
	(case f of FnExp(VarExp arg, typ1, typ2, expression, env_temp) => 
		if checkTypes(getTypeFun(f, env_temp, []), BinType(typ1, typ2)) = true then StringVal "Function Definition"
		else raise brokenTypes
	| _ => raise brokenTypes)
and
evalProgram(arg, env) = 
	case arg of
		Program(p, b) =>
			(case p of Function(f) =>
				(case f of Fun(VarExp(var) , bound , typ1 , typ2 , expression, env_fun) =>
					( (funcs := envAdd(var, FunVal(bound , typ1 , typ2 , expression, envAdd(var, StringVal var, env)), !funcs) ;  
						evalFunc(FnExp(bound , typ1 , typ2 , expression, envAdd(var, StringVal var, env)), envAdd(var, StringVal var, env)) :: evalProgram(b, envAdd(var, FunVal(bound , typ1 , typ2 , expression, envAdd(var, StringVal var, env)), env)))  )
				| _ => raise brokenTypes)
			| Expression(e) =>
				evalExp(e, env) :: evalProgram(b, env))
		| Single(p) =>
			(case p of Function(f) =>
				(case f of Fun(VarExp(var) , bound , typ1 , typ2 , expression, env_fun) =>
					[(funcs := envAdd(var, FunVal(bound , typ1 , typ2 , expression, envAdd(var, StringVal var, env)), !funcs); evalFunc(FnExp(bound , typ1 , typ2 , expression, envAdd(var, StringVal var, env)), envAdd(var, StringVal var, env)))]
				| _ => raise brokenTypes)
			| Expression(e) =>
				[evalExp(e, env)] )
and
getRealExp(e) = 
	case e of Exp(a) => getRealExp(a)
		| _ => e

and
typtostr(t) = 
	case t of Int => "Int"
		| Bool => "Bool"
		| Type t => typtostr(t)
		| BinType(t1, t2) => "("^typtostr(t1)^")->("^typtostr(t2)^")"
and
getRealType(t) = 
	case t of Type t1 => getRealType(t1)
		| _ => t
fun evalResult([], ct) = ()
	| evalResult(x::l, ct) = 
		case x of
			BoolVal b1 => ( ( print("Statement "^Int.toString(ct)^": "); print ((Bool.toString b1)^"\n") ) ; (evalResult(l, ct+1)) )
			| IntVal n1 => ( (print("Statement "^Int.toString(ct)^": "); print ((Int.toString n1)^"\n") ) ; (evalResult(l, ct+1)) )
			| FunVal (VarExp(bound) , typ1 , typ2 , expression, env) =>  ((print("Statement "^Int.toString(ct)^": "); print ("Fn ("^bound^")\n")) ; (evalResult(l, ct+1)))
			| StringVal s1 => ((print("Statement "^Int.toString(ct)^": "); print (s1^"\n") ) ; (evalResult(l, ct+1)))
			| _ => raise brokenTypes

fun printResult(l : value list) = (print("\nResult:\n\n"); evalResult(l, 1); print("\n"))
end