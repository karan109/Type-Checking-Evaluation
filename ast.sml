structure AST =
struct
datatype exp = CONST of string
| ID of string
| BinExp of exp * binop * exp
| BinExp2 of binop2 * exp * binop2
| UnExp of unop * exp
| TerExp of terop * exp * terop * exp * terop * exp
and binop = IMPLIES | AND | OR | XOR | EQUALS
and binop2 = LPAREN | RPAREN
and unop = NOT
and terop = IF | THEN | ELSE
datatype statement = Exp3 of exp * term
and term = TERM
datatype program = Exp4 of program * statement
| Exp5 of statement
end

fun print_final(AST.BinExp(a, b, c)) = 
	if b = AST.AND then
		(print_final(a); print("AND AND, "); print_final(c); print ("exp => exp AND exp, "))
	else if b = AST.OR then
		(print_final(a); print("OR OR, "); print_final(c); print ("exp => exp OR exp, "))
	else if b = AST.XOR then
		(print_final(a); print("XOR XOR, "); print_final(c); print ("exp => exp XOR exp, "))
	else if b = AST.EQUALS then
		(print_final(a); print("EQUALS EQUALS, "); print_final(c); print ("exp => exp EQUALS exp, "))
	else
		(print_final(a); print("IMPLIES IMPLIES, "); print_final(c); print ("exp => exp IMPLIES exp, "))
	| print_final(AST.BinExp2(a, b, c)) = 
		(print("LPAREN (, "); print_final(b); print("RPAREN ), "); print ("exp => LPAREN exp RPAREN, "))
	| print_final(AST.UnExp(a, b)) = 
		(print("NOT NOT, "); print_final(b); print ("exp => NOT exp, "))
	| print_final(AST.TerExp(a, b, c, d, e, f)) = 
		(print("if if, "); print_final(b); print("then then, "); print_final(d); print("else else, "); print_final(f); print("fi fi, "); print ("exp => if exp then exp else exp fi, "))
	| print_final(AST.CONST(a:string)) = (print("CONST "); print(a); print(", exp => CONST, "))
	| print_final(AST.ID(a:string)) = (print("ID "); print(a); print(", exp => ID, "))

fun print_statement(AST.Exp3(a, b)) = (print_final(a); print("TERM ;, "); print("statement => exp TERM, "))

fun temp(AST.Exp4(a, b)) = (temp(a); print_statement(b);  print("program => program statement, "))
	| temp(AST.Exp5(a)) = (print_statement(a); print("program => statement, "))

fun print_program(AST.Exp4(a, b)) = (temp(a); print_statement(b);  print("program => program statement"))
	| print_program(AST.Exp5(a)) = (print_statement(a); print("program => statement"))



