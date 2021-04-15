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
		(print_final(a); print("AND AND, "); print_final(c); print ("formula => formula AND formula, "))
	else if b = AST.OR then
		(print_final(a); print("OR OR, "); print_final(c); print ("formula => formula OR formula, "))
	else if b = AST.XOR then
		(print_final(a); print("XOR XOR, "); print_final(c); print ("formula => formula XOR formula, "))
	else if b = AST.EQUALS then
		(print_final(a); print("EQUALS EQUALS, "); print_final(c); print ("formula => formula EQUALS formula, "))
	else
		(print_final(a); print("IMPLIES IMPLIES, "); print_final(c); print ("formula => formula IMPLIES formula, "))
	| print_final(AST.BinExp2(a, b, c)) = 
		(print("LPAREN (, "); print_final(b); print("RPAREN ), "); print ("formula => LPAREN formula RPAREN, "))
	| print_final(AST.UnExp(a, b)) = 
		(print("NOT NOT, "); print_final(b); print ("formula => NOT formula, "))
	| print_final(AST.TerExp(a, b, c, d, e, f)) = 
		(print("IF IF, "); print_final(b); print("THEN THEN, "); print_final(d); print("ELSE ELSE, "); print_final(f); print ("formula => IF formula THEN formula ELSE formula, "))
	| print_final(AST.CONST(a:string)) = (print("CONST "); print(a); print(", formula => CONST, "))
	| print_final(AST.ID(a:string)) = (print("ID "); print(a); print(", formula => ID, "))

fun print_statement(AST.Exp3(a, b)) = (print_final(a); print("TERM ;, "); print("statement => formula TERM, "))

fun temp(AST.Exp4(a, b)) = (temp(a); print_statement(b);  print("program => program statement, "))
	| temp(AST.Exp5(a)) = (print_statement(a); print("program => statement, "))

fun print_program(AST.Exp4(a, b)) = (temp(a); print_statement(b);  print("program => program statement"))
	| print_program(AST.Exp5(a)) = (print_statement(a); print("program => statement"))



