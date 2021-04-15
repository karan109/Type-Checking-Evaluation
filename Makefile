all:
	mlyacc formula.yacc
	mllex formula.lex
	mlton a3.mlb
run:
	./a3 input.txt
clean:
	rm formula.lex.sml
	rm formula.yacc.desc
	rm formula.yacc.sig
	rm formula.yacc.sml
	rm a3
