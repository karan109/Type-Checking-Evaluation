all:
	mlyacc exp.yacc
	mllex exp.lex
	mlton a3.mlb
run:
	./a3 input.txt
clean:
	rm exp.lex.sml
	rm exp.yacc.desc
	rm exp.yacc.sig
	rm exp.yacc.sml
	rm a3
