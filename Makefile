all:
	mlyacc exp.yacc
	mllex exp.lex
	mlton a3.mlb

interpret:
	ml-yacc exp.yacc
	ml-lex exp.lex
	sml loader.sml input.txt

run:
	./a3 input.txt
clean:
	rm exp.lex.sml
	rm exp.yacc.desc
	rm exp.yacc.sig
	rm exp.yacc.sml
	rm a3
