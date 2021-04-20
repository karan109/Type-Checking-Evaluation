structure Tokens= Tokens
	
	type pos = int
	type col = int
	type exc = bool
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token  
	type lexresult = (svalue, pos) token

	val pos = ref 1
	val col = ref 0
	val exc = ref false
	val lex_result = ref [#"["]

	fun tail([]) = []
			| tail(x::ls) = ls
	
	fun append([], ls2) = ls2
  		| append(x::ls1, ls2) =  x::append(ls1, ls2)

  	fun rev([]) = []
  			| rev(x::ls) =  append(rev(ls), [x])
	
  	fun length([]) = 0
  			| length(x::ls) = 1+length(ls)

  	fun process([]) = ""
  		| process(ls) = 
	  		if (!exc) = false andalso length(ls) > 1 then String.implode( rev( #"\n"::(#"\n"::(#"]"::tail( tail( !lex_result) ) ) ) ) )
	  		else ""

	val eof = fn () => (print(process(!lex_result)); Tokens.EOF(!pos, !col))
	val error = fn (e, l:int, col:int) => print("Unknown Token:" ^ (Int.toString l) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n\n")

	
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
alphadigit = [A-Za-z0-9];
tilda = [~];
digit=[0-9];
ws = [\ \t];
all = .;
%%
\n       => (col := 0; pos := (!pos) + 1; lex());
{ws}+    => (col := (!col) + String.size(yytext); lex());
"TRUE"	=> (lex_result := append(rev(String.explode("BOOL \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.BOOL(true, !pos, !col));
"FALSE"	=> (lex_result := append(rev(String.explode("BOOL \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.BOOL(false, !pos, !col));
"XOR"	=> (lex_result := append(rev(String.explode("XOR \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.XOR(!pos,!col));
"AND"      => (lex_result := append(rev(String.explode("AND \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.AND(!pos,!col));
"OR"      => (lex_result := append(rev(String.explode("OR \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.OR(!pos,!col));
"NOT"      => (lex_result := append(rev(String.explode("NOT \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.NOT(!pos,!col));
"EQUALS"      => (lex_result := append(rev(String.explode("EQUALS \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.EQUALS(!pos,!col));
"IMPLIES"      => (lex_result := append(rev(String.explode("IMPLIES \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.IMPLIES(!pos,!col));
"PLUS"      => (lex_result := append(rev(String.explode("PLUS \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.PLUS(!pos,!col));
"MINUS"      => (lex_result := append(rev(String.explode("MINUS \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.MINUS(!pos,!col));
"TIMES"      => (lex_result := append(rev(String.explode("TIMES \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.TIMES(!pos,!col));
"NEGATE"      => (lex_result := append(rev(String.explode("NEGATE \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.NEGATE(!pos,!col));
"LESSTHAN"      => (lex_result := append(rev(String.explode("LESSTHAN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.LESSTHAN(!pos,!col));
"GREATERTHAN"      => (lex_result := append(rev(String.explode("GREATERTHAN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.GREATERTHAN(!pos,!col));
"if"      => (lex_result := append(rev(String.explode("if \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.IF(!pos,!col));
"then"      => (lex_result := append(rev(String.explode("then \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.THEN(!pos,!col));
"else"      => (lex_result := append(rev(String.explode("else \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ELSE(!pos,!col));
"fi"      => (lex_result := append(rev(String.explode("fi \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.FI(!pos,!col));
"let"      => (lex_result := append(rev(String.explode("let \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.LET(!pos,!col));
"in"      => (lex_result := append(rev(String.explode("in \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.IN(!pos,!col));
"end"      => (lex_result := append(rev(String.explode("end \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.END(!pos,!col));
"=>"      => (lex_result := append(rev(String.explode("DEFARROW \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.DEFARROW(!pos,!col));
"="      => (lex_result := append(rev(String.explode("ASSIGN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ASSIGN(!pos,!col));
"fun"      => (lex_result := append(rev(String.explode("FUN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.FUN(!pos,!col));
"fn"      => (lex_result := append(rev(String.explode("FN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.FN(!pos,!col));
":"      => (lex_result := append(rev(String.explode("COLON \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.COLON(!pos,!col));
"->"      => (lex_result := append(rev(String.explode("ARROW \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ARROW(!pos,!col));
"int"      => (lex_result := append(rev(String.explode("INTTYPE \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.INTTYPE(!pos,!col));
"bool"      => (lex_result := append(rev(String.explode("BOOLTYPE \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.BOOLTYPE(!pos,!col));
"("      => (lex_result := append(rev(String.explode("LPAREN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.LPAREN(!pos,!col));
")"      => (lex_result := append(rev(String.explode("RPAREN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.RPAREN(!pos,!col));
";"		=> (lex_result := append(rev(String.explode("TERM \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.TERM(!pos,!col));
{tilda}?{digit}+ => (lex_result := append(rev(String.explode("NUM \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.NUM(valOf (Int.fromString yytext),!pos,!col));
{alpha}{alphadigit}* => (lex_result := append(rev(String.explode("ID \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ID(yytext,!pos,!col));
{all}     => (exc := false; col := (!col) + String.size(yytext); error (yytext,!pos, !col); lex());