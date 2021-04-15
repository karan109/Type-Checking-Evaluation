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
ws = [\ \t];
all = .;
%%
\n       => (col := 0; pos := (!pos) + 1; lex());
{ws}+    => (col := (!col) + String.size(yytext); lex());
"TRUE"	=> (lex_result := append(rev(String.explode("CONST \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.CONST("TRUE", !pos, !col));
"FALSE"	=> (lex_result := append(rev(String.explode("CONST \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.CONST("FALSE", !pos, !col));
"XOR"	=> (lex_result := append(rev(String.explode("XOR \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.XOR(!pos,!col));
"AND"      => (lex_result := append(rev(String.explode("AND \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.AND(!pos,!col));
"OR"      => (lex_result := append(rev(String.explode("OR \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.OR(!pos,!col));
"NOT"      => (lex_result := append(rev(String.explode("NOT \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.NOT(!pos,!col));
"EQUALS"      => (lex_result := append(rev(String.explode("EQUALS \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.EQUALS(!pos,!col));
"IMPLIES"      => (lex_result := append(rev(String.explode("IMPLIES \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.IMPLIES(!pos,!col));
"if"      => (lex_result := append(rev(String.explode("if \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.IF(!pos,!col));
"then"      => (lex_result := append(rev(String.explode("then \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.THEN(!pos,!col));
"else"      => (lex_result := append(rev(String.explode("else \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ELSE(!pos,!col));
"fi"      => (lex_result := append(rev(String.explode("fi \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.FI(!pos,!col));
"("      => (lex_result := append(rev(String.explode("LPAREN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.LPAREN(!pos,!col));
")"      => (lex_result := append(rev(String.explode("RPAREN \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.RPAREN(!pos,!col));
";"		=> (lex_result := append(rev(String.explode("TERM \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.TERM(!pos,!col));
{alpha}+ => (lex_result := append(rev(String.explode("ID \""^yytext^"\", ")), (!lex_result)); col := (!col) + String.size(yytext); Tokens.ID(yytext,!pos,!col));
{all}     => (exc := false; col := (!col) + String.size(yytext); error (yytext,!pos, !col); lex());

