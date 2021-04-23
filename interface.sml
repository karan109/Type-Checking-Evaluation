structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)

exception emptyFile;

fun head([]) = ""
    | head((x:string)::ls) = x

fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) =
                    print("Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end


fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end

fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
        else result
    end

val ast = 
    let 
        val str = (TextIO.inputAll (TextIO.openIn ( (*"input.txt"*)head(CommandLine.arguments()) )))
    in
        if str = "" then raise emptyFile
        else (parse o stringToLexer) (str)
    end;
print("\n\nAST: \n\n");
AST.programToString(ast, 0);
print("\n\n");
EVALUATOR.printResult(EVALUATOR.evalProgram(ast, []));