CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "exp.yacc.sig";
use "exp.yacc.sml";
use "exp.lex.sml";
use "interface.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)