(* Load the external parser *)

//#r @"D:\projects\PhpAbsint\packages\Devsense.Php.Parser.1.2.14\lib\net45\Devsense.PHP.Parser.dll"
#r @"\\ssl.local\Users\Staff\danielef\Documents\phpabsint\packages\Devsense.Php.Parser.1.2.14\lib\net45\Devsense.PHP.Parser.dll"
open Devsense.PHP
open System.Text 

(* Load our code as usual, and open modules *)

#load @"../KphpLib.fs"
open PhpAbsint.KphpLib

#load @"../Interpreter.fs"
open PhpAbsint.Interpreter

(* Php file to run -> feel free to use your own! *)

//let file = @"D:\projects\PhpAbsint\PhpAbsint\tests\hello.php"
let file = @"\\ssl.local\Users\Staff\danielef\Documents\phpabsint\PhpAbsint\tests\hello.php"
// 
(* This is how to invoke the parser *)

let ast = Parsing.parseFile file

// How to get a constant name...
//((((ast.[0] :?> Syntax.Ast.ExpressionStmt).Expression :?> Syntax.Ast.ValueAssignEx).RValue :?> Syntax.Ast.GlobalConstUse).Name).Name.Value
//((((ast.[0] :?> Syntax.Ast.ExpressionStmt).Expression :?> Syntax.Ast.ValueAssignEx).RValue :?> Syntax.Ast.GlobalConstUse).Name)

(* And this is how to execute it *)

let s = Execution.runFile file

(* And now let's look at the state! *)
(* NB: for now, we only show the heap, as there isn't much else to show... *)

printfn "%A" <| PrettyPrint.prettyState s