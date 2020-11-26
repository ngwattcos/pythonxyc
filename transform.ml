open Ast
open Printf

let rec printp (prog: Ast.program) = ()

let rec printc (c: Ast.com) = ()

let transformp (prog: Ast.program) = printp prog; match prog with
| c::tl -> printf "a command"; ()
| [] -> printf "end"; ()

let rec transformc (c: Ast.com) = printc c; match c with
| _ -> ()