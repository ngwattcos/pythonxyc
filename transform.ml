open Ast
open Printf

(* translation converts python-specific functions amd library calls *)
(* to a javascript-friendly format *)

let rec print_p (prog: Ast.program) = ()

let rec print_c (c: Ast.com) = ()

let rec print_e (c: Ast.exp) = ()

(* let translatep (prog: Ast.program) = print_p prog; match prog with
| c::tl -> printf "a command"; ()
| [] -> printf "end"; ()

let rec translatec (c: Ast.com) = print_c c; match c with
| _ -> () *)


(* len(var) -> var.length (does not matter if string or arr) *)
(* array slice: arr[0, 3] -> arr.slice(0, 3) *)
let rec translate_e (e: Ast.exp) = print_e e; match e with
| _ -> ()


let transform_p (prog: Ast.program) = print_p prog; match prog with
| c::tl -> printf "a command"; ()
| [] -> printf "end"; ()

let rec transform_c (c: Ast.com) = print_c c; match c with
| _ -> ()


let rec transform_e (e: Ast.exp) = print_e e; match e with
| _ -> ()
