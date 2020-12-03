open Ast
open Buffer
open Printf
open List

(* translation converts python-specific functions amd library calls *)
(* to a javascript-friendly format *)

let rec print_p (prog: Ast.program) = ()

let rec print_c (c: Ast.com) = ()

let rec print_e (c: Ast.exp) = ()

let rec reverse (lst: 'a list) (acc: 'a list) = match lst with
| h::tl -> reverse tl (h::acc)
| [] -> acc

let reverse_list (lst: 'a list) = reverse lst []

let reverse_var_access (va: Ast.var_access) (acc: Ast.var_access) = match va with
| Var (var) -> failwith "unimplemented"
| Dot (v1, var) -> failwith "unimplemented"
| Key (v1, exp) -> failwith "unimplemented"

(* let translatep (prog: Ast.program) = print_p prog; match prog with
| c::tl -> printf "a command"; ()
| [] -> printf "end"; ()

let rec translatec (c: Ast.com) = print_c c; match c with
| _ -> () *)


(* len(var) -> var.length (does not matter if string or arr) *)
(* array slice: arr[0, 3] -> arr.slice(0, 3) *)
let rec translate_e (e: Ast.exp) = print_e e; match e with
| e -> e


let rec transform_p (prog: Ast.program) (buf: Buffer.t) = match prog with
| c::tl ->
    ignore (transform_p tl buf);
    Buffer.add_string buf "\n";
    ignore (transform_c c buf);
    buf
| [] -> buf

and transform_c (c: Ast.com) (buf: Buffer.t) = print_c c; match c with
| ValUpdate (val_update) -> transform_val_update val_update buf
| FuncCallCom (func) -> transform_func_call_com func buf
| Continue -> Buffer.add_string buf "continue"
| _ -> failwith "unimplemented"

and transform_val_update (c: Ast.val_update) (buf: Buffer.t)= match c with
| JLet (var, exp) ->
    Buffer.add_string buf "let ";
    transform_var var buf;
    Buffer.add_string buf " = ";
    transform_e exp buf
| JConst (var, exp) ->
    Buffer.add_string buf "const ";
    transform_var var buf;
    Buffer.add_string buf " = ";
    transform_e exp buf
| Update (var_access, update_op, exp) -> failwith "unimplemented"

and transform_func_call_com (c: Ast.func_call) (buf: Buffer.t) =
match c with
| Call (var_access, args) ->
    (transform_var_access var_access buf);
    (transform_args (reverse_list args) buf)

and transform_e (e: Ast.exp) (buf: Buffer.t) =
let translated = translate_e in match translated with
| _ -> ()

and transform_var (v: Ast.var) (buf: Buffer.t) = Buffer.add_string buf v

(* Expands a var_access in  "reversed" (correct) order *)
and transform_var_access (v: Ast.var_access) (buf: Buffer.t) = match v with
| Var (var) -> transform_var var buf
| Dot (v1, var) ->
   transform_var_access v1 buf;
    transform_var var buf
| Key (v1, exp) ->
    transform_var_access v1 buf;
    transform_e exp buf

(* expands an argument list in "reversed" (correct) order *)
and transform_args (lst: Ast.exp list) (buf: Buffer.t) = match lst with
| arg::tl ->
    transform_args tl buf;
    transform_e arg buf
| [] -> ()


let transform (prog: Ast.program) = transform_p prog (Buffer.create 0)