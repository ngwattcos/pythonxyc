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
| c::tl -> transform_p tl (transform_c c buf)
| [] -> printf "end"; ()

and transform_c (c: Ast.com) (buf: Buffer.t): Buffer.t = print_c c; match c with
| ValUpdate (val_update) -> transform_val_update val_update buf
| FuncCallCom (func) -> transform_func_call_com func buf
| _ -> failwith "unimplemented"

and transform_val_update (c: Ast.val_update) (buf: Buffer.t): Buffer.t = match c with
| JLet (var, exp) ->
    Buffer.add_buffer buf (transform_var var buf);
    Buffer.add_buffer buf (transform_e exp buf);
    buf
| JConst (var, exp) ->
    Buffer.add_buffer buf (transform_var var buf);
    Buffer.add_buffer buf (transform_e exp buf);
    buf
| Update (var_access, update_op, exp) -> failwith "unimplemented"

and transform_func_call_com (c: Ast.func_call) (buf: Buffer.t): Buffer.t =
match c with
| Call (var_access, args) ->
    Buffer.add_buffer buf (transform_var_access var_access buf);
    buf

and transform_e (e: Ast.exp) (buf: Buffer.t): Buffer.t =
let translated = translate_e in match translated with
| _ -> buf

and transform_var (v: Ast.var) (buf: Buffer.t): Buffer.t = Buffer.add_string buf v; buf

(* Expands a var_access in  "reversed" (correct) order *)
and transform_var_access (v: Ast.var_access) (buf: Buffer.t): Buffer.t = match v with
| Var (var) -> transform_var var buf
| Dot (v1, var) ->
    ignore (transform_var_access v1 buf);
    Buffer.add_buffer buf (transform_var var buf);
    buf
| Key (v1, exp) ->
    ignore (transform_var_access v1 buf);
    Buffer.add_buffer buf (transform_e exp buf);
    buf

and transform_args (lst: Ast.exp list) (buf: Buffer.t): Buffer.t = match lst with
| arg::tl ->
    ignore (transform_args tl buf);
    Buffer.add_buffer buf (transform_e arg buf);
    buf
| [] -> buf


let transform (prog: Ast.program) = transform_p prog (Buffer.create 0)