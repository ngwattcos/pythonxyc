open Ast
open Buffer
open Printf
open List

(* references *)
let indbuf = ref (Buffer.create 0)

(* translation converts python-specific functions amd library calls *)
(* to a javascript-friendly format *)

let rec print_p (prog: program) = ()

let rec print_c (c: com) = ()

let rec print_e (c: exp) = ()

(* let rec reverse (lst: 'a list) (acc: 'a list) = match lst with
| h::tl -> reverse tl (h::acc)
| [] -> acc

let reverse_list (lst: 'a list) = reverse lst [] *)

(* let translatep (prog: program) = print_p prog; match prog with
| c::tl -> printf "a command"; ()
| [] -> printf "end"; ()

let rec translatec (c: com) = print_c c; match c with
| _ -> () *)


(* len(var) -> var.length (does not matter if string or arr) *)
(* array slice: arr[0, 3] -> arr.slice(0, 3) *)
let rec translate_e (e: exp) = print_e e; match e with
| e -> e


let rec transform_p (prog: program) (buf: Buffer.t) =
transform_coms prog buf;
buf

and transform_coms (prog: program) (buf: Buffer.t) = match prog with
| [] -> ()
| c::[] ->
    Buffer.add_buffer buf !indbuf;
    transform_c c buf;
    Buffer.add_string buf "\n"
| c::tl ->
    ignore (transform_coms tl buf);
    Buffer.add_buffer buf !indbuf;
    ignore (transform_c c buf);
    Buffer.add_string buf "\n"

and transform_c (c: com) (buf: Buffer.t) = match c with
| ValUpdate (val_update) ->
    transform_val_update val_update buf;
    Buffer.add_string buf ";"
| FuncDef (var, params_list, com_list) ->
    Buffer.add_buffer buf !indbuf;
    Buffer.add_string buf "const ";
    transform_var var buf;
    Buffer.add_string buf " = ";
    Buffer.add_string buf "(";
    transform_params params_list buf;
    Buffer.add_string buf ")";
    Buffer.add_string buf " => ";
    Buffer.add_string buf "{";
    Buffer.add_string buf "\n";
    Buffer.add_string !indbuf "    ";
    transform_coms com_list buf;
    Buffer.truncate !indbuf 4;
    Buffer.add_string buf "\n";
    Buffer.add_buffer buf !indbuf;
    Buffer.add_string buf "}";
| FuncCallCom (func) ->
    transform_func_call func buf;
    Buffer.add_string buf ";"
| ReturnExp (e) ->
    Buffer.add_string buf "return ";
    transform_e e buf;
    Buffer.add_string buf ";"
| Return ->
    Buffer.add_string buf "return";
    Buffer.add_string buf ";"
| Break ->
    Buffer.add_string buf "break";
    Buffer.add_string buf ";"

| Continue ->
    Buffer.add_string buf "continue";
    Buffer.add_string buf ";"
| _ -> failwith "unimplemented command"

and transform_val_update (c: val_update) (buf: Buffer.t) = match c with
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
| Update (var_access, update_op, exp) ->
    transform_var_access var_access buf;
    transform_update_op update_op buf;
    transform_e exp buf

and transform_update_op (op: update_op) (buf: Buffer.t) = match op with
| Equals ->
    Buffer.add_string buf " = "
| PlusEquals ->
    Buffer.add_string buf " += "
| MinusEquals ->
    Buffer.add_string buf " -= "
| TimesEquals ->
    Buffer.add_string buf " *= "
| DivideEquals ->
    Buffer.add_string buf " /= "
| ModuloEquals ->
    Buffer.add_string buf " %= "

and transform_e (exp: exp) (buf: Buffer.t) =
let e = translate_e exp in match e with
| Bexp (bexp) -> transform_bexp bexp buf
| String (str) -> Buffer.add_string buf str
| _ -> ()

and transform_bexp (e: bexp) (buf: Buffer.t) =
match e with
| Or (b1, b2) ->
    transform_bexp b1 buf;
    Buffer.add_string buf " || ";
    transform_bexp b2 buf
| And (b1, b2) ->
    transform_bexp b1 buf;
    Buffer.add_string buf " && ";
    transform_bexp b2 buf
| Not (b) ->
    Buffer.add_string buf " !";
    transform_bexp b buf;
    Buffer.add_string buf " "
| Bool (b) ->
    Buffer.add_string buf (string_of_bool b)
| GT (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " > ";
    transform_aexp a2 buf
| GE (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " >= ";
    transform_aexp a2 buf
| LT (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " < ";
    transform_aexp a2 buf
| LE (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " <= ";
    transform_aexp a2 buf
| EQ (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " === ";
    transform_aexp a2 buf
| NE (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " !== ";
    transform_aexp a2 buf
| Aexp (a) -> transform_aexp a buf

and transform_aexp (e: aexp) (buf: Buffer.t) =
match e with
| Int (i) -> Buffer.add_string buf (string_of_int i)
| Float (f) -> Buffer.add_string buf (string_of_float f)
| VarAccess (var_access) -> transform_var_access var_access buf
| FuncCallVal(func_call) -> transform_func_call func_call buf
| Paren (e) ->
    Buffer.add_string buf "(";
    transform_e e buf;
    Buffer.add_string buf ")"
| Expon (a1, a2) -> 
    transform_aexp a1 buf;
    Buffer.add_string buf " ** ";
    transform_aexp a2 buf
| Times (a1, a2) -> 
    transform_aexp a1 buf;
    Buffer.add_string buf " * ";
    transform_aexp a2 buf
| Div (a1, a2) -> 
    transform_aexp a1 buf;
    Buffer.add_string buf " / ";
    transform_aexp a2 buf
| Plus (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " + ";
    transform_aexp a2 buf
| Minus (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " - ";
    transform_aexp a2 buf
| Mod (a1, a2) ->
    transform_aexp a1 buf;
    Buffer.add_string buf " % ";
    transform_aexp a2 buf

and transform_func_call (fc: func_call) (buf: Buffer.t) = match fc with
| Call (var_access, args) ->
    transform_var_access var_access buf;
    Buffer.add_string buf "(";
    transform_args args buf;
    Buffer.add_string buf ")"

and transform_var (v: var) (buf: Buffer.t) = Buffer.add_string buf v

(* Expands a var_access in  "reversed" (correct) order *)
and transform_var_access (v: var_access) (buf: Buffer.t) = match v with
| Var (var) -> transform_var var buf
| Dot (v1, var) ->
    transform_var_access v1 buf;
    Buffer.add_string buf ".";
    transform_var var buf
| Key (v1, exp) ->
    transform_var_access v1 buf;
    Buffer.add_string buf "[";
    transform_e exp buf;
    Buffer.add_string buf "]"

(* expands an argument list in "reversed" (correct) order *)
and transform_args (lst: exp list) (buf: Buffer.t) = match lst with
| [] -> ()
| arg::[] ->
    transform_e arg buf
| arg::tl ->
    transform_args tl buf;
    Buffer.add_string buf ", ";
    transform_e arg buf

(* expands a parameter list in "reversed" (correct) order *)
and transform_params (lst: var list) (buf: Buffer.t) = match lst with
| [] -> ()
| param::[] ->
    transform_var param buf
| param::tl ->
    transform_params tl buf;
    Buffer.add_string buf ", ";
    transform_var param buf

let transform (prog: program) =
ignore (indbuf :=  Buffer.create 0);
transform_p prog (Buffer.create 0)