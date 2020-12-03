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

(* Things to fix
 * array slice: arr[0, 3] -> arr.slice(0, 3)
 * 
 * str(exp) -> exp.toString()
 * print(exp) -> console.log(exp)
 * import statements


 * for key in dict.keys() -> const key in dictionary

 Syntactically, we ony support this where arr is an array
 * for var in range(arr) -> for (const var = 0; var < arr.length; var++) {...}
 * for var in range(a, b) -> for (const var = a; var < b; var++) {...}
 * for var in range(a, b, c) -> for (const var = a; var < b; var += c) {...}

 *)
let rec transform_e (e: exp) = print_e e; 
match e with
| Bexp(Aexp(FuncCallVal(Call((Var "len"), [Bexp(Aexp(VarAccess s))])))) -> Bexp(Aexp(VarAccess(Dot(s, "length"))))
| Bexp(Aexp(FuncCallVal(Call((Var "len"), [List l])))) -> Bexp(Aexp(VarAccess(DotRaw(List l, "length"))))
(* | Bexp(Aexp(FuncCallVal(Call((Var "len"), [Dict p ])))) -> Bexp(Aexp(VarAccess(DotRaw(Dict p, "length")))) *)
| e -> e

let rec translate_p (prog: program) (buf: Buffer.t) =
translate_coms prog buf;
buf

and translate_coms (prog: program) (buf: Buffer.t) = match prog with
| [] -> ()
| c::[] ->
    Buffer.add_buffer buf !indbuf;
    translate_c c buf;
    Buffer.add_string buf "\n"
| c::tl ->
    ignore (translate_coms tl buf);
    Buffer.add_buffer buf !indbuf;
    ignore (translate_c c buf);
    Buffer.add_string buf "\n"

and translate_c (c: com) (buf: Buffer.t) = match c with
| ValUpdate (val_update) ->
    translate_val_update val_update buf;
    Buffer.add_string buf ";"
| FuncDef (var, params_list, com_list) ->
    Buffer.add_buffer buf !indbuf;
    Buffer.add_string buf "const ";
    translate_var var buf;
    Buffer.add_string buf " = (";
    translate_params params_list buf;
    Buffer.add_string buf ") => {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms com_list buf;
    Buffer.truncate !indbuf ((Buffer.length !indbuf) - 4);
    Buffer.add_buffer buf !indbuf;
    Buffer.add_string buf "}\n"
| FuncCallCom (func) ->
    translate_func_call func buf;
    Buffer.add_string buf ";"
| ReturnExp (e) ->
    Buffer.add_string buf "return ";
    translate_e e buf;
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

and translate_val_update (c: val_update) (buf: Buffer.t) = match c with
| JLet (var, exp) ->
    Buffer.add_string buf "let ";
    translate_var var buf;
    Buffer.add_string buf " = ";
    translate_e exp buf
| JConst (var, exp) ->
    Buffer.add_string buf "const ";
    translate_var var buf;
    Buffer.add_string buf " = ";
    translate_e exp buf
| Update (var_access, update_op, exp) ->
    translate_var_access var_access buf;
    translate_update_op update_op buf;
    translate_e exp buf

and translate_update_op (op: update_op) (buf: Buffer.t) = match op with
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

and translate_e (exp: exp) (buf: Buffer.t) =
let e = transform_e exp in match e with
| Bexp (bexp) -> translate_bexp bexp buf
| String (str) -> Buffer.add_string buf str
| NoneExp -> Buffer.add_string buf "null"
| List l -> 
    Buffer.add_string buf "[";
    List.iter (fun exp -> translate_e exp buf; Buffer.add_string buf ", ") l;
    Buffer.add_string buf "]"
| Dict l ->
    Buffer.add_string buf "{";
    List.iter (fun ((e1, e2): exp * exp) -> translate_e e1 buf; Buffer.add_string buf ": "; translate_e e2 buf; Buffer.add_string buf ", ") l;
    Buffer.add_string buf "}"
| _ -> ()

and translate_bexp (e: bexp) (buf: Buffer.t) =
match e with
| Or (b1, b2) ->
    translate_bexp b1 buf;
    Buffer.add_string buf " || ";
    translate_bexp b2 buf
| And (b1, b2) ->
    translate_bexp b1 buf;
    Buffer.add_string buf " && ";
    translate_bexp b2 buf
| Not (b) ->
    Buffer.add_string buf " !";
    translate_bexp b buf;
    Buffer.add_string buf " "
| Bool (b) ->
    Buffer.add_string buf (string_of_bool b)
| GT (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " > ";
    translate_aexp a2 buf
| GE (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " >= ";
    translate_aexp a2 buf
| LT (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " < ";
    translate_aexp a2 buf
| LE (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " <= ";
    translate_aexp a2 buf
| EQ (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " === ";
    translate_aexp a2 buf
| NE (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " !== ";
    translate_aexp a2 buf
| Aexp (a) -> translate_aexp a buf

and translate_aexp (e: aexp) (buf: Buffer.t) =
match e with
| Int (i) -> Buffer.add_string buf (string_of_int i)
| Float (f) -> Buffer.add_string buf (string_of_float f)
| VarAccess (var_access) -> translate_var_access var_access buf
| FuncCallVal(func_call) -> translate_func_call func_call buf
| Paren (e) ->
    Buffer.add_string buf "(";
    translate_e e buf;
    Buffer.add_string buf ")"
| Expon (a1, a2) -> 
    translate_aexp a1 buf;
    Buffer.add_string buf " ** ";
    translate_aexp a2 buf
| Times (a1, a2) -> 
    translate_aexp a1 buf;
    Buffer.add_string buf " * ";
    translate_aexp a2 buf
| Div (a1, a2) -> 
    translate_aexp a1 buf;
    Buffer.add_string buf " / ";
    translate_aexp a2 buf
| Plus (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " + ";
    translate_aexp a2 buf
| Minus (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " - ";
    translate_aexp a2 buf
| Mod (a1, a2) ->
    translate_aexp a1 buf;
    Buffer.add_string buf " % ";
    translate_aexp a2 buf

and translate_func_call (Call (var_access, args): func_call) (buf: Buffer.t) = 
    translate_var_access var_access buf;
    Buffer.add_string buf "(";
    translate_args args buf;
    Buffer.add_string buf ")"

and translate_var (v: var) (buf: Buffer.t) = Buffer.add_string buf v

(* Expands a var_access in  "reversed" (correct) order *)
and translate_var_access (v: var_access) (buf: Buffer.t) = 
match v with
| Var (var) -> translate_var var buf
| Dot (v1, var) ->
    translate_var_access v1 buf;
    Buffer.add_string buf ".";
    translate_var var buf
| Key (v1, exp) ->
    translate_var_access v1 buf;
    Buffer.add_string buf "[";
    translate_e exp buf;
    Buffer.add_string buf "]"
| DotRaw (e, var) -> 
    translate_e e buf;
    Buffer.add_string buf ".";
    translate_var var buf
| KeyRaw (l, e) -> 
    translate_e (List l) buf;
    Buffer.add_string buf "[";
    translate_e e buf;
    Buffer.add_string buf "]"

(* expands an argument list in "reversed" (correct) order *)
and translate_args (lst: exp list) (buf: Buffer.t) = match lst with
| [] -> ()
| arg::[] ->
    translate_e arg buf
| arg::tl ->
    translate_args tl buf;
    Buffer.add_string buf ", ";
    translate_e arg buf

(* expands a parameter list in "reversed" (correct) order *)
and translate_params (lst: var list) (buf: Buffer.t) = match lst with
| [] -> ()
| param::[] ->
    translate_var param buf
| param::tl ->
    translate_params tl buf;
    Buffer.add_string buf ", ";
    translate_var param buf

let translate (prog: program) =
ignore (indbuf :=  Buffer.create 0);
translate_p prog (Buffer.create 0)