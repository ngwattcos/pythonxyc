open Ast
open Buffer
open Printf
open List

(* references *)
let indbuf = ref (Buffer.create 0)
let buf = ref (Buffer.create 0)

(* transform converts python-specific functions amd library calls *)
(* to a javascript-friendly format *)


(* Things to transform
 * array slice: arr[0, 3] -> arr.slice(0, 3)
 * 
 * str(exp) -> String(exp)
 * print(exp) -> console.log(exp)
 * import statements


 * for key in dict.keys() -> const key in dictionary

 Syntactically, we ony support this where arr is an array
 * for var in range(arr) -> for (const var = 0; var < arr.length; var++) {...}
 * for var in range(a, b) -> for (const var = a; var < b; var++) {...}
 * for var in range(a, b, c) -> for (const var = a; var < b; var += c) {...}

 *)
let rec transform_e = function
| Bexp(Aexp(FuncCallVal(Call((Var "len"), [Bexp(Aexp(VarAccess s))])))) -> Bexp(Aexp(VarAccess(Dot(s, "length"))))
| Bexp(Aexp(FuncCallVal(Call((Var "len"), [List l])))) -> Bexp(Aexp(VarAccess(DotRaw(List l, "length"))))
(* | Bexp(Aexp(FuncCallVal(Call((Var "len"), [Dict p ])))) -> Bexp(Aexp(VarAccess(DotRaw(Dict p, "length")))) *)
| e -> e

let rec transform_c = function
| FuncCallCom(Call(Var("print"), l)) -> FuncCallCom(Call(Var("console.log"), l)) 
| c -> c

and translate_coms (prog: program) = match prog with
| [] -> ()
| c::[] ->
    Buffer.add_buffer !buf !indbuf;
    translate_c c;
    Buffer.add_string !buf "\n"
| c::tl ->
    ignore (translate_coms tl);
    Buffer.add_buffer !buf !indbuf;
    ignore (translate_c c);
    Buffer.add_string !buf "\n"

and translate_c (c: com) = 
match transform_c c with
| ValUpdate (val_update) ->
    translate_val_update val_update;
    Buffer.add_string !buf ";"
| FuncDef (var, params_list, com_list) ->
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "const ";
    translate_var var;
    Buffer.add_string !buf " = (";
    translate_params params_list;
    Buffer.add_string !buf ") => {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms com_list;
    Buffer.truncate !indbuf ((Buffer.length !indbuf) - 4);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "}\n"
| FuncCallCom (func) ->
    translate_func_call func;
    Buffer.add_string !buf ";"
| ReturnExp (e) ->
    Buffer.add_string !buf "return ";
    translate_e e;
    Buffer.add_string !buf ";"
| Return ->
    Buffer.add_string !buf "return";
    Buffer.add_string !buf ";"
| Break ->
    Buffer.add_string !buf "break";
    Buffer.add_string !buf ";"
| Continue ->
    Buffer.add_string !buf "continue";
    Buffer.add_string !buf ";"
| _ -> failwith "unimplemented command"

and translate_val_update (c: val_update) = match c with
| JLet (var, exp) ->
    Buffer.add_string !buf "let ";
    translate_var var;
    Buffer.add_string !buf " = ";
    translate_e exp
| JConst (var, exp) ->
    Buffer.add_string !buf "const ";
    translate_var var;
    Buffer.add_string !buf " = ";
    translate_e exp
| Update (var_access, update_op, exp) ->
    translate_var_access var_access;
    translate_update_op update_op;
    translate_e exp

and translate_update_op (op: update_op) = match op with
| Equals ->
    Buffer.add_string !buf " = "
| PlusEquals ->
    Buffer.add_string !buf " += "
| MinusEquals ->
    Buffer.add_string !buf " -= "
| TimesEquals ->
    Buffer.add_string !buf " *= "
| DivideEquals ->
    Buffer.add_string !buf " /= "
| ModuloEquals ->
    Buffer.add_string !buf " %= "

and translate_e (exp: exp) =
let e = transform_e exp in match e with
| Bexp (bexp) -> translate_bexp bexp
| Stringexp (strexp) -> translate_stringexp strexp
| NoneExp -> Buffer.add_string !buf "null"
| List l -> 
    Buffer.add_string !buf "[";
    List.iter (fun exp -> translate_e exp;Buffer.add_string !buf ", ") l;
    Buffer.add_string !buf "]"
| Dict l ->
    Buffer.add_string !buf "{";
    List.iter (fun ((e1, e2): exp * exp) ->
        translate_e e1;
        Buffer.add_string !buf ": ";
        translate_e e2;
        Buffer.add_string !buf ", ") l;
    Buffer.add_string !buf "}"
| _ -> ()

and translate_stringexp (e: concat) = match e with
| String (str) -> Buffer.add_string !buf str
| Concat (concat, str) ->
    translate_stringexp concat;
    Buffer.add_string !buf " + ";
    Buffer.add_string !buf str

and translate_bexp (e: bexp) =
match e with
| Or (b1, b2) ->
    translate_bexp b1;
    Buffer.add_string !buf " || ";
    translate_bexp b2
| And (b1, b2) ->
    translate_bexp b1;
    Buffer.add_string !buf " && ";
    translate_bexp b2
| Not (b) ->
    Buffer.add_string !buf " !";
    translate_bexp b;
    Buffer.add_string !buf " "
| Bool (b) ->
    Buffer.add_string !buf (string_of_bool b)
| GT (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " > ";
    translate_aexp a2
| GE (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " >= ";
    translate_aexp a2
| LT (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " < ";
    translate_aexp a2
| LE (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " <= ";
    translate_aexp a2
| EQ (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " === ";
    translate_aexp a2
| NE (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " !== ";
    translate_aexp a2
| Aexp (a) -> translate_aexp a

and translate_aexp (e: aexp) =
match e with
| Int (i) -> Buffer.add_string !buf (string_of_int i)
| Float (f) -> Buffer.add_string !buf (string_of_float f)
| VarAccess (var_access) -> translate_var_access var_access
| FuncCallVal(func_call) -> translate_func_call func_call
| Paren (e) ->
    Buffer.add_string !buf "(";
    translate_e e;
    Buffer.add_string !buf ")"
| Expon (a1, a2) -> 
    translate_aexp a1;
    Buffer.add_string !buf " ** ";
    translate_aexp a2
| Times (a1, a2) -> 
    translate_aexp a1;
    Buffer.add_string !buf " * ";
    translate_aexp a2
| Div (a1, a2) -> 
    translate_aexp a1;
    Buffer.add_string !buf " / ";
    translate_aexp a2
| Plus (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " + ";
    translate_aexp a2
| Minus (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " - ";
    translate_aexp a2
| Mod (a1, a2) ->
    translate_aexp a1;
    Buffer.add_string !buf " % ";
    translate_aexp a2

and translate_func_call (Call (var_access, args): func_call) = 
    translate_var_access var_access;
    Buffer.add_string !buf "(";
    translate_args args;
    Buffer.add_string !buf ")"

and translate_var (v: var) = Buffer.add_string !buf v; ()

(* Expands a var_access in  "reversed" (correct) order *)
and translate_var_access (v: var_access) = 
match v with
| Var (var) -> translate_var var
| Dot (v1, var) ->
    translate_var_access v1;
    Buffer.add_string !buf ".";
    translate_var var
| Key (v1, exp) ->
    translate_var_access v1;
    Buffer.add_string !buf "[";
    translate_e exp;
    Buffer.add_string !buf "]"
| DotRaw (e, var) -> 
    translate_e e;
    Buffer.add_string !buf ".";
    translate_var var
| KeyRaw (l, e) -> 
    translate_e (List l);
    Buffer.add_string !buf "[";
    translate_e e;
    Buffer.add_string !buf "]"

(* expands an argument list in "reversed" (correct) order *)
and translate_args (lst: exp list) = match lst with
| [] -> ()
| arg::[] ->
    translate_e arg
| arg::tl ->
    translate_args tl;
    Buffer.add_string !buf ", ";
    translate_e arg

(* expands a parameter list in "reversed" (correct) order *)
and translate_params (lst: var list) = match lst with
| [] -> ()
| param::[] ->
    translate_var param
| param::tl ->
    translate_params tl;
    Buffer.add_string !buf ", ";
    translate_var param

let translate (prog: program) =
ignore (indbuf :=  Buffer.create 0);
ignore (buf :=  Buffer.create 0);
translate_coms prog;
!buf