open Ast
open Buffer
open Printf
open List

(* references *)
let indbuf = ref (Buffer.create 0)
let buf = ref (Buffer.create 0)


let undent buffer = ((Buffer.length buffer) - 4)

(* Re-arranging Python AST expressions to a JavaScript-like format *)

(* Things left to transform

The difficulty is that map() and filter() returns an iterator
 * list(map(lambda (x1...xn) : e1, e2)) -> e2.map((x1...xn) => e1)
 * list(filter(lambda (x1...xn) : e1, e2)) -> e2.filter((x1...xn) => e1)

 Reduce on the other hand is simipler:
 *  reduce(lambda a, b : e1, e2) -> e1.reduce((a, b) => e2)
 *)
let rec transform_e (e: exp) = match e with
| e -> e

and tranform_var_access = function
(* handles len(var), len([]) *)
| FuncCallVal(Call((Var "len"), [Bexp(Aexp(VarAccess s))])) -> Dot(s, "length")
(* handles str(exp) -> String(exp) *)
| FuncCallVal(Call((Var "str"), [e])) -> FuncCallVal(Call((Var "String"), [e]))
(* recursively transform array slices *)
| Slice (v1, e1, e2) ->
    let v = tranform_var_access (v1) in
    FuncCallVal(Call(Dot(v, "slice"), [e2; e1]))
| v -> v


(* Re-arranging Python AST commands to a JavaScript-like format *)
(* Things to transform
* print(exp) -> console.log(exp)
* print() -> console.log()
 * import commands

 * for key in dict.keys() -> const key in dictionary

 Syntactically, we ony support this where arr is an array
 * for var in range(arr) -> for (let var = 0; var < arr.length; var++) {...}
 * for var in range(a, b) -> for (let var = a; var < b; var++) {...}
 * for var in range(a, b, c) -> for (let var = a; var < b; var += c) {...}

 *)
let rec transform_c = function
| FuncCallCom(Call(Var("print"), l)) -> FuncCallCom(Call(Var("console.log"), l))
| c -> c

and transform_for_com = function
| ForIterExp (var,
    Bexp(Aexp(VarAccess(FuncCallVal(Call(Var("range"), [c; Bexp(Aexp(Int(b))); a]))))), coms) ->
    let initial = ValUpdate(JLet(var, a)) in
    let terminal = Bexp(LT((VarAccess(Var(var))), Int(b))) in
    let update = ValUpdate(Update(Var(var), PlusEquals, c)) in
    ForJS(initial, terminal, update, coms)
| ForIterExp (var,
    Bexp(Aexp(VarAccess(FuncCallVal(Call(Var("range"), [Bexp(Aexp(Int(b))); a]))))), coms) ->
    let initial = ValUpdate(JLet(var, a)) in
    let terminal = Bexp(LT((VarAccess(Var(var))), Int(b))) in
    let update = ValUpdate(Update(Var(var), PlusEquals, Bexp(Aexp(Int(1))))) in
    ForJS(initial, terminal, update, coms)
| ForIterExp (var,
    Bexp(Aexp(VarAccess(FuncCallVal(Call(Var("range"), [Bexp(Aexp(Int(b)))]))))), coms) ->
    let initial = ValUpdate(JLet(var, Bexp(Aexp(Int(0))))) in
    let terminal = Bexp(LT((VarAccess(Var(var))), Int(b))) in
    let update = ValUpdate(Update(Var(var), PlusEquals, Bexp(Aexp(Int(1))))) in
    ForJS(initial, terminal, update, coms)
| ForIterExp (var, Bexp(Aexp(VarAccess(FuncCallVal(Call(Dot(Var(v), "keys"), []))))), coms) ->
    ForIterJS (var, v, coms)
| for_com -> for_com

(* Translations from Python AST to JavaScript/JSX. *)

let rec translate_coms (prog: program) = match prog with
| [] -> ()
| c::[] ->
    Buffer.add_buffer !buf !indbuf;
    translate_c c;
    Buffer.add_string !buf "\n"
| c::tl ->
    translate_coms tl;
    Buffer.add_buffer !buf !indbuf;
    translate_c c;
    Buffer.add_string !buf "\n"

and translate_c (c: com) =
translate_c_nosemi c;
match c with
| ValUpdate (val_update) ->
    Buffer.add_string !buf ";"
| FuncDef (var, params_list, com_list) -> ()
| FuncCallCom (func) ->
    Buffer.add_string !buf ";"
| While (e, coms) -> ()
| If if_com -> ()
| For for_com -> ()
| ReturnExp (e) ->
    Buffer.add_string !buf ";"
| Return ->
    Buffer.add_string !buf ";"
| Break ->
    Buffer.add_string !buf ";"
| Continue ->
    Buffer.add_string !buf ";"
| Import import ->
    Buffer.add_string !buf ";"
| Export export ->
    Buffer.add_string !buf ";"
| _ -> failwith "trying to add semicolon to unimplemented command"

and translate_c_nosemi (c: com) = 
match transform_c c with
| ValUpdate (val_update) ->
    translate_val_update val_update;
| FuncDef (var, params_list, com_list) -> translate_func_def var params_list com_list
| FuncCallCom (func) ->
    translate_func_call func;
| While (e, coms) -> translate_while e coms
| If if_com -> translate_if_com if_com
| For for_com -> translate_for_com for_com
| ReturnExp (e) ->
    Buffer.add_string !buf "return ";
    translate_e e;
| Return ->
    Buffer.add_string !buf "return";
| Break ->
    Buffer.add_string !buf "break";
| Continue ->
    Buffer.add_string !buf "continue";
| Import import -> translate_import import
| Export export -> translate_export export
| _ -> failwith "unimplemented command"

and translate_import import = match import with
| ImportDefault (v, str) ->
    Buffer.add_string !buf "import ";
    Buffer.add_string !buf v;
    Buffer.add_string !buf " from ";
    translate_string str;
| ImportFromString (lst, str) ->
    Buffer.add_string !buf "import { ";
    translate_names_list lst;
    Buffer.add_string !buf " } from ";
    translate_string str

and translate_export export = match export with
| ExportDefault s ->
    Buffer.add_string !buf "export default ";
    Buffer.add_string !buf s;
| ExportList lst ->
    Buffer.add_string !buf "export { ";
    translate_names_list lst;
    Buffer.add_string !buf " }";

and translate_names_list lst = match lst with
| [] -> ()
| h::[] ->
    Buffer.add_string !buf h
| h::t ->
    translate_names_list t;
    Buffer.add_string !buf ", ";
    Buffer.add_string !buf h

and translate_for_com for_com =
match (transform_for_com for_com) with
| ForJS (initial, terminal, update, coms) ->
    Buffer.add_string !buf "for (";
    translate_c initial;
    Buffer.add_string !buf " ";
    translate_e terminal;
    Buffer.add_string !buf "; ";
    translate_c_nosemi update;
    Buffer.add_string !buf ") {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "}";
| ForIterJS (var1, var2, coms) ->
    Buffer.add_string !buf "for (";
    translate_var var1;
    Buffer.add_string !buf " in ";
    translate_var var2;
    Buffer.add_string !buf ") {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "}";
| _ -> failwith "incorrectly translated for command"

and translate_while e coms =
    Buffer.add_string !buf "while (";
    translate_e e;
    Buffer.add_string !buf ") {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "}"

and translate_if_com ic = match ic with
| IfNoElse if_elifs -> translate_elifs if_elifs
| IfElse (if_elifs, el) ->
    translate_elifs if_elifs;
    translate_else el
    

and translate_elifs elifs = match elifs with
| IfBase ib -> translate_if ib
| IfElifs (ie, e) ->
    translate_elifs ie;
    translate_elif e

and translate_else coms =
    Buffer.add_string !buf " else {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf ("}")

and translate_elif e = match e with
| (e, coms) ->
    Buffer.add_string !buf " else if (";
    translate_e e;
    Buffer.add_string !buf ") {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf ("}")

and translate_if i = match i with
| (e, coms) ->
    Buffer.add_string !buf "if (";
    translate_e e;
    Buffer.add_string !buf ") {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms coms;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf ("}")

and translate_func_def var params_list com_list =
    Buffer.add_string !buf "const ";
    translate_var var;
    Buffer.add_string !buf " = (";
    translate_params params_list;
    Buffer.add_string !buf ") => {\n";
    Buffer.add_string !indbuf "    ";
    translate_coms com_list;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "}"

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
| NoneExp -> Buffer.add_string !buf "null"
| Lambda (params, e) -> translate_lambda params e
| React (react_component) -> translate_jsx react_component

and translate_jsx rc =
    Buffer.add_string !buf "(";
    translate_react rc;
    Buffer.add_string !buf "\n";
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf ")";

and translate_react rc = match rc with
| ReactComponentRecur (ReactOpen(str, attribs), children) ->
    Buffer.add_string !buf "\n";
    Buffer.add_string !indbuf "    ";
    Buffer.add_buffer !buf !indbuf;
    translate_react_component_recur str attribs children;
    Buffer.truncate !indbuf (undent !indbuf);
| ReactComponentExp (ReactOpen(str, attribs), e) ->
    Buffer.add_string !buf "\n";
    Buffer.add_string !indbuf "    ";
    Buffer.add_buffer !buf !indbuf;
    translate_react_component_inter str attribs e;
    Buffer.truncate !indbuf (undent !indbuf);

and translate_react_component_recur str attribs children =
    translate_react_open str attribs;
    translate_react_children children;
    Buffer.add_string !buf "\n";
    Buffer.add_buffer !buf !indbuf;
    translate_react_close str

and translate_react_children children = match children with
| h::t ->
    translate_react_children t;
    translate_react h;
| [] -> ()

and translate_react_component_inter str attribs e =
    translate_react_open str attribs;
    Buffer.add_string !buf "\n";
    Buffer.add_string !indbuf "    ";
    Buffer.add_buffer !buf !indbuf;
    Buffer.add_string !buf "{";
    translate_e e;
    Buffer.add_string !buf "}\n";
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_buffer !buf !indbuf;
    translate_react_close str


and translate_react_open str attribs =
    Buffer.add_string !buf "<";
    Buffer.add_string !buf str;
    translate_react_attribs attribs;
    Buffer.add_string !buf ">"


and translate_react_attribs attribs =
    List.iter (fun attrib ->
        Buffer.add_string !buf " ";
        translate_react_attrib attrib
    ) attribs;

and translate_react_attrib attrib = match attrib with
| Attrib (str, Bexp(Aexp(String(str_val)))) ->
    Buffer.add_string !buf str;
    Buffer.add_string !buf "=";
    Buffer.add_string !buf str_val;
| Attrib (str, e) ->
    Buffer.add_string !buf str;
    Buffer.add_string !buf "={";
    translate_e e;
    Buffer.add_string !buf "}";

and translate_react_close str =
    Buffer.add_string !buf "</";
    Buffer.add_string !buf str;
    Buffer.add_string !buf ">"

and translate_lambda (params: params_list) (e: exp) =
Buffer.add_string !buf "(";
translate_params params;
Buffer.add_string !buf (") => ");
translate_e e

and translate_string str =
Buffer.add_string !buf str;

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
    Buffer.add_string !buf "!";
    translate_bexp b;
    Buffer.add_string !buf ""
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
| EQ (b1, b2) ->
    translate_bexp b1;
    Buffer.add_string !buf " === ";
    translate_bexp b2
| NE (b1, b2) ->
    translate_bexp b1;
    Buffer.add_string !buf " !== ";
    translate_bexp b2
| Aexp (a) -> translate_aexp a

and translate_aexp (e: aexp) =
match e with
| Int (i) -> Buffer.add_string !buf (string_of_int i)
| Float (f) -> Buffer.add_string !buf (string_of_float f)
| String (strexp) -> translate_string strexp
| VarAccess (var_access) -> translate_var_access var_access
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
(* See https://discuss.ocaml.org/t/narrowing-variant-types-alternatives/3806/2
for an important discussion on "narrowing" the var_access type
with GADTs *)
and translate_var_access (var: var_access) =
let v = tranform_var_access var in
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
| List l -> 
    Buffer.add_string !buf "[";
    List.iter (fun exp -> translate_e exp;Buffer.add_string !buf ", ") l;
    Buffer.add_string !buf "]"
| Dict l ->
    Buffer.add_string !buf "{";
    Buffer.add_string !indbuf "    ";
    if l = [] then () else
    Buffer.add_string !buf "\n";
    List.iter (fun ((e1, e2): exp * exp) ->
        Buffer.add_buffer !buf !indbuf;
        translate_e e1;
        Buffer.add_string !buf ": ";
        translate_e e2;
        Buffer.add_string !buf ",\n") l;
    Buffer.truncate !indbuf (undent !indbuf);
    Buffer.add_string !buf "}"
| FuncCallVal(func_call) -> translate_func_call func_call
| Slice (v1, e1, e2) -> failwith "unreachable?"

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