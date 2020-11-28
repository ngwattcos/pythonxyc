open Ast

let sprintf = Printf.sprintf

let pretty_3 (s1: string) (op: string) (s2: string) = sprintf "%s %s %s" s1 op s2

let pretty_update_op = function
| Equals -> "="
| PlusEquals -> "+="
| MinusEquals -> "-="
| TimesEquals -> "*="
| DivideEquals -> "/="
| ModuloEquals -> "%="

let rec pretty_react_close = function
| ReactClose s -> failwith "Unimplemented"

let rec pretty_import = function
| ImportBase (v, s) -> sprintf "import %s from \"%s\"" v s
| ImportAs (v1, s, v2) -> sprintf "import %s as %s from \"%s\"" v1 s v2

let rec pretty_aexp = function
| Int i -> sprintf "%d" i
| Float f -> sprintf "%.5f" f
| IntParen a -> pretty_aexp a |> sprintf "(%s)" 
| Expon (a1, a2) -> sprintf "%s^%s" (pretty_aexp a1) (pretty_aexp a2)
| Neg a -> pretty_aexp a |> sprintf "-%s" 
| Times (a1, a2) -> pretty_3 (pretty_aexp a1) "*" (pretty_aexp a2)
| Div (a1, a2) -> pretty_3 (pretty_aexp a1) "\\" (pretty_aexp a2)
| Plus (a1, a2) -> pretty_3 (pretty_aexp a1) "+" (pretty_aexp a2)
| Minus (a1, a2) -> pretty_3 (pretty_aexp a1) "-" (pretty_aexp a2)
| Mod (a1, a2) -> pretty_3 (pretty_aexp a1) "%" (pretty_aexp a2)
| IntVarAccess va -> pretty_var_access va
| IntFuncCallVal fc -> pretty_function_call fc

and pretty_bexp = function
| BoolVarAccess va -> pretty_var_access va
| BoolFuncCallVal fc -> pretty_function_call fc
| BoolParen b -> pretty_bexp b |> sprintf "(%s)" 
| Or (b1, b2) -> pretty_3 (pretty_bexp b1) "||" (pretty_bexp b2)
| And (b1, b2) -> pretty_3 (pretty_bexp b1) "&&" (pretty_bexp b2)
| Not b -> pretty_bexp b |> sprintf "!%s" 
| Bool true -> "True"
| Bool false -> "False"
| GT (a1, a2) -> pretty_3 (pretty_aexp a1) ">" (pretty_aexp a2)
| GE (a1, a2) -> pretty_3 (pretty_aexp a1) ">=" (pretty_aexp a2)
| LT (a1, a2) -> pretty_3 (pretty_aexp a1) "<" (pretty_aexp a2)
| LE (a1, a2) -> pretty_3 (pretty_aexp a1) "<=" (pretty_aexp a2)
| EQ (a1, a2) -> pretty_3 (pretty_aexp a1) "==" (pretty_aexp a2)
| NE (a1, a2) -> pretty_3 (pretty_aexp a1) "!=" (pretty_aexp a2)
| Aexp a -> pretty_aexp a

and pretty_function_call = function
| Call (va, al) -> sprintf "%s(%s)" (pretty_var_access va) (pretty_args_list al)

and pretty_var_access = function
| Var v -> v
| Dot (va, v) -> sprintf "%s.%s" (pretty_var_access va) v
| Key (va, e) -> sprintf "%s[%s]" (pretty_var_access va) (pretty_exp e)

and pretty_react_open = function
| ReactOpen (s, ral) -> failwith "Unimplemented"

and pretty_react_component = function
| ReactComponent (ro, rc) -> failwith "Unimplemented"

and pretty_react_attribute = function
| Attrib (s, e) -> failwith "Unimplemented"

and pretty_exp = function
| None -> "null"
| Bexp b -> pretty_bexp b
| String s -> "\"" ^ s ^ "\""
| VarAccess va -> pretty_var_access va
| Dict l -> failwith "Unimplemented"
| List l -> failwith "Unimplemented"
| Lambda (pl, e) -> failwith "Unimplemented"
| FuncCallVal fc -> pretty_function_call fc
| Paren e -> pretty_exp e |> sprintf "(%s)"
| React rc -> pretty_react_component rc

and pretty_com = function
| ValUpdate vu -> failwith "Unimplemented"
| FuncDef (v, pl, cl) -> failwith "Unimplemented"
| FuncCallCom fc -> pretty_function_call fc
| While (e, c) -> failwith "Unimplemented"
| If ic -> pretty_if_com ic
| For fc -> pretty_for_com fc
| ReturnExp e -> "return " ^ (pretty_exp e)
| Return -> "return"
| Break -> "break"
| Continue -> "continue"
| Import i -> pretty_import i
| Raise s -> failwith "Unimplemented"

and pretty_if_com = function
| IfBase (e, cl, ecl) -> failwith "Unimplemented"
| IfElse (e, cl1, ecl, cl2) -> failwith "Unimplemented"

and pretty_elif_com = function
| Elif (e, cl) -> failwith "Unimplemented" 

and pretty_for_com = function
| ForFunc (v, s, cl) -> failwith "Unimplemented"
| ForIterVar (v1, v2, cl) -> failwith "Unimplemented"

and pretty_val_update = function
| JLet (v, e) -> sprintf "let %s = %s" v (pretty_exp e)
| JConst (v, e) ->  sprintf "const %s = %s" v (pretty_exp e)
| Update (va, uo, e) -> failwith "Unimplemented"

and pretty_params_list = function
| Params vl -> failwith "Unimplemented"

and pretty_args_list = function
| Args el -> failwith "Unimplemented"

and pretty_program = function
| cl -> failwith "Unimplemented"