open Ast
exception TypeError of string

let sprintf = Printf.sprintf

let pretty_update_op = function
| Equals -> "Equals", "="
| PlusEquals -> "PlusEquals", "+="
| MinusEquals -> "MinusEquals", "-="
| TimesEquals -> "TimesEquals", "*="
| DivideEquals -> "DivideEquals", "/="
| ModuloEquals -> "ModuloEquals", "%="

let pretty_math_helper = function
| Expon _ -> "Expon", "**"
| Times _ -> "Times", "*"
| Div _ -> "Div", "/"
| Plus _ -> "Plus", "+"
| Minus _ -> "Minus", "-"
| Mod _ -> "Mod", "%"
| _ -> raise (TypeError "pretty_math_helper")

let rec pretty_aexp a = match a with
| Int i -> sprintf "Int( %d )" i, string_of_int i
| Float f -> sprintf "Float( %.5f )" f, sprintf " %.5f " f
| VarAccess va -> 
    let pva1, pva2 = pretty_va va in
    sprintf "VarAccess( %s )" pva1, sprintf " %s " pva2
| Paren e -> 
    let pe1, pe2 = pretty_exp e in 
    sprintf "Paren( %s )" pe1, sprintf "( %s )" pe2
| Expon (a1, a2) | Times (a1, a2) | Div (a1, a2) | Plus (a1, a2) | Minus (a1, a2) | Mod (a1, a2) ->
    let pop1, pop2 = pretty_math_helper a in
    let pa11, pa12 = pretty_aexp a1 in
    let pa21, pa22 = pretty_aexp a2 in
    sprintf "%s( %s, %s )" pop1 pa11 pa21, sprintf " %s %s %s " pa12 pop2 pa22

and pretty_bexp b = match b with
| Or (b1, b2) | And (b1, b2) -> "", ""
| Not b1 -> 
    let b11, b12 = pretty_bexp b1 in
    sprintf "Not( %s )" b11, sprintf " !%s " b12
| Bool false -> "Bool( false )", " false "
| Bool true -> "Bool ( true )", " true "
| GT (b1, b2)| GE (b1, b2)| LT (b1, b2)| LE (b1, b2)| EQ (b1, b2)| NE (b1, b2) -> "", ""
| Aexp a1 -> "", ""

and pretty_va = function
| _ -> "Unimplemented", "Unimplemented"

and pretty_exp = function
| _ -> "Unimplemented", "Unimplemented"
(*
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
| cl -> failwith "Unimplemented" *)
