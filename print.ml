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

let pretty_bool_helper = function
| Or _ -> "Or", "||"
| And _ -> "And", "&&"
| GT _ -> "GT", ">"
| GE _ -> "GE", ">="
| LT _ -> "LT", "<"
| LE _ -> "LE", "<="
| EQ _ -> "EQ", "=="
| NE _ -> "NEQ", "!="
| _ -> raise (TypeError "pretty_bool_helper")

let rec pretty_aexp a = match a with
| Int i -> sprintf "Int( %d )" i, string_of_int i
| Float f -> sprintf "Float( %.5f )" f, sprintf " %.5f " f
| String s -> "String( " ^ s ^ " )", s
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
| Or (b1, b2) | And (b1, b2) -> 
    let pop1, pop2 = pretty_bool_helper b in
    let pb11, pb12 = pretty_bexp b1 in
    let pb21, pb22 = pretty_bexp b2 in
    sprintf "%s( %s, %s )" pop1 pb11 pb21, sprintf " %s %s %s " pop2 pb12 pb22
| Not b1 -> 
    let b11, b12 = pretty_bexp b1 in
    sprintf "Not( %s )" b11, sprintf " !%s " b12
| Bool false -> "Bool( false )", " false "
| Bool true -> "Bool ( true )", " true "
| GT (b1, b2)| GE (b1, b2)| LT (b1, b2)| LE (b1, b2) -> 
    let pop1, pop2 = pretty_bool_helper b in
    let pb11, pb12 = pretty_aexp b1 in
    let pb21, pb22 = pretty_aexp b2 in
    sprintf "%s( %s, %s )" pop1 pb11 pb21, sprintf " %s %s %s " pop2 pb12 pb22
| EQ (b1, b2)| NE (b1, b2) -> 
    let pop1, pop2 = pretty_bool_helper b in
    let pb11, pb12 = pretty_bexp b1 in
    let pb21, pb22 = pretty_bexp b2 in
    sprintf "%s( %s, %s )" pop1 pb11 pb21, sprintf " %s %s %s " pop2 pb12 pb22
| Aexp a -> 
    let pa1, pa2 = pretty_aexp a in
    sprintf "Aexp( %s )" pa1, pa2

and pretty_dict_helper = function
| (e1, e2)::t -> 
    let pe11, pe12 = pretty_exp e1 in
    let pe21, pe22 = pretty_exp e2 in
    let ph1, ph2 = pe11 ^ ": " ^ pe21, pe12 ^ ": " ^ pe22 in
    if t = [] 
    then ph1, ph2 
    else let pt1, pt2 = pretty_dict_helper t in ph1 ^ ", " ^ pt1, ph2 ^ ", " ^ pt2
| [] -> "", ""

and pretty_list_helper = function
| e::t ->  
    let pe1, pe2 = pretty_exp e in
    if t = [] 
    then pe1, pe2
    else let pt1, pt2 = pretty_list_helper t in pe1 ^ ", " ^ pt1, pe1 ^ ", " ^ pt2
| [] -> "", ""

and pretty_va = function
| Var v -> "Var( " ^ v ^ " )", v
| Dict l -> let pl1, pl2 = pretty_dict_helper l in "Dict( ["^ pl1 ^"] )", "{" ^ pl2 ^"}"
| List l -> let pl1, pl2 = pretty_list_helper l in "List( ["^ pl1 ^"] )", "[" ^ pl2 ^"]"
| Dot (va, v) -> 
    let pva1, pva2 = pretty_va va in 
    sprintf "Dot( %s, %s )"  pva1 v, sprintf "%s.%s" pva2 v
| Key (va, e) ->
    let pva1, pva2 = pretty_va va in 
    let pe1, pe2 = pretty_exp e in
    sprintf "Key( %s, %s )"  pva1 pe1, sprintf "%s[ %s ]" pva2 pe2
| Slice (va, e1, e2) ->
    let pva1, pva2 = pretty_va va in
    let pe11, pe12 = pretty_exp e1 in 
    let pe21, pe22 = pretty_exp e2 in
    sprintf "Slice( %s, %s, %s )"  pva1 pe11 pe21, sprintf "%s[ %s : %s ]" pva2 pe12 pe22
| FuncCallVal (Call (va, l)) -> 
    let pva1, pva2 = pretty_va va in 
    let pargs1, pargs2 = pretty_args_list l in 
    sprintf "FuncCallVal( Call( %s, [ %s ] ) )" pva1 pargs1, pva2 ^ "( " ^ pargs2 ^ " )"

and pretty_react_open (ReactOpen (s, l))= "ReactOpen()", "<>"

and pretty_react_attribute (Attrib (s, e)) =  "Attrib()", "Attrib()"

and pretty_react_close (ReactClose s) = "ReactClose( " ^ s ^ " )", "<" ^ s ^ "/>"

and pretty_react_component = function
| ReactComponentRecur (ro, l) -> "React", "React"
| ReactComponentExp (ro, e) -> "React", "React"

and pretty_exp = function
| NoneExp -> "NoneExp", "None"
| Bexp b -> 
    let pb1, pb2 = pretty_bexp b in "Bexp( " ^ pb1 ^ " )", pb2
| Lambda (l, e) -> 
    let pl1, pl2 = pretty_params_list l in
    let pe1, pe2 = pretty_exp e in
    sprintf "Lambda( [ %s ], %s )" pl1 pe1, "λ( " ^ pl2 ^ " )." ^ pe2
| React rc -> 
    let prc1, prc2 = pretty_react_component rc in "React( " ^ prc1 ^ " )", prc2

and pretty_com = function 
| NoneCom -> "NoneCom", "NoneC"
| ValUpdate vu -> 
    let pvu1, pvu2 = pretty_vu vu in 
    sprintf "ValUpdate( %s )" pvu1, pvu2
| FuncCallCom (Call (va, l)) ->  
    let pva1, pva2 = pretty_va va in 
    let pargs1, pargs2 = pretty_args_list l in 
    sprintf "FuncCallVal( Call( %s, [ %s ] ) )" pva1 pargs1, pva2 ^ "( " ^ pargs2 ^ " )"
| ReturnExp e -> 
    let pe1, pe2 = pretty_exp e in
    "Return( " ^ pe1 ^ " )", "return " ^ pe2
| Return -> "Return", "return"
| Break -> "Break", "break"
| Continue -> "Continue", "continue"
| Import i -> 
    let pi1, pi2 = pretty_import i in
    "Import( " ^ pi1 ^ " )", "import " ^ pi2 ^ " "
| Export e -> "export of some kind", "yeah"
| Raise s -> "Raise( " ^ s ^ " )", "raise"
| FuncDef (v, pl, cl) -> "FuncDef", ""
| While (e, cl) -> "While", "while"
| If ic ->  "If", "if"
| For fc ->  "For", " for"

and pretty_import = function 
| ImportDefault (v, str) -> "ImportDefault( " ^ v ^ ", " ^ str ^ " )", "ImportDefault"
| ImportFromString (lst, str) -> "ImportBaseString( " ^ combine lst "" ^ " )", "ImportFromString"

and combine lst acc = match lst with
| h::t -> h ^ acc
| [] -> acc

and pretty_vu = function 
| JLet (v, e) -> 
    let pe1, pe2 = pretty_exp e in 
    sprintf "JLet( %s, %s )" v pe1, v ^ " = " ^ pe2
| JConst (v, e) -> 
    let pe1, pe2 = pretty_exp e in 
    sprintf "JConst( %s, %s )" v pe1, v ^ " = " ^ pe2
| Update (va, op, e) -> 
    let pva1, pva2 = pretty_va va in
    let pop1, pop2 = pretty_update_op op in
    let pe1, pe2 = pretty_exp e in 
    sprintf "Update( %s, %s, %s )" pva1 pop1 pe1, pva2 ^ pop2 ^ pe2

and pretty_params_list = function
| v::t -> 
    if t = [] 
    then v, v
    else let pt1, pt2 = pretty_params_list t in v ^ ", " ^ pt1, v ^ ", " ^ pt2
| [] -> "", ""

and pretty_args_list = function
| e::t -> 
 let pe1, pe2 = pretty_exp e in
    if t = [] 
    then pe1, pe2
    else let pt1, pt2 = pretty_list_helper t in pe1 ^ ", " ^ pt1, pe1 ^ ", " ^ pt2
| [] -> "", ""

and pretty_program b l =
print_string "[\n";
List.iter (fun c -> let pc1, pc2 = pretty_com c in if b then print_string pc1 else print_string pc2; print_string ",\n") l;
print_string "]\n"

let print_verbose = pretty_program true
let print_mini = pretty_program false