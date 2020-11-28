(* info/error-reporting type copied from *)
(* CS 4110 A2 from Cornell University *)
type info = (int * int) * (int * int)

type aexp =
| Int of int
| Float of float
| VarAccess of var_access
| FuncCallVal of func_call
| Paren of exp
| Expon of aexp * aexp
| Neg of aexp
| Times of aexp * aexp
| Div of aexp * aexp
| Plus of aexp * aexp
| Minus of aexp * aexp
| Mod of aexp * aexp

and bexp =
| Or of bexp * bexp
| And of bexp * bexp
| Not of bexp
| Bool of bool
| GT of aexp * aexp
| GE of aexp * aexp
| LT of aexp * aexp
| LE of aexp * aexp
| EQ of aexp * aexp
| NE of aexp * aexp
| Aexp of aexp

and update_op =
| Equals
| PlusEquals
| MinusEquals
| TimesEquals
| DivideEquals
| ModuloEquals

and func_call = Call of (var_access * args_list)

and var = string

and var_access = Var of var
| Dot of var_access * var
| Key of var_access * exp

and react_open = ReactOpen of string * (react_attribute list)

and react_close = ReactClose of string

and react_component =
| ReactComponentRecur of react_open * (react_component list)
| ReactComponentExp of react_open * exp

and react_attribute = Attrib of string * exp

and exp =
| NoneExp
| Bexp of bexp
| String of string
| Dict of (exp * exp) list
| List of exp list                      (* Will also include tuples *)
| Lambda of params_list * (exp)
| React of react_component

and com =
| NoneCom
| ValUpdate of val_update
| FuncDef of var * params_list * (com list)
| FuncCallCom of func_call
| While of exp * (com list)
| If of if_com
| For of for_com
| ReturnExp of exp
| Return
| Break
| Continue
| Import of import
| Raise of string

and import =
| ImportBase of var * string
| ImportAs of var * string * var

and if_com =
| IfBase of exp * (com list) * (elif_com list)                       (* If-then *)
| IfElse of exp * (com list) * (elif_com list) * (com list)          (* If-elif-elif...else *)

and elif_com =
| Elif of exp * (com list)

(* for i in range(...) *)
(* for i in items *)
(* for i in [0, 1, 2, 3] *)
and for_com =
| ForFunc of var * (var * args_list) * (com list)
| ForIterVar of var * var * (com list)
(* | ForIter of var * (exp list) *)

and val_update =
| JLet of var * exp
| JConst of var * exp
| Update of var_access * update_op * exp

and params_list = Params of var list

and args_list = Args of exp list

and program = com list