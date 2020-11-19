type aexp =
| Int of int
| Float of float
| IntVarAccess of var_access
| IntFuncCallVal of func_call
| IntParen of exp
| Mod of aexp * aexp
| Plus of aexp * aexp
| Minus of aexp * aexp
| Times of aexp * aexp
| Div of aexp * aexp
| Expon of aexp * aexp
| Neg of aexp

and bexp =
| Bool of bool
| BoolVarAccess of var_access
| BoolFuncCallVal of func_call
| BoolParen of exp
| And of bexp * bexp
| Or of bexp * bexp
| GT of aexp * aexp
| GE of aexp * aexp
| LT of aexp * aexp
| LE of aexp * aexp
| EQ of exp * exp
| NE of exp * exp
| Not of bexp

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

and exp =
| None
| Aexp of aexp
| Bexp of bexp
| String of string
| VarAccess of var_access
| Dict of (exp * exp) list
| List of exp list                      (* Will also include tuples *)
| Lambda of params_list * (exp)
| FuncCallVal of func_call       (* Applying arguments *)
| Paren of exp

and com =
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