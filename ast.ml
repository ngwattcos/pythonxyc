type binop =
| Plus
| Minus
| Times
| Divide
| Modulo
| Expon

and boolop =
| And
| Or
| GT
| GE
| LT
| LE
| EQ
| NE

and t_bool = Boolean of bool
| BoolOp of exp * boolop * exp
| Not of exp

and binop_com =
| PlusEquals
| MinusEquals
| TimesEquals
| DivideEquals
| ModuloEquals

and var = string

and var_access = Var of var
| Dot of var_access * var
| Key of var_access * exp

and exp =
| None
| Int of int
| Float of float
| String of string
| VarAccess of var_access
| Dict of (exp * exp) list
| List of exp list                      (* Will also include typles *)
| TBool of t_bool
| Binop of exp * binop * exp
| Neg of exp
| Lambda of params_list * (exp)
| FuncCallVal of var_access * args_list       (* Applying arguments *)
| Paren of exp

and com =
| ValUpdate of val_update
| FuncDef of var * params_list * (com list)
| FuncCallCom of var_access * args_list
| While of exp * (com list)
| If of if_com
| For of for_com
| Exp of exp
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
| Update of var_access * exp
| BinopCom of var_access * binop_com * exp

and params_list = Params of var list

and args_list = Args of exp list

and program = com list