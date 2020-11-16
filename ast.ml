type binop =
| Plus
| Minus
| Times
| Divide
| Modulo
| And
| Or
| GT
| GE
| LT
| LE
| EQ
| NE

and unop =
| Not
| Neg

and binop_com =
| PlusEquals of var_access * exp
| MinusEquals of var_access * exp
| TimesEquals of var_access * exp
| DivdeEquals of var_access * exp
| ModuloEquals of var_access * exp

and var = ID of string

and var_access = Var of var
| Dot of var_access * var
| Key of var_access * string

and lambda = Lambda of (var list) * (com list)

and exp =
| None
| Int of int
| Float of float
| String of string
| VarAccess of var_access
| Dict of (exp * exp) list
| List of exp list                      (* Will also include typles *)
| Binop of exp * binop * exp
| Unop of exp * unop
| Func of lambda
| FuncCallVal of var * (exp list)       (* Applying arguments *)

and com =
| ValUpdate of val_update
| FuncDef of var * lambda
| FuncCallCom of var_access * (exp list)
| While of exp * (com list)
| If of if_com
| For of for_com
| Exp of exp
| Empty

and if_com =
| IfBase of bexp * (com list) * (elif_com list)                       (* If-then *)
| IfElse of bexp * (com list) * (elif_com list) * (com list)          (* If-elif-elif...else *)

and elif_com =
| Elif of bexp * (com list)

(* for i in range(...) *)
(* for i in items *)
(* for i in [0, 1, 2, 3] *)
and for_com =
| ForRange of var * (var * (exp list)) * (com list)
| ForIterVar of var * var * (com list)
(* | ForIter of var * (exp list) *)

and val_update =
| Jlet of var * exp
| JConst of var * exp
| Update of var_access * exp
| BinopCom of var_access * binop_com * exp
| Return of exp option
| Break
| Continue
| Raise of string
