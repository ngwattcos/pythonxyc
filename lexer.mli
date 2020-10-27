(* header *)
{
  open Printf
}

(* 

Definitions

 *)

(* optional minus sign + any digit* + 1-9{1} *)
let _number_ = ['-']{0-1}['1'-'9']*['0'-'9']{1}['.']*['0'-'9']*
let _var_ = ['a'-'z''A'-'Z']+['a'-'z''A'-'Z''_''0'-'9']*
let _varallowprop_ = ['a'-'z''A'-'Z']+['a'-'z''A'-'Z''_''0'-'9']*[.['a'-'z''A'-'Z']+['a'-'z''A'-'Z''_''0'-'9']*]*


(* rules *)

(* trailer *)