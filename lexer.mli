(* header *)
{
  open Printf
}

(* 

Definitions

 *)



(* whitespace *)
(* let _crlf_ = '\\r\\n' *)
let _newline_ = '\n'
let eol = '\n' | '\r''\n'
let _tab_ = '\t'
(* let _whitespace_ = '\s' *)
 
(* comments *)
let _singlelinecomment_ = "#"(_)*eol
let _multilinecomment_ = "'''"(_)*"'''"

(* optional minus sign + any digit* + 1-9{1} *)
let _doublequote_ = '\"'

let _singlequote_ = '\''

(* boolean *)
let _true_ = "True"
let _false_ = "False"

let _string_ = '\"'(_[^'\"'])*'\"'

(* let _number_ = "-"{0-1}['1'-'9']*['0'-'9']{1}['\.']*['0'-'9']* *)
let _number_ = "-"?['1'-'9']*['0'-'9']"."['0'-'9']*
let _var_ = ['a'-'z' 'A'-'Z' '_']+['a'-'z' 'A'-'Z' '_' '0'-'9']*

(* assignment operators *)
let _equals_ = "="
let _plusequals_ = "+="
let _minusequals_ = "-="
let _timesequals_ = "*="
let _divideequals_ = "/="
let _moduloequals_ = "%="


(* equality check *)
let _doubleequals_ = "=="

(* boolean logic operators *)
let _and_ = "and"
let _or_ = "or"
let _not_ = "not"

(* object checks *)
let _in_ = "in"
let _is_ = "is"

(* numerical operators *)
let _plus_ = "+"
let _minus_ = "-"
let _times_ = "*"
let _exp_ = "**"
let _divide_ = "/"
let _floordivide_ = "//"
let _modulo_ = "%"
let _gt_ = ">"
let _ge_ = ">="
let _lt_ = "<"
let _le_ = "<="

(* braces, brackets, and parentheses *)
let _lparen_ = "("
let _rparen_ = ")"

let _lbracket_ = "["
let _rbracket_ = "]"
let _comma_ = ","

let _lbrace_ = "{"
let _rbrace_ = "{"

(* block begin and end *)
let _colon_ = ":"
let _endblock_ = "@end"
let _reactblock_ = "@react"

(* control flow *)
let _if_ = "if"
let _elif_ = "elif"
let _else_ = "if"

let _while_ = "while"
let _for_ = "for"


let _break_ = "break"
let _continue_ = "continue"


let _class_ = "class"

let _dot_ = "."

(* functions *)
let _def_ = "def"
let _lambda_ = "lambda"

let _return_ = "return"

let _try_ = "try"
let _except_ = "except"

let _raise_ = "raise"

let _delete_ = "delete"




(* 

rules

 *)



rule token = parse
| _singlelinecomment_ { token lexbuf }
| _multilinecomment_ { token lexbuf }
| _doublequote_ { print_endline "DOUBLE_QUOTE" }
| _singlequote_ { print_endline "SINGLE_QUOTE" }
| _true_ { print_endline "TRUE"}
| _false_ { print_endline "FALSE"}
| _string_ as string { print_endline ("STR(" ^ string ")") }
| _number_ as number { print_endline ("NUM(" ^ number ^ ")") }
| _var_ as var { print_endline ("VAR(" ^ var ^ ")")}
| _equals_ { print_endline "EQUALS" }
| _plusequals_ { print_endline "PLUS_EQUALS" }
| _minusequals_ { print_endline "MINUS_EQUALS" }
| _timesequals_ { print_endline "TIMES_EQUALS" }
| _divideequals_ { print_endline "DIVIDE_EQUALS" }
| _moduloequals_ { print_endline "MODULO_EQUALS" }
| _doubleequals_ { print_endline "DOUBLE_EQUALS" }
| _and_ { print_endline "AND" }
| _or_ { print_endline "OR" }
| _not_ { print_endline "NOT" }
| _in_ { print_endline "IN"}
| _is_ { print_endline "IS" }
| _plus_ { print_endline "PLUS"}
| _minus_ { print_endline "MINUS"}
| _times_ { print_endline "TIMES"}
| _exp_ { print_endline "EXP"}
| _divide_ { print_endline "DIVIDE"}
| _floordivide_ { print_endline "FLOOR_DIVIDE"}
| _modulo_ { print_endline "MODULO"}
| _gt_ { print_endline "GT"}
| _ge_ { print_endline "GE"}
| _lt_ { print_endline "LT"}
| _le_ { print_endline "LE"}
| _lparen_ { print_endline "LEFT_PAREN"}
| _rparen_ { print_endline "RIGHT_PAREN"}
| _lbracket_ { print_endline "LEFT_BRACKET"}
| _rbracket_ { print_endline "RIGHT_BRACKET"}
| _comma_ { print_endline "COMMA"}
| _lbrace_ { print_endline "LEFT_BRACE"}
| _rbrace_ { print_endline "RIGHT_BRACE"}
| _colon_ { print_endline "COLON"}
| _endblock_ { print_endline "END"}
| _reactblock_ { print_endline "REACT"}
| _if_ { print_endline "IF"}
| _elif_ { print_endline "ELIF"}
| _else_ { print_endline "ELSE"}
| _while_ { print_endline "WHILE"}
| _for_ { print_endline "FOR"}
| _break_ { print_endline "BREAK"}
| _continue_ { print_endline "CONTINUE"}
| _class_ { print_endline "CLASS"}
| _dot_ { print_endline "DOT"}
| _def_ { print_endline "DEF"}
| _lambda_ { print_endline "LAMBDA"}
| _return_ { print_endline "RETURN"}  
| _try_ { print_endline "TRY"}
| _except_ { print_endline "EXCEPT"}
| _raise_ { print_endline "RAISE"}
| _delete_ { print_endline "DELETE"}



(* 

trailer

 *)