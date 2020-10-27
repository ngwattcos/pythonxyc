(* header *)
{
  open Printf
}

(* 

Definitions

 *)

(* optional minus sign + any digit* + 1-9{1} *)
let _doublequote_ = '"'

let _singlequote_ = '\''

let _true_ = 'True'
let _false = 'False'

let _string_ = '"'[_^'"']'"'

let _number_ = ['-']{0-1}['1'-'9']*['0'-'9']{1}['.']*['0'-'9']*
let _var_ = ['a'-'z' 'A'-'Z' '_']+['a'-'z' 'A'-'Z' '_' '0'-'9']*

let _equals_ = '='
let _doubleequals_ = '=='


let _and_ = 'and'
let _or_ = 'or'
let _not_ = 'not'


let _in_ = 'in'
let _is_ = 'is'

let _plus_ = '+'
let _minus_ = '-'
let _times_ = '*'
let _exp_ = '**'
let _divide_ = '/'
let _floordivide_ = '//'
let _modulo_ = '%'
let _gt_ = '>'
let _ge_ = '>='
let _lt_ = '<'
let _le_ = '<='

let _lparen- = '('
let _rparen- = ')'

let _lbracket_ = '['
let _rbracket_ = ']'
let _comma_ = ','

let _lbrace_ = '{'
let _rbrace_ = '{'

let _colon_ = ':'
let _endblock_ = '@end'

let _if_ = 'if'
let _elif_ = 'elif'
let _else_ = 'if'

let _while_ = 'while'
let _for_ = 'for'


let _break_ = 'break'
let _continue_ = 'continue'


let _class_ = 'class'

let _def_ = 'def'
let _lambda_ = 'lambda'

let _return_ = 'return'

let _try_ = 'try'

let _raise_ = 'raise'

let _delete_ = 'del'




(* rules *)

(* trailer *)