(* header *)
{
  open Printf
}

(* 

Definitions

 *)

 (* whitespace *)
 let _crlf_ = '\r\n'
 let _newline_ = '\n'
 let _tab_ = '\t'
 let _whitespace_ = '\s'

(* optional minus sign + any digit* + 1-9{1} *)
let _doublequote_ = '"'

let _singlequote_ = '\''

(* primitives *)
let _true_ = 'True'
let _false = 'False'

let _string_ = '"'[_^'"']*'"'

let _number_ = ['-']{0-1}['1'-'9']*['0'-'9']{1}['.']*['0'-'9']*
let _var_ = ['a'-'z' 'A'-'Z' '_']+['a'-'z' 'A'-'Z' '_' '0'-'9']*

(* assignment operators *)
let _equals_ = '='
let _plusequals_ = '+='
let _minusequals_ = '-='
let _timesequals_ = '*='
let _divideequals_ = '/='
let _moduloequals_ = '%='


(* equality check *)
let _doubleequals_ = '=='

(* boolean logic operators *)
let _and_ = 'and'
let _or_ = 'or'
let _not_ = 'not'

(* object checks *)
let _in_ = 'in'
let _is_ = 'is'

(* numerical operators *)
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

(* braces, brackets, and parentheses *)
let _lparen- = '('
let _rparen- = ')'

let _lbracket_ = '['
let _rbracket_ = ']'
let _comma_ = ','

let _lbrace_ = '{'
let _rbrace_ = '{'

(* block begin and end *)
let _colon_ = ':'
let _endblock_ = '@end'
let _reactblock_ = '@react'

(* control flow *)
let _if_ = 'if'
let _elif_ = 'elif'
let _else_ = 'if'

let _while_ = 'while'
let _for_ = 'for'


let _break_ = 'break'
let _continue_ = 'continue'


let _class_ = 'class'

(* functions *)
let _def_ = 'def'
let _lambda_ = 'lambda'

let _return_ = 'return'

let _try_ = 'try'

let _raise_ = 'raise'

let _delete_ = 'del'




(* 

rules

 *)






(* 

trailer

 *)