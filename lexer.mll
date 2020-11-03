(* header *)
{
  open Printf
  open Lexing

  type token =
  | SQUOTE
  | DQUOTE
  | BOOL of bool
  | STRING of string
  | INT of int
  | FLOAT of float
  | VAR of string
  | EQUALS
  | PLUS_EQUALS
  | MINUS_EQUALS
  | TIMES_EQUALS
  | DIVIDE_EQUALS
  | MODULO_EQUALS
  | DOUBLE_EQUALS
  | PLUS
  | MINUS
  | TIMES
  | EXP
  | DIVIDE
  | FLOOR_DIVIDE
  | MODULO
  | GT
  | GE
  | LT
  | LE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | END
  | REACT
  | DOT
  | AND
  | OR
  | NOT
  | IN
  | IS
  | IF
  | ELIF
  | ELSE
  | WHILE
  | FOR
  | BREAK
  | CONTINUE
  | CLASS
  | DEF
  | LAMBDA
  | TRY
  | EXCEPT
  | RAISE
  | DELETE
  | IMPORT
  | AS
  | JLET
  | JCONST
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
let _multilinecomment_ = "'''"
let _multilineentirecomment_ = "'''"(_)*"'''"

(* optional minus sign + any digit* + 1-9{1} *)
let _doublequote_ = '\"'

let _singlequote_ = '\''

(* boolean *)
let _true_ = "True"
let _false_ = "False"

let _string_ = '\"'(_[^'\"'])*'\"'

(* let _number_ = "-"{0-1}['1'-'9']*['0'-'9']{1}['\.']*['0'-'9']* *)
(* let _int_ = "-"?['1'-'9']*['0'-'9'] *)
(* let _float_ = "-"?['1'-'9']*['0'-'9']"."['0'-'9']* *)
let _int_ = ['1'-'9']*['0'-'9']
let _float_ = ['1'-'9']*['0'-'9']"."['0'-'9']*
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

let _import_ = "import"
let _as_ = "as"

let _jlet_ = "@let"
let _jconst_ = "@const"

(* 

rules

 *)



rule tokens = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _singlelinecomment_ { token lexbuf }
| _multilinecomment_ { comment lexbuf }
| [' ' '\t' '\n'] { token lexbuf }
| _doublequote_ { DQUOTE }
| _singlequote_ {  SQUOTE }
| _true_ as b { BOOL b}
| _false_ as b { BOOL b}
| _string_ as string { STRING string }
| _int_ as int { INT int_of_string(int) }
| _float_ as float { FLOAT float_of_string(float) }
| _var_ as var { VAR var}
| _equals_ { EQUALS }
| _plusequals_ { PLUS_EQUALS }
| _minusequals_ { MINUS_EQUALS }
| _timesequals_ { TIMES_EQUALS }
| _divideequals_ { DIVIDE_EQUALS }
| _moduloequals_ { MODULO_EQUALS }
| _doubleequals_ { DOUBLE_EQUALS }
| _plus_ { PLUS }
| _minus_ { MINUS }
| _times_ { TIMES }
| _exp_ { EXP }
| _divide_ { DIVIDE }
| _floordivide_ { FLOOR_DIVIDE }
| _modulo_ { MODULO }
| _gt_ { GT }
| _ge_ { GE }
| _lt_ { LT }
| _le_ { LE }
| _lparen_ { LPAREN }
| _rparen_ { RPAREN }
| _lbracket_ { LBRACKET }
| _rbracket_ { RBRACKET }
| _comma_ { COMMA }
| _lbrace_ { LBRACE }
| _rbrace_ { RBRACE }
| _colon_ { COLON }
| _endblock_ { END }
| _reactblock_ { REACT }
| _dot_ { DOT }
| _and_ { AND }
| _or_ { OR }
| _not_ { NOT }
| _in_ { IN}
| _is_ { IS  }
| _if_ { IF}
| _elif_ { ELIF }
| _else_ { ELSE }
| _while_ { WHILE }
| _for_ { FOR }
| _break_ { BREAK }
| _continue_ { CONTINUE }
| _class_ { CLASS }
| _def_ { DEF }
| _lambda_ { LAMBDA }
| _return_ { RETURN }
| _try_ { TRY }
| _except_ { EXCEPT }
| _raise_ { RAISE }
| _delete_ { DELETE }
| _import_ { IMPORT }
| _as_ { AS }
| _jconst_ { JCONST }
| _jlet_ { JLET }
| eof { raise End_of_file }
| _ as c {
  let pos = lexbuf.Lexing.lex_curr_p in
  printf "Error @ line %d\n" pos.Lexing.pos_lnum;
  printf "Unrecognized character: %c\n" c
  }

and comment = parse
| _multilinecomment_ { token lexbuf }
| _ { comment lexbuf }

and react = parse
| _reactblock_ { token lexbuf }
| _ { lexeme lexbuf }


(* 

trailer

 *)

 {
  let rec parse lexbuf =
  let token = toy_lang lexbuf in
  parse lexbuf

  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

  let _ = Printexc.print main ()
 }