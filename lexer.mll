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
let _int_ = "-"?['1'-'9']*['0'-'9']
let _float_ = "-"?['1'-'9']*['0'-'9']"."['0'-'9']*
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


(* 

rules

 *)



rule tokens = parse
| ws { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _singlelinecomment_ { token lexbuf }
| _multilinecomment_ { comment lexbuf }
| [' ' '\t' '\n'] { token lexbuf }
| _doublequote_ { print_endline "DOUBLE_QUOTE"; DQUOTE }
| _singlequote_ { print_endline "SINGLE_QUOTE"; SQUOTE }
| _true_ as b { print_endline "TRUE"; BOOL b}
| _false_ as b { print_endline "FALSE" BOOL b}
| _string_ as string { print_endline ("STR(" ^ string ")"); STRING string }
| _int_ as int { print_endline ("NUM(" ^ int ^ ")"); INT int }
| _float_ as float { print_endline ("NUM(" ^ float ^ ")"); FLOAT float }
| _var_ as var { print_endline ("VAR(" ^ var ^ ")"); VAR var}
| _equals_ { print_endline "EQUALS"; EQUALS }
| _plusequals_ { print_endline "PLUS_EQUALS"; PLUS_EQUALS }
| _minusequals_ { print_endline "MINUS_EQUALS"; MINUS_EQUALS }
| _timesequals_ { print_endline "TIMES_EQUALS"; TIMES_EQUALS }
| _divideequals_ { print_endline "DIVIDE_EQUALS"; DIVIDE_EQUALS }
| _moduloequals_ { print_endline "MODULO_EQUALS"; MODULO_EQUALS }
| _doubleequals_ { print_endline "DOUBLE_EQUALS"; DOUBLE_EQUALS }
| _plus_ { print_endline "PLUS"; PLUS }
| _minus_ { print_endline "MINUS"; MINUS }
| _times_ { print_endline "TIMES"; TIMES }
| _exp_ { print_endline "EXP"; EXP}
| _divide_ { print_endline "DIVIDE"; DIVIDE}
| _floordivide_ { print_endline "FLOOR_DIVIDE"; FLOOR_DIVIDE}
| _modulo_ { print_endline "MODULO"; MODULO}
| _gt_ { print_endline "GT"; GT}
| _ge_ { print_endline "GE"; GE}
| _lt_ { print_endline "LT"; LT}
| _le_ { print_endline "LE"; LE}
| _lparen_ { print_endline "LEFT_PAREN"; LPAREN }
| _rparen_ { print_endline "RIGHT_PAREN"; RPAREN }
| _lbracket_ { print_endline "LEFT_BRACKET"; LBRACKET }
| _rbracket_ { print_endline "RIGHT_BRACKET"; RBRACKET }
| _comma_ { print_endline "COMMA"; COMMA }
| _lbrace_ { print_endline "LEFT_BRACE"; LBRACE }
| _rbrace_ { print_endline "RIGHT_BRACE"; RBRACE }
| _colon_ { print_endline "COLON"; COLON }
| _endblock_ { print_endline "END"; END }
| _reactblock_ { print_endline "REACT"; REACT }
| _dot_ { print_endline "DOT"; DOT }
| _and_ { print_endline "AND"; AND }
| _or_ { print_endline "OR"; OR }
| _not_ { print_endline "NOT"; NOT }
| _in_ { print_endline "IN"; IN}
| _is_ { print_endline "IS"; IS  }
| _if_ { print_endline "IF"; IF}
| _elif_ { print_endline "ELIF"; ELIF }
| _else_ { print_endline "ELSE"; ELSE }
| _while_ { print_endline "WHILE"; WHILE }
| _for_ { print_endline "FOR"; FOR }
| _break_ { print_endline "BREAK"; BREAK }
| _continue_ { print_endline "CONTINUE"; CONTINUE }
| _class_ { print_endline "CLASS"; CLASS }
| _def_ { print_endline "DEF"; DEF }
| _lambda_ { print_endline "LAMBDA"; LAMBDA }
| _return_ { print_endline "RETURN"; RETURN }
| _try_ { print_endline "TRY"; TRY }
| _except_ { print_endline "EXCEPT"; EXCEPT }
| _raise_ { print_endline "RAISE"; RAISE }
| _delete_ { print_endline "DELETE"; DELETE }
| _import_ { print_endline "IMPORT"; IMPORT }
| _as_ { print_endline "AS"; AS}
| _ as c
  { printf "Unrecognized character: %c\n" c }
| eof
  { raise End_of_file }

and comment = parse
| _multilinecomment_ { token lexbuf}
| _ { comment lexbuf}

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