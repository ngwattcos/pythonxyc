(* header *)
{
  open Printf
  open Char
  open Lexing
  open Grammar

exception UnrecognizedStr of string
exception UnrecognizedChar
exception Eof
exception LexingError of string

(* error and line number code borrowed from *)
(* CS 4110 A2 from Cornell University *)
let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno;
  ()

let info lexbuf =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let ((l1,c1),(l2,c2)) = i in
  let s =
    if l2=l1
    then Printf.sprintf "\nline %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "\nline %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  let err = Printf.sprintf "%s: lexing error %s at %s."
    s
    msg
    t in
  raise (LexingError err)


}

(* 

Definitions

 *)



(* whitespace *)
(* let _crlf_ = '\\r\\n' *)
let _eol_ = '\n' | '\r''\n'
let _tab_ = '\t'
(* let _whitespace_ = '\s' *)

let _extend_ = "..."

let _expand_ = _extend_ _eol_+
 
(* comments *)
let _singlelinecomment_ = "#"
let _singlelinecommententire_ = "#"(_)*_eol_
let _multilinecomment_ = "'''"
let _multilineentirecomment_ = "'''"(_)*"'''"

(* optional minus sign + any digit* + 1-9{1} *)
let _doublequote_ = '\"'

let _singlequote_ = '\''

let _none_ = "None"

(* boolean *)
let _true_ = "True"
let _false_ = "False"

(*let _string_ = '\"'(_[^'\"'])*'\"'*)

(* See https://stackoverflow.com/questions/63460031/some-special-characters-are-allowed-only-if-they-are-preceded-by-an-escape-chara
for help on handling escape sequences.

*)

let _anything_ = ['a'-'z' 'A' - 'Z' '0' - '9' '!' '@' '#' '$' '%' '^' '&' '*'
'(' ')' '[' ']' '-' '_' '=' '+' '{' '}' '|' '\\' ';' ''' ':'
 ',' '.' '/' '<' '>' '?' '`' '~' ' ' '\t' '\n']

let _anything_non_special_ = ['a'-'z' 'A' - 'Z' '0' - '9' '!' '@' '#' '$']

let _string_ = "\""_anything_*"\""

let _digit_ = ['0'-'9']
(* let _int_ = ['1'-'9']*['0'-'9'] *)
(* let _float_ = ['1'-'9']*['0'-'9']"."['0'-'9']+ *)
let _int_ = _digit_+
let _float_ = _digit_+"."_digit_+
let _var_ = ['a'-'z' 'A'-'Z' '_']+_anything_non_special_*

(* assignment operators *)
let _equals_ = "="
let _plusequals_ = "+="
let _minusequals_ = "-="
let _timesequals_ = "*="
let _divideequals_ = "/="
let _moduloequals_ = "%="


(* equality check *)
let _doubleequals_ = "=="
let _notequals_ = "!="

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
let _rbrace_ = "}"

(* block begin and end *)
let _colon_ = ":"
let _endblock_ = "@end"
let _reactblock_ = "@react"

(* control flow *)
let _if_ = "if"
let _elif_ = "elif"
let _else_ = "else"

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
let _export_ = "export"
let _default_ = "default"
let _from_ = "from"
let _as_ = "as"

let _jlet_ = "@let"
let _jconst_ = "@const"

(* 

rules

 *)

rule token = parse
| [' ' '\t'] { token lexbuf }
| _extend_ (_eol_ | [' ' '\t'])* { token lexbuf }
| _eol_ { newline lexbuf; NEWLINE(info lexbuf)  }
| _singlelinecomment_ { single_comment lexbuf }
| _multilinecomment_ { multi_comment lexbuf }
| [' ' '\t' '\n'] { token lexbuf }
(*| _doublequote_ { DQUOTE(info lexbuf); parse_double_quote lexbuf }
| _singlequote_ { SQUOTE(info lexbuf); parse_single_quote lexbuf }*)
| _none_ { NONE(info lexbuf) }
| _true_ { BOOL(info lexbuf, true) }
| _false_ {  BOOL((info lexbuf), false) }
| _int_ as n { INT((info lexbuf), int_of_string n) }
| _float_ as n { FLOAT((info lexbuf), float_of_string n) }
| _equals_ { EQUALS(info lexbuf) }
| _plusequals_ { PLUS_EQUALS(info lexbuf) }
| _minusequals_ { MINUS_EQUALS(info lexbuf) }
| _timesequals_ { TIMES_EQUALS(info lexbuf) }
| _divideequals_ { DIVIDE_EQUALS(info lexbuf) }
| _moduloequals_ { MODULO_EQUALS(info lexbuf) }
| _doubleequals_ { DOUBLE_EQUALS(info lexbuf) }
| _notequals_ { NOT_EQUALS(info lexbuf) }
| _plus_ { PLUS(info lexbuf) }
| _minus_ { MINUS(info lexbuf) }
| _times_ { TIMES(info lexbuf) }
| _exp_ { EXP(info lexbuf) }
| _divide_ { DIVIDE(info lexbuf) }
(*| _floordivide_ { FLOOR_DIVIDE(info lexbuf) }*)
| _modulo_ { MODULO(info lexbuf) }
| _gt_ { GT(info lexbuf) }
| _ge_ { GE(info lexbuf) }
| _lt_ { LT(info lexbuf) }
| _le_ { LE(info lexbuf) }
| _lparen_ { LPAREN(info lexbuf) }
| _rparen_ { RPAREN(info lexbuf) }
| _lbracket_ { LBRACKET(info lexbuf) }
| _rbracket_ { RBRACKET(info lexbuf) }
| _comma_ { COMMA(info lexbuf) }
| _lbrace_ { LBRACE(info lexbuf) }
| _rbrace_ { RBRACE(info lexbuf) }
| _colon_ { COLON(info lexbuf) }
| _endblock_ { END(info lexbuf) }
| _reactblock_ { react lexbuf }
| _dot_ { DOT(info lexbuf) }
| _and_ { AND(info lexbuf) }
| _or_ { OR(info lexbuf) }
| _not_ { NOT(info lexbuf) }
| _in_ { IN(info lexbuf) }
| _is_ { IS(info lexbuf) }
| _if_ { IF(info lexbuf) }
| _elif_ { ELIF(info lexbuf) }
| _else_ { ELSE(info lexbuf) }
| _while_ { WHILE(info lexbuf) }
| _for_ { FOR(info lexbuf) }
| _break_ { BREAK(info lexbuf) }
| _continue_ { CONTINUE(info lexbuf) }
| _class_ { CLASS(info lexbuf) }
| _def_ { DEF(info lexbuf) }
| _lambda_ { LAMBDA(info lexbuf) }
| _return_ { RETURN(info lexbuf) }
| _try_ {TRY(info lexbuf) }
| _except_ { EXCEPT(info lexbuf) }
| _raise_ { RAISE(info lexbuf) }
| _delete_ { DELETE(info lexbuf) }
| _import_ { IMPORT(info lexbuf) }
| _export_ { EXPORT(info lexbuf) }
| _default_ { DEFAULT(info lexbuf) }
| _from_ { FROM(info lexbuf) }
| _as_ { AS(info lexbuf) }
| _jconst_ { JCONST(info lexbuf) }
| _jlet_ { JLET(info lexbuf) }
| _var_ as var { VAR((info lexbuf), var)}
| eof { EOF }
| _string_ as str { STRING((info lexbuf), str) }
| _ as c { error lexbuf (String.make 1 c) }

(*and parse_single_quote = parse
| _singlequote_ {  SQUOTE; token lexbuf }
| _ as c { parse_single_quote lexbuf }

and parse_double_quote = parse
| _doublequote_ { DQUOTE; token lexbuf }
| _ as c { parse_double_quote lexbuf }*)

and single_comment = parse
| _eol_ { newline lexbuf; token lexbuf }
| eof  { exit 0 }
| _ { single_comment lexbuf }

and multi_comment = parse
| _eol_ { newline lexbuf; multi_comment lexbuf}
| _multilinecomment_ { token lexbuf }
| _ { multi_comment lexbuf }

and react = parse
| _reactblock_ { token lexbuf }
(*| _ as jsx { REACT_CHAR (jsx); react lexbuf }*)


(* 

trailer

 *)

 {
  (* let rec parse lexbuf = parse lexbuf *)
  (*
  let main () = begin
    try
      let filename = Sys.argv.(1) in
      let file_handle = open_in filename in
      let lexbuf = Lexing.from_channel file_handle in

      while true do
        let result = token lexbuf in ()
      done
    with Eof -> exit 0
  end;;
  main () ;;
 *)
 }