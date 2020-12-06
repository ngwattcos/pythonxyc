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
(*
  type token =
  | NEWLINE
  | EXTEND
  | SQUOTE
  | DQUOTE
  | NONE
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
  | NOT_EQUALS
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
  | RETURN
  | LAMBDA
  | TRY
  | EXCEPT
  | RAISE
  | DELETE
  | IMPORT
  | FROM
  | AS
  | JLET
  | JCONST
  | REACT
  | REACT_JSX of string
  | REACT_CHAR of char
  | EOF
*)

(* error and line number code borrowed from *)
(* CS 4110 A2 from Cornell University *)
let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno;
  printf "\\n\n";
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

let _anything_ = ['a'-'z' 'A' - 'Z' '0' - '9' '!' '@' '#' '$' '%' '^' '&' '*'
'(' ')' '[' ']' '-' '_' '=' '+' '{' '}' '|' '\\' ';' ''' ':'
 ',' '.' '/' '<' '>' '?' '`' '~' ' ' '\t' '\n']

let _string_ = "\""_anything_*"\""

let _digit_ = ['0'-'9']
(* let _int_ = ['1'-'9']*['0'-'9'] *)
(* let _float_ = ['1'-'9']*['0'-'9']"."['0'-'9']+ *)
let _int_ = _digit_+
let _float_ = _digit_+"."_digit_+
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
let _from_ = "from"
let _as_ = "as"

let _jlet_ = "@let"
let _jconst_ = "@const"

(* 

rules

 *)

rule token = parse
| [' ' '\t'] { token lexbuf }
| _extend_ (_eol_ | [' ' '\t'])* { printf "(...)"; token lexbuf }
| _eol_ { newline lexbuf; NEWLINE(info lexbuf)  }
| _singlelinecomment_ { printf "#"; single_comment lexbuf }
| _multilinecomment_ { printf "'''---"; multi_comment lexbuf }
| [' ' '\t' '\n'] { print_endline "[ws]"; token lexbuf }
(*| _doublequote_ { printf "\""; DQUOTE(info lexbuf); parse_double_quote lexbuf }
| _singlequote_ { printf "'"; SQUOTE(info lexbuf); parse_single_quote lexbuf }*)
| _none_ { printf "NONE "; NONE(info lexbuf) }
| _true_ { printf "(TRUE) "; BOOL(info lexbuf, true) }
| _false_ { printf "(FALSE) "; BOOL((info lexbuf), false) }
| _int_ as n { printf "int(%s) " n; INT((info lexbuf), int_of_string n) }
| _float_ as n { printf "float(%s) " n; FLOAT((info lexbuf), float_of_string n) }
| _equals_ { printf "(=) "; EQUALS(info lexbuf) }
| _plusequals_ { printf "(+=) "; PLUS_EQUALS(info lexbuf) }
| _minusequals_ { printf "(-=) "; MINUS_EQUALS(info lexbuf) }
| _timesequals_ { printf "(*=) "; TIMES_EQUALS(info lexbuf) }
| _divideequals_ { printf "(/=) "; DIVIDE_EQUALS(info lexbuf) }
| _moduloequals_ { printf "(MOD=) "; MODULO_EQUALS(info lexbuf) }
| _doubleequals_ { printf "(==) "; DOUBLE_EQUALS(info lexbuf) }
| _notequals_ { printf "(!=) "; NOT_EQUALS(info lexbuf) }
| _plus_ { printf "(+) "; PLUS(info lexbuf) }
| _minus_ { printf "(-) "; MINUS(info lexbuf) }
| _times_ { printf "(*) "; TIMES(info lexbuf) }
| _exp_ { printf "(**) "; EXP(info lexbuf) }
| _divide_ { printf "(/) "; DIVIDE(info lexbuf) }
(*| _floordivide_ { printf "(//) "; FLOOR_DIVIDE(info lexbuf) }*)
| _modulo_ { printf "(MOD) "; MODULO(info lexbuf) }
| _gt_ { printf "> "; GT(info lexbuf) }
| _ge_ { printf ">= "; GE(info lexbuf) }
| _lt_ { printf "< "; LT(info lexbuf) }
| _le_ { printf "<= "; LE(info lexbuf) }
| _lparen_ { printf "·(·"; LPAREN(info lexbuf) }
| _rparen_ { printf "·)·"; RPAREN(info lexbuf) }
| _lbracket_ { printf "·[·"; LBRACKET(info lexbuf) }
| _rbracket_ { printf "·]·"; RBRACKET(info lexbuf) }
| _comma_ { printf ", "; COMMA(info lexbuf) }
| _lbrace_ { printf "·{·"; LBRACE(info lexbuf) }
| _rbrace_ { printf "·}·"; RBRACE(info lexbuf) }
| _colon_ { printf ":: "; COLON(info lexbuf) }
| _endblock_ { printf "END "; END(info lexbuf) }
| _reactblock_ { printf "REACTBEGIN "; react lexbuf }
| _dot_ { printf "(dot)"; DOT(info lexbuf) }
| _and_ { printf "(&&) "; AND(info lexbuf) }
| _or_ { printf "(||) "; OR(info lexbuf) }
| _not_ { printf "!"; NOT(info lexbuf) }
| _in_ { printf "IN "; IN(info lexbuf) }
| _is_ { printf "IS "; IS(info lexbuf) }
| _if_ { printf "IF "; IF(info lexbuf) }
| _elif_ { printf "ELIF "; ELIF(info lexbuf) }
| _else_ { printf "ELSE "; ELSE(info lexbuf) }
| _while_ { printf "WHILE "; WHILE(info lexbuf) }
| _for_ { printf "FOR "; FOR(info lexbuf) }
| _break_ { printf "BREAK "; BREAK(info lexbuf) }
| _continue_ { printf "CONTINUE "; CONTINUE(info lexbuf) }
| _class_ { printf "CLASS "; CLASS(info lexbuf) }
| _def_ { printf "DEF "; DEF(info lexbuf) }
| _lambda_ { printf "LAMBDA "; LAMBDA(info lexbuf) }
| _return_ { printf "RETURN "; RETURN(info lexbuf) }
| _try_ { printf "TRY "; TRY(info lexbuf) }
| _except_ {printf "EXCEPT ";  EXCEPT(info lexbuf) }
| _raise_ { printf "RAISE "; RAISE(info lexbuf) }
| _delete_ { printf "DELETE "; DELETE(info lexbuf) }
| _import_ { printf "IMPORT "; IMPORT(info lexbuf) }
| _from_ { printf "FROM "; FROM(info lexbuf) }
| _as_ { printf "AS "; AS(info lexbuf) }
| _jconst_ { printf "CONST "; JCONST(info lexbuf) }
| _jlet_ { printf "LET "; JLET(info lexbuf) }
| _var_ as var { printf "var(%s) " var; VAR((info lexbuf), var)}
| eof { print_endline "\nEOF"; EOF }
| _string_ as str {printf "\"%s\" " str;  STRING((info lexbuf), str) }
| _ as c { error lexbuf (String.make 1 c) }

(*and parse_single_quote = parse
| _singlequote_ { printf "' "; SQUOTE; token lexbuf }
| _ as c { printf "%c" c; parse_single_quote lexbuf }

and parse_double_quote = parse
| _doublequote_ { printf "\" "; DQUOTE; token lexbuf }
| _ as c { printf "%c" c; parse_double_quote lexbuf }*)

and single_comment = parse
| _eol_ { print_endline " ##"; newline lexbuf; token lexbuf }
| eof  { exit 0 }
| _ as c { printf "%c" c; single_comment lexbuf }

and multi_comment = parse
| _eol_ { newline lexbuf; multi_comment lexbuf}
| _multilinecomment_ { print_endline "---'''"; token lexbuf }
| _ { multi_comment lexbuf }

and react = parse
| _reactblock_ { printf "\nREACTEND "; token lexbuf }
(*| _ as jsx { printf "%c" jsx; REACT_CHAR (jsx); react lexbuf }*)


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