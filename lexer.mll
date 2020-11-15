(* header *)
{
  open Printf
  open Char
  open Lexing

  type token =
  | NEWLINE
  | EXTEND
  | SQUOTE
  | DQUOTE
  | NONE
  | BOOL of string
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

exception UnrecognizedStr of string
exception UnrecognizedChar
exception Eof
}

(* 

Definitions

 *)



(* whitespace *)
(* let _crlf_ = '\\r\\n' *)
let _newline_ = '\n'
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

let _string_ = '\"'(_[^'\"'])*'\"'

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
| _eol_+ { printf " (\\n)\n"; Lexing.new_line lexbuf; NEWLINE }
| _singlelinecomment_ { printf "#"; single_comment lexbuf }
| _multilinecomment_ { printf "'''---"; multi_comment lexbuf }
| [' ' '\t' '\n'] { print_endline "[ws]"; token lexbuf }
| _doublequote_ { printf "\""; DQUOTE; parse_double_quote lexbuf }
| _singlequote_ { printf "'"; SQUOTE; parse_single_quote lexbuf }
| _none_ { printf "NONE "; NONE }
| _true_ as b { printf "(TRUE) "; BOOL b}
| _false_ as b { printf "(FALSE) "; BOOL b}
| _string_ as str {printf "\"%s\" " str;  STRING str }
| _int_ as n { printf "int(%s) " n; INT (int_of_string n) }
| _float_ as n { printf "float(%s) " n; FLOAT (float_of_string n) }
| _equals_ { printf "(=) "; EQUALS }
| _plusequals_ { printf "(+=) "; PLUS_EQUALS }
| _minusequals_ { printf "(-=) "; MINUS_EQUALS }
| _timesequals_ { printf "(*=) "; TIMES_EQUALS }
| _divideequals_ { printf "(/=) "; DIVIDE_EQUALS }
| _moduloequals_ { printf "(MOD=) "; MODULO_EQUALS }
| _doubleequals_ { printf "(==) "; DOUBLE_EQUALS }
| _notequals_ { printf "(!=) "; NOT_EQUALS }
| _plus_ { printf "(+) "; PLUS }
| _minus_ { printf "(-) "; MINUS }
| _times_ { printf "(*) "; TIMES }
| _exp_ { printf "(**) "; EXP }
| _divide_ { printf "(/) "; DIVIDE }
| _floordivide_ { printf "(//) "; FLOOR_DIVIDE }
| _modulo_ { printf "(MOD) "; MODULO }
| _gt_ { printf "> "; GT }
| _ge_ { printf ">= "; GE }
| _lt_ { printf "< "; LT }
| _le_ { printf "<= "; LE }
| _lparen_ { printf "·(·"; LPAREN }
| _rparen_ { printf "·)·"; RPAREN }
| _lbracket_ { printf "·[·"; LBRACKET }
| _rbracket_ { printf "·]·"; RBRACKET }
| _comma_ { printf ", "; COMMA }
| _lbrace_ { printf "·{·"; LBRACE }
| _rbrace_ { printf "·}·"; RBRACE }
| _colon_ { printf ":: "; COLON }
| _endblock_ { printf "END "; END }
| _reactblock_ { printf "REACTBEGIN "; react lexbuf }
| _dot_ { printf "(dot)"; DOT }
| _and_ { printf "(&&) "; AND }
| _or_ { printf "(||) "; OR }
| _not_ { printf "!"; NOT }
| _in_ { printf "IN "; IN }
| _is_ { printf "IS "; IS }
| _if_ { printf "IF "; IF }
| _elif_ { printf "ELIF "; ELIF }
| _else_ { printf "ELSE "; ELSE }
| _while_ { printf "WHILE "; WHILE }
| _for_ { printf "FOR "; FOR }
| _break_ { printf "BREAK "; BREAK }
| _continue_ { printf "CONTINUE "; CONTINUE }
| _class_ { printf "CLASS "; CLASS }
| _def_ { printf "DEF "; DEF }
| _lambda_ { printf "LAMBDA "; LAMBDA }
| _return_ { printf "RETURN "; RETURN }
| _try_ { printf "TRY "; TRY }
| _except_ {printf "EXCEPT ";  EXCEPT }
| _raise_ { printf "RAISE "; RAISE }
| _delete_ { printf "DELETE "; DELETE }
| _import_ { printf "IMPORT "; IMPORT }
| _from_ { printf "FROM "; FROM }
| _as_ { printf "AS "; AS }
| _jconst_ { printf "CONST "; JCONST }
| _jlet_ { printf "LET "; JLET }
| _var_ as var { printf "var(%s) " var; VAR var}
| eof { raise End_of_file }
| _ as c {
  let pos = lexbuf.Lexing.lex_curr_p in
  printf "Error @ line %d\n" pos.Lexing.pos_lnum;
  printf "Unrecognized character: %c\n" c;
  raise UnrecognizedChar
  }

and parse_single_quote = parse
| _singlequote_ { printf "' "; SQUOTE; token lexbuf }
| _ as c { printf "%c" c; parse_single_quote lexbuf }

and parse_double_quote = parse
| _doublequote_ { printf "\" "; DQUOTE; token lexbuf }
| _ as c { printf "%c" c; parse_double_quote lexbuf }

and single_comment = parse
| _eol_ { print_endline " ##"; token lexbuf }
| eof  { raise End_of_file }
| _ as c { printf "%c" c; single_comment lexbuf }

and multi_comment = parse
| _multilinecomment_ { print_endline "---'''"; token lexbuf }
| _ { multi_comment lexbuf }

and react = parse
| _reactblock_ { printf "REACTEND "; token lexbuf }
| _ as jsx { printf "%c " jsx; REACT_CHAR (jsx) }


(* 

trailer

 *)

 {
  (* let rec parse lexbuf = parse lexbuf *)

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
 }