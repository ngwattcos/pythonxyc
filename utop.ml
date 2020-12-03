let parse_program (filename:string) =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast =
    try Grammar.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  (ast)

let name (num:int) = 
  let numS = string_of_int num in
  "tests/parser/parse" ^ 
  (if num < 10 then "0" else "") ^ numS ^ ".pyx"

let pnum (num:int) = parse_program (name num)
