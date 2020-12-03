open Ast
open Lexer
open Grammar
open Transform

let () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let c =
    try Grammar.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  let transpiled = Buffer.contents (Transform.transform c) in
  print_endline transpiled