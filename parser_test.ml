open OUnit2
open Ast
open Lexer
open Grammar
open Transform

let parse_file (filename:string) =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast =
    try Grammar.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  ast

let empty_list = []
let list_a = [1;2;3]


let test_fun _ =
  let list_b = List.append empty_list [1;2;3] in
  assert_equal list_b list_a

let test_var _ =
  let list_b = List.append empty_list [1;2;3] in
  assert_equal list_b list_a

let suite =
  "ParserTests" >::: [
    "test_fun" >:: test_fun;
    "test_var" >:: test_var
  ]

let () =
  run_test_tt_main suite