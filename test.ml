open OUnit2

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

let lexer_path n = "tests/lexer/lex0" ^ n ^ ".pyx"
let parser_path n = "tests/parser/parse" ^ n ^ ".pyx"
let transformer_path n = "tests/transformer/transform" ^ n ^ ".pyx"

let test_lexer n _ = ignore(string_of_int n |> lexer_path |> parse_program); print_endline "\n\n"

let suite = 
  "LexerTests" >::: [
    "lex00" >:: test_lexer 0;
    "lex01" >:: test_lexer 1;
    "lex02" >:: test_lexer 2;
    "lex03" >:: test_lexer 3;
    "lex04" >:: test_lexer 4;
    "lex05" >:: test_lexer 5;
  ]
 

let () =
  run_test_tt_main suite