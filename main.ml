
open Ast
open Lexer
open Grammar
open Transform
open Printf
open Print

let banner = "----------------------------------------------------------------------------\n"

let l = Array.length Sys.argv - 1
let path = Sys.argv.(1)
let inDir = if l >= 2 then Sys.argv.(2) else ""
let outDir = if l >= 3 then Sys.argv.(3) else ""
let opt = if l >= 4 then Sys.argv.(4) else if l >= 2 then Sys.argv.(2) else "T"

let flag = match opt with
| "l" | "L" | "p" | "P" | "a" | "A" | "g" | "G" | "t" | "T" -> opt
| _ -> "t"

let input_path = inDir ^ path
let filename =
if l >= 3 then
String.sub path 0 ((String.length path) - 4)
else ""
let output_path = outDir ^ filename ^ ".jsx"

(* let _ =
print_endline (string_of_int l);
print_endline ("path: " ^ path);
print_endline ("filename: " ^ filename);
print_endline input_path;
print_endline output_path *)

(* let test_file = match flag with 
| "l" | "L" -> "tests/lexer/lex" ^ path
| "p" | "P" | "a" | "A" | "g" | "G" -> "tests/parser/parse" ^ path
| "t" | "T" | _ -> "tests/translator/translate" ^ path *)

let lexbuf = match flag with
| _ -> Lexing.from_channel (open_in input_path)
(* | "T" | _ -> Lexing.from_channel (open_in test_file) *)

(* enable print lexing *)
let token = match flag with 
| "l" | "L" -> Lexer.token
| _ -> Lexer.token

let ast =
  try Grammar.program token lexbuf
  with Parsing.Parse_error ->
    Printf.printf "Syntax error at line %d character %d\n"
    !Lexer.lineno
    (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
  exit 1

(* print AST *)
let _ = match flag with 
  | "p" | "P" | "a" | "A" | "g" | "G" -> print_string banner; print_verbose ast
  | _ -> ()

let write_output = output_string (open_out output_path)


(* print tranformed AST *)
let _ =
  match flag with
  | "T" -> print_string banner; ast |> Transform.translate |> Buffer.contents |> print_endline
  | "t" -> ast |> Transform.translate |> Buffer.contents |> write_output
  | _ -> ()
