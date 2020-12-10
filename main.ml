
open Ast
open Lexer
open Lexer_verbose
open Grammar
open Transform
open Printf
open Print

let banner = "----------------------------------------------------------------------------\n"

let l = Array.length Sys.argv - 1
let path = Sys.argv.(1)
let inDir = Sys.argv.(2)
let outDir = Sys.argv.(3)
let opt = if l > 4 then Sys.argv.(4) else "t"

let flag = match opt with
| "l" | "L" | "p" | "P" | "a" | "A" | "g" | "G" | "t" | "T" -> opt 
| _ -> "t"

let input_path = "../" ^ inDir ^ path
let output_path = "../" ^ outDir ^ path

let filename = match flag with 
| "l" | "L" -> "tests/lexer/lex" ^ path ^ ".pyx"
| "p" | "P" | "a" | "A" | "g" | "G" -> "tests/parser/parse" ^ path ^ ".pyx"
| "t" | "T" | _ -> "tests/translator/translate" ^ path ^ ".pyx"

let lexbuf = match Lexing.from_channel (open_in input_path)

(* enable print lexing *)
let token = match flag with 
| "l" | "L" -> Lexer_verbose.token
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
