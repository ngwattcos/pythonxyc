open Ast
open Lexer
open Lexer_verbose
open Grammar
open Transform
open Printf
open Print

let banner = "----------------------------------------------------------------------------\n"

let l = Array.length Sys.argv - 1
let num = Sys.argv.(1)
let opt = if 2 <= l then Sys.argv.(2) else "t"

let flag = match opt with
| "l" | "L" | "p" | "P" | "a" | "A" | "g" | "G" | "t" | "T" -> opt 
| _ -> "t"

let filename = match flag with 
| "l" | "L" -> "tests/lexer/lex" ^ num ^ ".pyx"
| "p" | "P" | "a" | "A" | "g" | "G" -> "tests/parser/parse" ^ num ^ ".pyx"
| "t" | "T" | _ -> "tests/translator/translate" ^ num ^ ".pyx"

let lexbuf = Lexing.from_channel (open_in filename)

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

(* print tranformed AST *)
let _ = 
  match flag with
  | "t" | "T" -> print_string banner; ast |> Transform.translate |> Buffer.contents |> print_endline
  | _ -> ()
