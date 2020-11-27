all:
	make ast
	make grammar
	make lexer
	make transform
	make main

ml:
	ocamlopt -c ast.ml
	ocamlopt -c grammar.mli grammar.ml
	ocamlopt -c Lexer lexer.ml
	ocamlolt -c main main.ml

o:
	ocamlopt -o Ast ast.cmx
	ocamlopt -o Grammar ast.cmx grammar.cmx
	ocamlopt -o Lexer grammar.cmx lexer.cmx
	ocamlopt -o Transform ast.cmx transform.cmx
	ocamlopt -o main ast.cmx grammar.cmx lexer.cmx transform.cmx

grammar:
	ocamlyacc grammar.mly
	ocamlopt -o Grammar grammar.mli grammar.ml


lexer:
	ocamllex lexer.mll
	ocamlopt -o Lexer lexer.ml

ast:
	ocamlopt -o Ast ast.ml

clean:
	rm *.cmi *.cmx *.cmo *.o *.mli Grammar Lexer Transform Ast main grammar.ml lexer.ml
	rm parser_test

clean-grammar:
	rm grammar

transform:
	ocamlopt -o Transform transform.ml

main:
	ocamlopt -c main.ml
	ocamlopt -o main ast.cmx grammar.cmx lexer.cmx transform.cmx main.cmx
test:
	make ast
	make grammar
	make lexer
	make transform
	ocamlfind ocamlc -o parser_test -package oUnit -linkpkg -g ast.ml grammar.ml lexer.ml parser_test.ml