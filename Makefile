all:
	ocamllex lexer.mll && ocamlopt -o main lexer.ml
	make lexer
	make ast
	make grammar

gm:
	ocamlyacc -v grammar2.mly && ocamlopt -o grammar2 grammar2.mli grammar2.ml

grammar:
	ocamlyacc grammar.mly
	ocamlopt -o grammar grammar.mli grammar.ml


lexer:
	ocamllex lexer.mll
	ocamlopt -o Lexer lexer.ml

ast:
	ocamlopt -o Ast ast.ml

clean:
	rm *.cmi *.cmx *.o main grammar

clean-grammar:
	rm grammar