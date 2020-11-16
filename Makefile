all:
	ocamllex lexer.mll && ocamlopt -o main lexer.ml

grammar:
	ocamlyacc grammar.mly
	ocamlopt -o grammar grammar.mli grammar.ml

ast:
	ocamlopt -o Ast ast.ml

clean:
	rm *.cmi *.cmx *.o main lexer.ml