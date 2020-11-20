all:
	ocamllex lexer.mll && ocamlopt -o main lexer.ml

gm:
	ocamlyacc -v grammar2.mly && ocamlopt -o grammar2 grammar2.mli grammar2.ml

grammar:
	ocamlyacc grammar.mly
	ocamlopt -o grammar grammar.mli grammar.ml


grammar2:
	ocamlyacc grammar2.mly
	ocamlopt -o grammar2 grammar2.mli grammar2.ml

ast:
	ocamlopt -o Ast ast.ml

clean:
	rm *.cmi *.cmx *.o main lexer.ml grammar.ml grammar.mli grammar grammar2.ml grammar2.mli grammar2

clean-grammar2:
	rm grammar2.ml grammar2.mli grammar2