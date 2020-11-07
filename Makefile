all:
	ocamllex lexer.mll && ocamlopt -o main lexer.ml

clean:
	rm *.cmi *.cmx *.o main lexer.ml